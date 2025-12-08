#' @title Funkcja tworząca podział na skrypty
#' @details Funkcja zwraca ramkę danych będącą definicją podziału na zbliżoną
#' liczbę wierszy w amce danych z definicją podziału na grupy. Podział ten jest
#' potrzebny do zrównoleglenia obliczeń - do stworzenia poszczególnych skryptów
#' będących częścią infrastruktury zrównoleglającej obliczenia.
#' @param nrow liczba wierszy w ramce danych wygenerowanej za pomocą funkcji
#' [utworz_grupowanie_odn_teryt()]
#' @param podzial liczba opisująca na ile części ma zostać podzielona liczba
#' wierszy (tyle będzie finalnie działających sesji R podczas zrównoleglania)
#' @return ramka danych zawierająca zakres wierszy dla każdej jednostki podziału
#' @importFrom
#' @export
skrypty_podzial <- function(wsk_szk) {
  stopifnot(
    is_tibble(wsk_szk) | is.data.frame(wsk_szk)
  )
  
  row_per_part <- nrow %/% podzial
  reszta <- nrow %% podzial
  
  skrypt <- integer(podzial)
  od <- integer(podzial)
  do <- integer(podzial)
  start <- 1
  
  for (i in 1:podzial) {
    current_part_rows <- row_per_part + ifelse(i <= reszta, 1, 0)
    
    skrypt[i] <- i
    od[i] <- start
    do[i] <- start + current_part_rows - 1
    start <- start + current_part_rows
  }
  
  stopifnot("Liczba wierszy powinna być równa liczbie w pierwszym argumencie" = sum(do - od + 1) == nrow)
  stopifnot("Zakresy 'd' i 'do' nie powinny nachodzić na siebie" = all(od[-1] == do[-length(do)] + 1))
  
  df <- data.frame(skrypt = skrypt,
                  od = od,
                  do = do)
  
  stopifnot("Zwracana ramka danych powinna mieć dokładnie tyle wierszy ile w argumencie 'podzial'" = nrow(df) == podzial)
  
  return(df)
}
#' @title Generowanie skryptów służących do obliczania wskaźników
#' @description
#' Funkcja ta zapisuje w zadanej lokalizacji zadany typ skryptu w liczbie
#' ustalonej w argumencie `podzial` funkcji [skrypty_podzial()].
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w ktorej mają być
#' zapisane wygenerowane skrypty
#' @param plik_tab_posrednie ścieżka do pliku w formacie tekstowym, w ktorej
#' znajdują się tabele pośrednie
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @param rodzaj_wsk ciąg znaków określający dla jakiego rodzaju wskaźników
#' należy zwórócić definicję podziału na grupy. Możliwe wartości:
#' \describe{
#'   \item{`"szk_god"`}{Główny podział na szkołę oraz jej grupę odniesienia z
#'   wykluczeniem}
#'   \item{`"god1_god2"`}{}
#'   \item{`"szkozaw"`}{Zbiór, gdzie jednostką podziału jest zawód nauczany (i
#'   posiadający absolwentów) w danej szkol - szkoło-zawod.}
#'   \item{`"woj"`}{}
#' }
#' @param podzial_grupy_df ramka danych wygenerowana zwykle za pomocą funkcji
#' [definicje_podzialu()]
#' @importFrom tibble is_tibble
#' @return pliki txt będące skryptami uruchamianymi podczas zrównoleglania
#' @seealso [definicje_podzialu()]
#' @seealso [przygotuj_tabele_posrednie()]
#' @export
skrypt_wzor <- function(sciezka_docelowa, plik_tab_posrednie,
                        rok_ukonczenia = 2024, rodzaj_wsk,
                        podzial_grupy_df) {
  
  stopifnot(is.character(sciezka_docelowa) & length(sciezka_docelowa) > 0,
            is.character(plik_tab_posrednie) & length(plik_tab_posrednie) > 0,
            rodzaj_wsk %in% rodzaje,
            "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1,
            is.numeric(rok_ukonczenia),
            is_tibble(podzial_grupy_df) | is.data.frame(podzial_grupy_df),
            nrow(podzial_grupy_df) > 1)
  
  if (rodzaj_wsk == "szk_god") {
    stale <- paste(
      "# biblioteki",
      "library(dplyr)",
      "library(MONIKAdane)",
      "library(beepr)",
      "# ładownaie danych",
      paste0("load(\"", plik_tab_posrednie, "\")"),
      paste0("load(\"", sciezka_docelowa, "grupy_szk_god.RData\")"),
      "grupy1 <- grupy_szk[maska,]",
      "# liczenie wskaźników",
      paste0("wsk <- agreguj_1rokpo_adm(wsk2 = p2, wsk3 = p3, wsk4 = p4, podzial_grupy = grupy1, rok_abso = ", rok_ukonczenia, ")"),
      "szk <- wsk$grupy",
      "god <- wsk$grupyOdniesienia",
      sep = "\n"
    )
    
    for (i in 1:nrow(podzial_grupy_df)) {
      nazwa_pliku = paste0(sciezka_docelowa, "skrypt_N", podzial_grupy_df$skrypt[i], ".R")
      cat(
        "# parametry\nmaska <- ",
        podzial_grupy_df$od[i],
        ":",
        podzial_grupy_df$do[i],
        "\nprefiks <- \"N",
        podzial_grupy_df$skrypt[i],
        "\"\n",
        stale,
        "\n",
        "# zapisywanie wskaźników\n",
        paste0("plik <- paste0(\"", sciezka_docelowa, "partial/wsk_szk_god_\", prefiks, \".RData\")\n"),
        "save(szk, god, file = plik)\n",
        "beep(5)",
        sep = "",
        file = nazwa_pliku
      )
    }
  } else if (rodzaj_wsk == "szkozaw") {
    stale <- paste(
      "# biblioteki",
      "library(dplyr)",
      "library(MONIKAdane)",
      "library(beepr)",
      "# ładownaie danych",
      paste0("load(\"", plik_tab_posrednie, "\")"),
      paste0("load(\"", sciezka_docelowa, "grupy_szkozaw.RData\")"),
      "grupy1 <- grupy_szkozaw[maska,]",
      "# liczenie wskaźników",
      paste0("wsk <- agreguj_szkozaw_1rokpo_adm(wsk3 = p3, wsk4 = p4, podzial_grupy = grupy1, rok_abso = ", rok_ukonczenia, ")"),
      "szkozaw <- wsk$grupyOdniesienia",
      sep = "\n"
    )
    
    for (i in 1:nrow(podzial_grupy_df)) {
      nazwa_pliku = paste0(sciezka_docelowa, "skrypt_N", podzial_grupy_df$skrypt[i], ".R")
      cat(
        "# parametry\nmaska <- ",
        podzial_grupy_df$od[i],
        ":",
        podzial_grupy_df$do[i],
        "\nprefiks <- \"N",
        podzial_grupy_df$skrypt[i],
        "\"\n",
        stale,
        "\n",
        "# zapisywanie wskaźników\n",
        paste0("plik <- paste0(\"", sciezka_docelowa, "partial/wsk_szkozaw_\", prefiks, \".RData\")\n"),
        "save(szkozaw, file = plik)\n",
        "beep(5)",
        sep = "",
        file = nazwa_pliku
      )
    }
  } else if (rodzaj_wsk %in% c("god1_god2", "woj")) {
    message(paste0("Wskaźniki *", rodzaj_wsk, "* nie są zwykle obliczane równolegle, więc nie przewidziadno dla nich tworzenia infrastruktury dla zrównoleglania. Należy je policzyć \"ręcznie\"."))
  }
}
#' @title Funkcja generująca plik ze ścieżkami do skryptów
#' @description
#' Funkcja zwraca plik txt będący spisem ścieżek do poszczególnych skryptów,
#' które tworzone są za pomocą funkcji [skrypt_wzor()] - mogą również być
#' stworzone w inny sposób, ale ważne żeby ich nazewnictwo i struktura były
#' zgodne z tymi, które zwraca wspomniana funkcja. 
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w ktorej ma być
#' zapisany plik source - jest to równocześnie ścieżka, w której powinny
#' znajdować się skrypty według podziału, który ma być zastosowany w
#' zrównoleglaniu
#' @return plik txt zawierający ścieżki do skryptów R liczących wskaźniki
#' zagregowane
#' @export
#' @seealso [skrypt_bat()]
#' @seealso [skrypt_wzor()]
plik_source <- function(sciezka_docelowa) {
  stopifnot(
    is.character(sciezka_docelowa) & length(sciezka_docelowa) > 0,
    dir.exists(sciezka_docelowa)
  )
  
  if (!any(grepl("^skrypt_", list.files(sciezka_docelowa)))) {
    stop(paste0("W podanej lokalizacji (", sciezka_docelowa, ") nie znaleziono plików ze skryptami, których nazwy zaczynają się od \"skrypt_\""))
  }
  
  plik <- paste(
    gsub("/", "\\\\", paste0(sciezka_docelowa, list.files(sciezka_docelowa)[grep("^skrypt_", list.files(sciezka_docelowa))])),
    collapse = "\n", sep = ""
  )
  
  writeLines(plik, paste0(sciezka_docelowa, "source_raporty.txt"))
}
#' @title Tworzenie skryptów `.bat` uruchamiających zrównoleglanie obliczeń
#' @description
#' Funkcja zwraca skrypt `.bat`, który uruchamia tyle niezależnych sesji R, ile
#' wierszy zawiera plik "source" zawierający ścieżki do skryptów R służących
#' zrównolegleniu generowania raportów.
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w ktorej ma być
#' zapisany plik source - jest to równocześnie ścieżka, w której powinny
#' znajdować się skrypty według podziału, który ma być zastosowany w
#' zrównoleglaniu
#' @return skrypt `.bat` uruchamiający skrypty w oddzielnych sesjach R
#' @export
skrypt_bat <- function(sciezka_docelowa) {
  stopifnot(
    is.character(sciezka_docelowa) & length(sciezka_docelowa) > 0,
    dir.exists(sciezka_docelowa)
    )
  
  bat_script <- paste0(
    "@echo off",
    "\nsetlocal enabledelayedexpansion",
    "\n\n:: Path to the txt file",
    paste0("\nset file=", normal_windows_path(sciezka_docelowa), "source_raporty.txt"),
    "\n\n:: Read the file line by line",
    "\nfor /f \"usebackq delims=\" %%A in (\"%file%\") do (",
    "\n    :: Remove quotes from the line",
    "\n    set line=%%A",
    "\n\n    :: Run the R script using Rscript.exe simultaneously",
    "\n    start \"\" \"C:\\PROGRA~1\\R\\R-4.4.1\\bin\\x64\\Rscript.exe\" !line!",
    "\n)",
    "\n\nendlocal",
    "\npause"
  )
  writeLines(bat_script, paste0(sciezka_docelowa, "generuj_raporty.bat"))
}
