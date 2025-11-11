#' @title Tworzenie definicji podziału na skrypty zrównoleglające
#' @description
#' Funkcja służąca do wygenerowania ramki danych będącej zbiorem definicji
#' podziałów na grupy i ewentualnie grupy odniesienia dla danego zestawu
#' wskaźników zagregowanych.
#' @param p4 tabela pośrednia *p4*
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
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% select mutate distinct filter arrange
#' @seealso [utworz_grupowanie_odn_teryt()]
#' @return ramka danych będąca zbiorem definicji podziałów na agregaty
#' @export
definicje_podzialu <- function(p4, rodzaj_wsk, rok_ukonczenia = 2024) {
  rodzaje <- c("szk_god", "god1_god2", "szkozaw", "woj")
  
  stopifnot(
    is_tibble(p4) | is.data.frame(p4),
    nrow(p4) > 1,
    rodzaj_wsk %in% rodzaje,
    "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1
  )
  
  check_kolumny_tabele(p4, "p4", force = TRUE)
  
  stopifnot(
    length(unique(p4$rok_abs)) == 1,
    p4$rok_abs == rok_ukonczenia
  )
  
  if (rodzaj_wsk == "szk_god") {
    grupy = utworz_grupowanie_odn_teryt(
      x = p4,
      zmGrupujace = c(id_szk, rok_abs, typ_szk),
      zmPominGrupaOdniesienia = c(id_szk),
      idAbs = id_abs)
    
    return(grupy)
  } else if (rodzaj_wsk == "god1_god2") {
    p4$nazwa_zaw[is.na(p4$nazwa_zaw)] = "BRAK ZAWODU"
    
    grupy = p4 %>% 
      select(typ_szk, nazwa_zaw) %>%
      mutate(grupa = paste0("(typ_szk %in% \"", typ_szk, "\") & ",
                            "(nazwa_zaw %in% \"", nazwa_zaw, "\")"),
             odniesienie = paste0("typ_szk %in% \"", typ_szk, "\"")) %>% 
      distinct()
    
    return(grupy)
  } else if (rodzaj_wsk == "szkozaw") {
    p4 = p4 %>% 
      filter(typ_szk %in% c("Branżowa szkoła I stopnia",
                            "Branżowa szkoła II stopnia",
                            "Technikum",
                            "Szkoła policealna"))
    
    p4$nazwa_zaw = gsub("\u00A0", " ", p4$nazwa_zaw, fixed = TRUE)
    
    grupy = utworz_grupowanie_odn_teryt(
      x = p4,
      zmGrupujace = c(id_szk, nazwa_zaw, typ_szk))
    
    return(grupy)
  } else if (rodzaj_wsk == "woj") {
    grupy = p4 %>%
      select(typ_szk, teryt_woj) %>%
      mutate(grupa = paste0("teryt_woj %in% ", teryt_woj),
             odniesienie = paste0("(teryt_woj %in% ", teryt_woj, ") & ",
                                  "(typ_szk %in% \"", typ_szk, "\")")) %>% 
      arrange(typ_szk) %>% 
      distinct()
    
    return(grupy)
  }
}
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
#' @export
skrypty_podzial <- function(nrow, podzial = 11) {
  stopifnot(is.numeric(nrow), 
            nrow > 0, 
            floor(nrow) == nrow, 
            is.numeric(podzial), 
            podzial > 0, 
            floor(podzial) == podzial)
  
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
  rodzaje <- c("szk_god", "god1_god2", "szkozaw", "woj")
  
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
#' @param rodzaj_wsk ciąg znaków określający dla jakiego rodzaju wskaźników
#' należy zwórócić definicję podziału na grupy. Możliwe wartości to `"szk_god"`
#' lub `"szkozaw"`. Dla poozstałych wartości funkcja nic nie zwraca.
#' @return plik txt zawierający ścieżki do skryptów R liczących wskaźniki
#' zagregowane
#' @export
#' @seealso [skrypt_bat()]
#' @seealso [skrypt_wzor()]
plik_source <- function(sciezka_docelowa, rodzaj_wsk) {
  rodzaje <- c("szk_god", "god1_god2", "szkozaw", "woj")
  stopifnot(is.character(sciezka_docelowa) & length(sciezka_docelowa) > 0,
            rodzaj_wsk %in% rodzaje,
            "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1,
            dir.exists(sciezka_docelowa))
  
  if (!any(grepl("^skrypt_N", list.files(sciezka_docelowa)))) {
    stop(paste0("W podanej lokalizacji (", sciezka_docelowa, ") nie znaleziono plików ze skryptami, których nazwy zaczynają się od \"skrypt_N\""))
  }
  
  if (rodzaj_wsk %in% c("szk_god", "szkozaw")) {
    plik <- paste(gsub("/", "\\\\",
                 paste0(sciezka_docelowa,
                        list.files(sciezka_docelowa)[grep("^skrypt_N", list.files(sciezka_docelowa))])),
                 collapse = "\n", sep = "")
    writeLines(plik, paste0(sciezka_docelowa, "source_", rodzaj_wsk, ".txt"))
  } else {
    message(paste0("Dla wskaźników *", rodzaj_wsk, "* funkcja nic nie zwraca."))
  }
}
#' @title Tworzenie skryptów `.bat` uruchamiających zrównoleglanie obliczeń
#' @description
#' Funkcja zwraca skrypt `.bat`, który uruchamia tyle niezależnych sesji R, ile
#' wierszy zawiera plik "source" zawierający ścieżki do skryptów R służących
#' zrównolegleniu obliczeń.
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w ktorej ma być
#' zapisany plik source - jest to równocześnie ścieżka, w której powinny
#' znajdować się skrypty według podziału, który ma być zastosowany w
#' zrównoleglaniu
#' @param rodzaj_wsk ciąg znaków określający dla jakiego rodzaju wskaźników
#' należy zwórócić definicję podziału na grupy. Możliwe wartości to `"szk_god"`
#' lub `"szkozaw"`. Dla poozstałych wartości funkcja nic nie zwraca.
#' @return skrypt `.bat` uruchamiający skrypty w oddzielnych sesjach R
#' @export
skrypt_bat <- function(sciezka_docelowa, rodzaj_wsk) {
  rodzaje <- c("szk_god", "god1_god2", "szkozaw", "woj")
  stopifnot(is.character(sciezka_docelowa) & length(sciezka_docelowa) > 0,
            rodzaj_wsk %in% rodzaje,
            "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1,
            dir.exists(sciezka_docelowa))
  
  if (rodzaj_wsk %in% c("szk_god", "szkozaw")) {
    bat_script <- paste0(
      "@echo off",
      "\nsetlocal enabledelayedexpansion",
      "\n\n:: Path to the txt file",
      paste0("\nset file=", normal_windows_path(sciezka_docelowa), "source_", rodzaj_wsk, ".txt"),
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
    writeLines(bat_script, paste0(sciezka_docelowa, "generuj_", rodzaj_wsk,".bat"))
  } else {
    message(paste0("Dla wskaźników *", rodzaj_wsk, "* funkcja nic nie zwraca."))
  }
}
#' @title Łączenie częściowych zbiorów wskaźników zagregowanych
#' @description
#' Funkcja służy do scalenia częściowych zbiorów, które powstają podczas pracy
#' skryptów `.bat` (zwracanych przez funkcję [skrypt_bat()]). Zbiory są łączone,
#' a czasem dodawane są dodatkowe zmienne. 
#' @param sciezka_partial ścieżka w formacie tekstowym, w ktorej znajdują się
#' częściowe zbiory danych
#' @param sciezka_zapisu ścieżka do folderu w formacie tekstowym, w którym ma
#' być zapisany połączony zbiór
#' @param rodzaj_wsk wektor tekstowy określający dla jakiego rodzaju wskaźników
#' należy przeprowadzić łącznie zbiorów. Możliwe wartości to `"szk_god"` lub
#' `"szkozaw"`. Dla poozstałych wartości funkcja nie zwraca nic lub zwraca błąd
#' (jeżeli wartość jest spoza zbioru sensownych wartości).
#' @return zapis wskaźników zagregowanych
#' @export
zlacz_partial <- function(sciezka_partial, sciezka_zapisu, rodzaj_wsk) {
  rodzaje <- c("szk_god", "god1_god2", "szkozaw", "woj")
  stopifnot(is.character(sciezka_partial) & length(sciezka_partial) > 0,
            is.character(sciezka_zapisu) & length(sciezka_zapisu) > 0,
            rodzaj_wsk %in% rodzaje,
            "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1,
            dir.exists(sciezka_partial))
  
  sciezka_partial <- paste0(sciezka_partial, "partial/")
  
  if (!any(grepl("^wsk_.*\\.RData$", list.files(sciezka_partial)))) {
    stop(paste0("W podanej lokalizacji (", sciezka_partial, ") nie znaleziono zbiorów częściowych.\nSprawdź czy na pewno dla tego rodzaju wskaźników są one generowane."))
  }
  
  if (rodzaj_wsk == "szk_god") {
    zbiory <- list.files(path = sciezka_partial, pattern = "\\.RData$", full.names = TRUE)
    szk_lista <- list()
    god_lista <- list()
    
    for (i in 1:length(zbiory)) {
      env <- new.env()
      load(zbiory[[i]], envir = env)
      obiekty <- ls(env)
      szk_lista[[i]] <- get(obiekty[grepl("szk", obiekty)], envir = env)
      god_lista[[i]] <- get(obiekty[grepl("god", obiekty)], envir = env)
    }
    
    szk <- do.call(rbind, szk_lista)
    god <- do.call(rbind, god_lista)
    
    szk <- dodaj_odmiany(szk, "szk_god")
    
    sciezka_zapisu <- ifelse(grepl("/^", sciezka_zapisu),
                             paste0(sciezka_zapisu, "wskazniki_szk_god.RData"),
                             paste0(sciezka_zapisu, "/wskazniki_szk_god.RData"))
    save(szk, god, file = sciezka_zapisu)
  } else if (rodzaj_wsk == "szkozaw") {
    zbiory <- list.files(path = sciezka_partial, pattern = "\\.RData$", full.names = TRUE)
    szkozaw_lista <- list()
    
    for (i in 1:length(zbiory)) {
      env <- new.env()
      load(zbiory[[i]], envir = env)
      obiekty <- ls(env)
      szkozaw_lista[[i]] <- get(obiekty[grepl("szkozaw", obiekty)], envir = env)
    }
    
    szkozaw <- do.call(rbind, szkozaw_lista)
    
    sciezka_zapisu <- ifelse(grepl("/^", sciezka_zapisu),
                             paste0(sciezka_zapisu, "wskazniki_szkozaw.RData"),
                             paste0(sciezka_zapisu, "/wskazniki_szkozaw.RData"))
    save(szkozaw, file = sciezka_zapisu)
  } else {
    message(paste0("Dla wskaźników *", rodzaj_wsk, "* funkcja nic nie zwraca."))
  }
}
#' @title Dodawanie do zbioru zmiennej zawierającej odmiany np. rzeczowników
#' @description
#' Funkcja używana w ramach funkcji [zlacz_partial()]. Na podstawie zmiennych ze
#' zbioru wskaźników zagregowanch `szk` lub `woj` określa odmianę rzeczowników,
#' które następnie używane są w raportach szkolnych.
#' @param wskazniki zbiór wskaźników zagregowanch (`szk` lub `woj`)
#' @param rodzaj_wsk określa dla jakiego zbioru wyliczyć odmiany
#' @importFrom dplyr group_by reframe case_when row_number left_join
#' @importFrom tidyr nest
#' @return zbiór wskaźników zagregowanch z dołączoną zmienną `odmiany`
#' @export
dodaj_odmiany <- function(wskazniki, rodzaj_wsk) {
  stopifnot(
    is.data.frame(wskazniki),
    is.character(rodzaj_wsk),
    rodzaj_wsk %in% c("szk_god", "woj")
  )
  
  if (rodzaj_wsk == "szk_god") {
    wymagane_zmienne <- c("id_szk", "l_abs", "l_kobiet",  "l_abs_zrodla")
    stopifnot(
      all(wymagane_zmienne %in% names(wskazniki))
    )
    
    odmiany <- wskazniki %>% 
      group_by(id_szk) %>%
      reframe(
        abs = ifelse(l_abs %in% 1, " absolwenta", " absolwentów"),
        kob = if (l_kobiet %in% c(0:19)) {
          case_when(
            l_kobiet %in% 0 ~ "kobiet",
            l_kobiet %in% 1 ~ "kobietę",
            l_kobiet %in% c(2:4) ~ "kobiety",
            l_kobiet %in% c(5:19) ~ "kobiet")
        } else {
          case_when(
            (l_kobiet %% 10) %in% c(0, 1, 5:9) ~ "kobiet",
            (l_kobiet %% 10) %in% c(2:4) ~ "kobiety")
        },
        abs_polon = ifelse(l_abs_zrodla[[row_number()]]$n_polon %in% 1, " absolwenta", " absolwentów"),
        abs_zus = ifelse(l_abs_zrodla[[row_number()]]$n_zus %in% 1, " absolwenta", " absolwentów"),
        osob_zus = if (l_abs_zrodla[[row_number()]]$n_zus %in% c(0:19)) {
          case_when(
            l_abs_zrodla[[row_number()]]$n_zus %in% 0 ~ " osób",
            l_abs_zrodla[[row_number()]]$n_zus %in% 1 ~ " osoba",
            l_abs_zrodla[[row_number()]]$n_zus %in% c(2:4) ~ " osoby",
            l_abs_zrodla[[row_number()]]$n_zus %in% c(5:19) ~ " osób")
        } else {
          case_when(
            (l_abs_zrodla[[row_number()]]$n_zus %% 10) %in% c(0, 1, 5:9) ~ " osoby",
            (l_abs_zrodla[[row_number()]]$n_zus %% 10) %in% c(2:4) ~ " osób")
        }
      )
    
    odmiany <- odmiany %>% 
      nest(.by = id_szk, .key = "odmiany")
    
    wskazniki <- wskazniki %>% 
      left_join(odmiany)
    
    return(wskazniki)
  } else if (rodzaj_wsk == "woj") {
    wymagane_zmienne <- "teryt_woj_szk"
    stopifnot(
      all(wymagane_zmienne %in% names(wskazniki))
    )
    
    odmiany <- wskazniki %>% 
      select(teryt_woj_szk) %>% 
      mutate(
        nazwa = case_when(
          teryt_woj_szk %in% 2 ~ "dolnośląskie",
          teryt_woj_szk %in% 4 ~ "kujawsko-pomorskie",
          teryt_woj_szk %in% 6 ~ "lubelskie",
          teryt_woj_szk %in% 8 ~ "lubuskie",
          teryt_woj_szk %in% 10 ~ "łódzkie",
          teryt_woj_szk %in% 12 ~ "małopolskie",
          teryt_woj_szk %in% 14 ~ "mazowieckie",
          teryt_woj_szk %in% 16 ~ "opolskie",
          teryt_woj_szk %in% 18 ~ "podkarpackie",
          teryt_woj_szk %in% 20 ~ "podlaskie",
          teryt_woj_szk %in% 22 ~ "pomorskie",
          teryt_woj_szk %in% 24 ~ "śląskie",
          teryt_woj_szk %in% 26 ~ "świętokrzyskie",
          teryt_woj_szk %in% 28 ~ "warmińsko-mazurskie",
          teryt_woj_szk %in% 30 ~ "wielkopolskie",
          teryt_woj_szk %in% 32 ~ "zachodniopomorskie")
      ) %>% 
      mutate(
        nazwa_dop = paste0(nazwa, "go"))
    
    odmiany <- odmiany %>% 
      nest(.by = teryt_woj_szk, .key = "odmiany")
    
    wskazniki <- wskazniki %>% 
      left_join(odmiany, join_by(teryt_woj_szk))
    
    return(wskazniki)
  }
}
