#' @title Zmiana ukośników w ścieżce na Windowsowe
#' @description
#' Funkcja przyjmuje ścieżkę pliku jako ciąg znaków i zamienia wszystkie ukośniki
#' z formatu Unixowego (`/`) na format Windowsowy (`\\`).#'
#' @param path Ścieżka pliku jako ciąg znaków (string). Musi być niepusty.
#' @return Zwraca tekst z poprawionymi ukośnikami, gotowy do użycia w Windows.
#' @examples
#' normal_windows_path("folder/podfolder/plik.txt")
#' # [1] "folder\\podfolder\\plik.txt"
#' @export
normal_windows_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("Argument 'path' musi być niepustym ciągiem znaków.")
  }
  gsub("/", "\\\\", path)
}
#' @title Generowanie skryptów służących do obliczania wskaźników
#' @description
#' Funkcja ta zapisuje w zadanej lokalizacji zadany typ skryptu dla danego typu
#' szkoły.
#' @param sciezka_wd ścieżka do katalogu roboczego w formacie tekstowym
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @importFrom tibble is_tibble
#' @return skrypty .R do uruchomienia podczas zrównoleglania
#' @export
skrypt_wzor <- function(sciezka_wd,
                        rok_ukonczenia = 2025) {
  stopifnot(
    is.character(sciezka_wd) & length(sciezka_wd) > 0,
    is.numeric(rok_ukonczenia)
  )
  
  typy_szk <- list(
    "bs1" = "Branżowa szkoła I stopnia",
    "tech" = "Technikum",
    "spolic" = "Szkoła policealna",
    "lo" = "Liceum ogólnokształcące",
    "lodd" = "Liceum dla dorosłych",
    "bs2" = "Branżowa szkoła II stopnia"
  )
  
  stala <- paste(
    "czas_start <- Sys.time()",
    "cat(\"Start: \", format(czas_start, \"%Y.%m.%d %H:%M:%S\"), sep = \"\")",
    "# biblioteki",
    "library(dplyr)",
    "library(ggplot2)",
    "library(kableExtra)",
    "library(tidyr)",
    "library(MONIKAraporty)",
    "# ścieżki",
    "source(\"R/helpers.R\")",
    "parametry <- parametry()",
    "# dane",
    "cat(\"Ładowanie danych: \", format(Sys.time(), \"%Y.%m.%d %H:%M:%S\"), sep = \"\")",
    "load(paste0(parametry$project_path, \"data/02_wskazniki_zagregowane/wskazniki_szk_god_final.RData\"))",
    "load(paste0(parametry$project_path, \"data/02_wskazniki_zagregowane/wskazniki_god1_god2.RData\"))",
    "load(paste0(parametry$project_path, \"data/02_wskazniki_zagregowane/wskazniki_szkozaw.RData\"))",
    "load(paste0(parametry$project_path, \"data/02_wskazniki_zagregowane/wskazniki_woj.RData\"))\n",
    sep = "\n"
  )
  
  for (szkola in names(typy_szk)) {
    nazwa_pliku = paste0(sciezka_wd, "scripts/03_raporty/skrypt_", szkola, ".R")
    cat(
      "# --- ", szkola, " ---\n",
      "cat(\"Generowanie raportów dla szkoły typu: ", typy_szk[[szkola]], "\\n\")",
      stala,
      
      szkola, "_szk <- szk %>% filter(typ_szk %in% \"", typy_szk[[szkola]], "\")\n",
      szkola, "_god <- god %>% filter(id_szk %in% ", szkola, "_szk$id_szk)\n",
      "cat(\"Generowanie raportów: \", format(Sys.time(), \"%Y.%m.%d %H:%M:%S\"), sep = \"\")\n",
      "setwd(paste0(parametry$project_path, \"raporty/final/", szkola, "/\"))\n",
      "generuj_raporty(\n",
      "  pakiet = \"MONIKAraporty\",\n",
      "  szablon = \"adm_master_1rokpo.Rmd\",\n",
      "  wskazniki = ", szkola, "_szk,\n",
      "  wskaznikiGrPor = ", szkola, "_god,\n",
      "  kolumnaNazwaPliku = nazwa_pliku,\n",
      "  parametry = list(plikZObiektami = \"\",\n",
      "                   obiektWskazniki = \"", szkola, "_szk\",\n",
      "                   obiektWskaznikiPorownanie = \"", szkola, "_god\",\n",
      "                   obiektWskaznikiKraj1 = \"god1\",\n",
      "                   obiektWskaznikiKraj2 = \"god2\",\n",
      "                   obiektSzkoloZawody = \"szkozaw\",\n",
      "                   obiektWoj = \"woj\",\n",
      "                   progLiczebnosci = 10,\n",
      "                   typDokumentu = \"pdf\",\n",
      "                   wyrownanieTabWykr = \"center\",\n",
      "                   rok = ", rok_ukonczenia, "))\n",
      "czas_stop = Sys.time()\n",
      "czas_roznica = round(czas_stop - czas_start, 2)\n",
      "cat(\"Koniec: \", format(czas_stop, \"%Y.%m.%d %H:%M:%S\"), \"\\nComputation duration: \", paste(as.numeric(czas_roznica), attr(czas_roznica, \"units\")), \"\\n\", sep = \"\")\n",
      sep = "",
      file = nazwa_pliku
    )
  }
}
#' @title Funkcja generująca plik ze ścieżkami do skryptów
#' @description
#' Funkcja zwraca plik txt będący spisem ścieżek do poszczególnych skryptów,
#' które tworzone są za pomocą funkcji [skrypt_wzor()] - mogą również być
#' stworzone w inny sposób, ale ważne żeby ich nazewnictwo i struktura były
#' zgodne z tymi, które zwraca wspomniana funkcja. 
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w której ma być
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
    "\n    start \"\" \"C:\\PROGRA~1\\R\\R-4.6.1\\bin\\x64\\Rscript.exe\" !line!",
    "\n)",
    "\n\nendlocal",
    "\npause"
  )
  writeLines(bat_script, paste0(sciezka_docelowa, "generuj_raporty.bat"))
}
