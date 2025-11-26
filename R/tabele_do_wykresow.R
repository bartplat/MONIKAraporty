#' @title Konwersja wskaźników w formie list na tabele do wsadu do wykresu
#' @description Na potrzeby raportu funkcja tworzy z 3 list zawierających
#' wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
#' ramkę danych, która następnie jest używana jako wsad do funkcji rysujących
#' wyresy.
#' @param dane_szk lista zawierająca wskaźniki na poziomie agregacji: szkoła
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_god lista zawierająca wskaźniki na poziomie agregacji: grupa
#' odniesienia (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_kraj lista zawierająca wskaźniki na poziomie agregacji: cały kraj
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param nazwa_god nazwa powiatu, podregionu lub województwa
#' @param etykiety lista etykiet wierszy tabeli, która ma być użyta potem jako
#' etykiety wartości na wykresie. Nazwy etykiet nie powinny zawierać nazwy
#' kolumny "n".
#' @param typ_szk typ szkoły, której dotyczy raport
#' @param szer szerokość tekstu - argument w funkcji \link{\code{wrapper}}
#' @export
#' @return ramka danych w formacie tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select everything mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_sentence
tab_wykres = function(dane_szk, dane_god, dane_kraj, nazwa_god, etykiety, typ_szk, szer = 80) {
  stopifnot(is.list(dane_szk) | is.null(dane_szk),
            is.list(dane_god) | is.null(dane_god),
            is.list(dane_kraj) | is.null(dane_kraj),
            is.character(typ_szk),
            is.numeric(szer),
            szer > 0)
  if (!is.null(nazwa_god)) {
    stopifnot(is.character(nazwa_god),
              length(nazwa_god) %in% 1,
              nchar(nazwa_god) > 1)
  }
  
  szkola = case_when(
    typ_szk %in% "Branżowa szkoła I stopnia" ~ "branżowe szkoły I stopnia",
    typ_szk %in% "Branżowa szkoła II stopnia" ~ "branżowe szkoły II stopnia",
    typ_szk %in% "Technikum" ~ "technika",
    typ_szk %in% "Szkoła policealna" ~ "szkoły policealne",
    typ_szk %in% "Liceum ogólnokształcące" ~ "licea",
    typ_szk %in% "Szkoła specjalna przysposabiająca do pracy" ~ "szkoły specjalne przysposabiające",
    typ_szk %in% "Liceum dla dorosłych" ~ "licea dla dorosłych")
  
  typ_god = ifelse(!is.null(nazwa_god),
                   paste0("Pozostałe ", szkola, "\nz ", nazwa_god),
                   "<GRUPA>")

  if (!is.null(dane_szk)) {
    tab_szk = dane_szk %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             typ = "Państwa szkoła")
    
    tab_szk$name = etykiety
  } else {
    tab_szk = NULL
  }

  if (!is.null(dane_god)) {
    tab_god = dane_god %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             typ = typ_god)
    
    tab_god$name = etykiety
  } else {
    tab_god = NULL
  }

  if (!is.null(dane_kraj)) {
    tab_kraj = dane_kraj %>%
      as_tibble() %>%
      select(-n) %>%
      pivot_longer(everything()) %>%
      mutate(lab = ifelse(round(value, 2) >= 0.06,
                          paste0(round(value * 100), "%"),
                          ""),
             # typ = ifelse(typ_szk %in% "Branżowa szkoła I stopnia",
             #              "Branżowe szkoły I stopnia\nw całej Polsce",
             #              paste0(str_to_sentence(szkola), "\nw całej Polsce")),
             typ = case_when(
               typ_szk %in% "Branżowa szkoła I stopnia" ~ "Branżowe szkoły I stopnia\nw całej Polsce",
               typ_szk %in% "Branżowa szkoła II stopnia" ~ "Branżowe szkoły II stopnia\nw całej Polsce",
               TRUE ~ paste0(str_to_sentence(szkola), "\nw całej Polsce")))
    
    tab_kraj$name = etykiety
  } else {
    tab_kraj = NULL
  }
  
  kolejnosc = ordered(c(ifelse(!is.null(tab_szk$typ), unique(tab_szk$typ), "1"),
                        ifelse(!is.null(tab_god$typ), unique(tab_god$typ), "2"),
                        ifelse(!is.null(tab_kraj$typ), unique(tab_kraj$typ), "3")),
                      levels = c(ifelse(!is.null(tab_szk$typ), unique(tab_szk$typ), "1"),
                                 ifelse(!is.null(tab_god$typ), unique(tab_god$typ), "2"),
                                 ifelse(!is.null(tab_kraj$typ), unique(tab_kraj$typ), "3")))
  
  tab = rbind(tab_szk, tab_god, tab_kraj)
  tab$typ = factor(tab$typ, levels = kolejnosc)
  return(tab)
}
#' @title Konwersja wskaźników w formie list na tabele do wsadu do wykresu
#' @description Na potrzeby raportu funkcja tworzy z 3 list zawierających
#' wskaźniki na różnych poziomach agregacji: szkoła, województwo i cały kraj,
#' ramkę danych, która następnie jest używana jako wsad do funkcji rysującej
#' wyres \code{wyk_facet()}.
#' @param dane_szk lista zawierająca wskaźniki na poziomie agregacji: szkoła
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_god lista zawierająca wskaźniki na poziomie agregacji: grupa
#' odniesienia (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param dane_kraj lista zawierająca wskaźniki na poziomie agregacji: cały kraj
#' (lub wartość NULL jeśli ten poziom agregacji nie jest wymagany)
#' @param god nazwa poziomu podziału terytorialnego, w którym znajduje
#' się dana szkoła w formie dopełniacza (lub wartość NULL jeśli nie jest
#' wymagana)
#' @param nazwa_god nazwa poiatu, podregionu lub województwa
#' @param typ_szk typ szkoły, której dotyczy raport
#' @param szer szerokość tekstu - argument w funkcji \link{\code{wrapper}}
#' @export
#' @return ramka danych w formacie tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when select starts_with mutate arrange
#' @importFrom stringr str_to_sentence
tab_facet <- function(dane_szk, dane_god, dane_kraj, nazwa_god, typ_szk, szer = 80) {
  stopifnot(is.list(dane_szk) | is.null(dane_szk),
            is.list(dane_god) | is.null(dane_god),
            is.list(dane_kraj) | is.null(dane_kraj),
            is.character(typ_szk),
            is.numeric(szer),
            szer > 0)
  if (!is.null(nazwa_god)) {
    stopifnot(is.character(nazwa_god),
              length(nazwa_god) %in% 1,
              nchar(nazwa_god) > 1)
  }
  
  szkola = case_when(
    typ_szk %in% "Branżowa szkoła I stopnia" ~ "branżowe szkoły I stopnia",
    typ_szk %in% "Branżowa szkoła II stopnia" ~ "branżowe szkoły II stopnia",
    typ_szk %in% "Technikum" ~ "technika",
    typ_szk %in% "Szkoła policealna" ~ "szkoły policealne",
    typ_szk %in% "Liceum ogólnokształcące" ~ "licea",
    typ_szk %in% "Szkoła specjalna przysposabiająca do pracy" ~ "szkoły specjalne przysposabiające",
    typ_szk %in% "Liceum dla dorosłych" ~ "licea dla dorosłych")
  
  typ_god = ifelse(!is.null(nazwa_god),
                   paste0("Pozostałe ", szkola, "\nz ", nazwa_god),
                   "<GRUPA>")

  if (!is.null(dane_szk)) {
    tab_szk = dane_szk %>%
      as_tibble() %>%
      select(mies = bezrobocie_r0_wrzgru, pct) %>%
      mutate(typ = "Państwa szkoła") %>%
      arrange(mies) %>% 
      mutate(lab = paste0(round(pct * 100), "%"))
  } else {
    tab_szk = NULL
  }
  
  if (!is.null(dane_god)) {
    tab_god = dane_god %>%
      as_tibble() %>%
      select(mies = bezrobocie_r0_wrzgru, pct) %>%
      mutate(typ = typ_god) %>%
      arrange(mies) %>% 
      mutate(lab = paste0(round(pct * 100), "%"))
  } else {
    tab_god = NULL
  }
  
  if (!is.null(dane_kraj)) {
    tab_kraj = dane_kraj %>%
      as_tibble() %>%
      select(mies = bezrobocie_r0_wrzgru, pct) %>%
      mutate(typ = case_when(
        typ_szk %in% "Branżowa szkoła I stopnia" ~ "Branżowe szkoły I stopnia\nw całej Polsce",
        typ_szk %in% "Branżowa szkoła II stopnia" ~ "Branżowe szkoły II stopnia\nw całej Polsce",
        TRUE ~ paste0(str_to_sentence(szkola), "\nw całej Polsce"))) %>%
      arrange(mies) %>% 
      mutate(lab = paste0(round(pct * 100), "%"))
    
  } else {
    tab_kraj = NULL
  }
  
  kolejnosc = ordered(c(ifelse(!is.null(tab_szk$typ), unique(tab_szk$typ), "1"),
                        ifelse(!is.null(tab_god$typ), unique(tab_god$typ), "2"),
                        ifelse(!is.null(tab_kraj$typ), unique(tab_kraj$typ), "3")),
                      levels = c(ifelse(!is.null(tab_szk$typ), unique(tab_szk$typ), "1"),
                                 ifelse(!is.null(tab_god$typ), unique(tab_god$typ), "2"),
                                 ifelse(!is.null(tab_kraj$typ), unique(tab_kraj$typ), "3")))
  
  tab = rbind(tab_szk, tab_god, tab_kraj)
  tab$typ = factor(tab$typ, levels = kolejnosc)
  return(tab)
}
