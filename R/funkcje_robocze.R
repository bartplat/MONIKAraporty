#' @title Funkcje robocze dla raportowania w 3. edycji monitoringu - dane
#' administracyjne
#' @description Funkcja zwracająca kolejne numery wykresów. Funkcja ta była
#' częścią szablonów w poprzedniej edycji, a w obecnej będzie częścią pakietu,
#' żeby każdy raport nie ładował jej od nowa.
#' @return numeric
#' @export
wyk_num = function() {
  nr = get0("___nrWykresu___", parent.frame(), ifnotfound = 1)
  assign("___nrWykresu___", nr + 1, envir = parent.frame())
  return(nr)
}
#' @title Funkcje robocze dla raportowania w 3. edycji monitoringu - dane
#' administracyjne
#' @description Funkcja zwracająca kolejne numery tabel (analogiczna do funkcji
#' \code{wyk_num}). Funkcja ta była częścią szablonów w poprzedniej edycji, a w
#' obecnej będzie częścią pakietu, żeby każdy raport nie ładował jej od nowa.
#' @return numeric
#' @export
tab_num = function() {
  nr = get0("___nrTabeli___", parent.frame(), ifnotfound = 1)
  assign("___nrTabeli___", nr + 1, envir = parent.frame())
  return(nr)
}
#' @title Funkcje robocze dla raportowania w 3. edycji monitoringu - dane
#' administracyjne
#' @description Funkcja przycinająca tekst do zadanej szerokości liczonej
#' długością znaków. Przy dzieleniu na wiersze całe słowa są brane pod uwagę.
#' Źródło: \link{https://stackoverflow.com/a/3935429}
#' @param x ciąg znaków, który ma zostać podzielony
#' @param ... dowolny argument funkcji \code{strwrap()} np. \code{width = 80}
#' @return text
#' @export 
#' @examples
#' przykladowy_tekst = "Google trackers have been found on 75% of the top million websites. This means they are not only tracking what you search for, they're also tracking which websites you visit, and using all your data for ads that follow you around the Internet. Your personal data can also be subpoenaed by lawyers, including for civil cases like divorce. Google answered over 150,000 such data requests in 2019 alone! More and more people are also realizing the risk of relying on one company for so many personal services."
#' wrapper(przykladowy_tekst, width = 80)
wrapper = function(x, ...) {
  stopifnot(is.character(x))
  paste(strwrap(x, ...), collapse = "\n")
}
#' @title Funkcje robocze dla raportowania w 3. edycji monitoringu - dane
#' administracyjne
#' @description Funkcja używana w szablonie raportu, służąca do wyświetlania
#' wykresu oraz tabeli, w której pokazane są wartości z wykresu. Na wykresach
#' nie są prezentowane etykiety wartości mniejszych niż 6%, a z tabeli można je
#' odczytać.
#' @param wykres obiekt klasy \code{ggplot}
#' @param tabela obiekt klasy \code{knitr_kable}
#' @param tekst wartość logiczna określająca czy ma być zwracany tekst "Dane z
#' wykresu przedstawiono w poniższej tabeli:"?
#' @export
#' @return lista
#' @importFrom ggplot2 is.ggplot
zwroc_wykres_tabela = function(wykres, tabela, tekst = TRUE) {
  stopifnot(
    is.ggplot(wykres),
    class(tabela) %in% "knitr_kable")
  
  print(wykres)
  cat(ifelse(tekst, "Dane z wykresu przedstawiono w poniższej tabeli:\n\n", ""))
  cat(tabela)
}
