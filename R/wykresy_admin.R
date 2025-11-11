#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca poziomy, skumulowany, wykres słupkowy na
#' potrzeby raportu. Jako argument zawierający zbiór danych należy podać ramkę
#' danych będącą wynikiem działania funkcji \code{tab_wykres_ad1()} lub
#' inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_bar scale_fill_brewer scale_y_continuous
#' labs geom_text coord_flip theme guides ylab xlab scale_x_discrete
#' @importFrom dplyr .data
wykres_poziomy_statusy = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))
  
  kolory_slupkow = c("#d62f26", "#585cd6", "#f8dd00", "#d8ef8a", "#1a974f")

  ggplot(x, aes(x = .data$typ,
                y = .data$value,
                fill = .data$name)) +
    geom_bar(width = 0.75, stat = "identity", colour = "white") +
    scale_fill_manual(values = kolory_slupkow,
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = .data$lab),
              position = position_stack(vjust = 0.5), size = 3,
              colour = "#000000") +
    ylab(NULL) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "bottom",
          legend.justification = "left",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE, reverse = TRUE)) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca pionowy, skumulowany, wykres słupkowy na
#' potrzeby raportu. Jako argument zawierający zbiór danych należy podać ramkę
#' danych będącą wynikiem działania funkcji \code{tab_wykres_ad1()} lub
#' inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_bar scale_fill_brewer scale_y_continuous
#' labs geom_text coord_flip theme guides ylab xlab scale_x_discrete
#' @importFrom dplyr .data
wykres_pionowy_statusy = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))
  
  kolory_slupkow = c("#d62f26", "#585cd6", "#f8dd00", "#d8ef8a", "#1a974f")
  
  ggplot(x, aes(x = .data$month,
                y = .data$value,
                fill = .data$name)) +
    geom_bar(width = 0.75, stat = "identity", colour = "white") +
    scale_fill_manual(values = kolory_slupkow,
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(limits = rev) +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = .data$lab),
              position = position_stack(vjust = 0.5), size = 3,
              colour = "#000000") +
    ylab(NULL) +
    xlab(NULL) +
    theme(legend.position = "bottom",
          legend.justification = "left",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 6, byrow = TRUE, reverse = TRUE)) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres warstwowy na potrzeby raportu. Jako
#' argument zawierający zbiór danych należy podać ramkę danych będącą wynikiem
#' działania funkcji \code{tab_wykres_wave_ad1()} lub inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_area scale_fill_brewer theme guides ylab xlab
#' labs
#' @importFrom dplyr .data
wave_chart = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))
  
  kolory_slupkow = c("#d62f26", "#585cd6", "#f8dd00", "#d8ef8a", "#1a974f")

  ggplot(x, aes(x = .data$month, y = .data$value, fill = .data$name)) +
    geom_area(position = position_stack(reverse = TRUE), stat = "identity") +
    theme(legend.position = "bottom",
          legend.justification = "left",
          legend.title = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          # plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    scale_fill_manual(values = kolory_slupkow,
                      guide = guide_legend(reverse = TRUE)) +
    guides(fill = guide_legend(nrow = 6, byrow = TRUE, reverse = TRUE), title = NULL) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Odsetek absolwentów") +
    xlab("Miesiąc") +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca poziomy wykres na potrzeby raportu. Jako
#' argument zawierający zbiór danych należy podać ramkę danych będącą wynikiem
#' działania funkcji \code{tab_wykres()} lub inną, analogiczną.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_bar scale_fill_brewer scale_y_continuous
#' labs geom_text coord_flip theme guides ylab xlab
#' @importFrom dplyr .data
wykres_poziomy_slupki = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$name, y = .data$value)) +
    geom_bar(width = 0.9, stat = "identity", colour = "white", fill = "#90CF60", position = position_dodge(width = 0.9)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_x_discrete(limits = rev) +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = lab),
              # position = position_stack(vjust = ifelse(round(x$value * 100) < 6, -1, 0.5)),
              position = position_stack(vjust = 1.05),
              size = 3,
              # nudge_y = ifelse(round(x$value * 100) < 6, -1, 0.5),
              colour = "#000000") +
    ylab(NULL) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres typu boxplot na potrzeby raportu. Wykres
#' pokazuje odsetki.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_errorbar geom_boxplot theme xlab
#' @importFrom dplyr .data
boxplot_proc = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ, fill = .data$typ)) +
    geom_errorbar(aes(ymin = .data$q5, ymax = .data$q95), width = 0.5) +
    geom_boxplot(aes(ymin = .data$q5,
                     lower = .data$q25,
                     middle = .data$med,
                     upper = .data$q75,
                     ymax = .data$q95),
                 stat = "identity",
                 width = 0.5) +
    scale_fill_manual(values = c("#8de04e",	"#f8dd00"),
                      guide = guide_legend(reverse = TRUE)) +
    # scale_y_continuous(limits = c(0.0, 1.0), expand = c(0,0)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11),
          panel.border = element_rect(colour = "lightgrey", fill = NA, size = 1),
          axis.line.y = element_line(colour = "lightgrey", size = 1)) +
    scale_y_continuous(labels = scales::percent) +
    xlab(NULL) +
    labs(title = tytul) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres słupkowy, pionowy, skumulowany na
#' potrzeby raportu.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_bar scale_y_continuous labs geom_text ylab
#' xlab theme
#' @importFrom dplyr .data
wykres_pion_skum = function(x, tytul) {
  stopifnot(is_tibble(x),
            ifelse(is.null(tytul), FALSE, nchar(tytul) > 1))

  ggplot(x, aes(x = .data$typ, y = .data$value, fill = .data$name)) +
    geom_bar(width = 0.75, stat = "identity", colour = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "RdYlGn") +
    labs(fill = NULL,
         title = tytul) +
    geom_text(aes(label = lab), position = position_stack(vjust = 0.5), size = 3, colour = "#000000") +
    ylab(NULL) +
    xlab(NULL) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "white", size = 1.5),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11)) +
    guides(fill = guide_legend(nrow = 7, byrow = TRUE)) %>%
    return()
}
#' @title Wykresy w raportach z 3. rundy monitoringu na danych administracyjnych
#' @description Funkcja rysująca wykres słupkowy, pionowy, skumulowany na
#' potrzeby raportu.
#' @param x ramka z danymi do przedstawienia na wykresie
#' @param tytul tytuł wykresu w formie ciągu tekstowego lub wartość NULL dla
#' braku tytułu
#' @param os_x zmienna zawierająca etykiety osi x - miesiące lub przedziały
#' @param tytul_x tytuł osi x
#' @export
#' @return wykres
#' @importFrom tibble is_tibble
#' @importFrom ggplot2 ggplot geom_bar scale_y_continuous labs geom_text ylab
#' xlab theme
#' @importFrom dplyr .data
wyk_facet = function(x, tytul, os_x, tytul_x) {
  stopifnot(is_tibble(x))

  os_x = ensym(os_x)

  ggplot(x, aes(x = .data[[os_x]], y = .data$pct)) +
    geom_bar(aes(fill = .data$typ), stat = "identity") +
    xlab(tytul_x) +
    facet_grid(cols = vars(.data$typ)) +
    ylab(NULL) +
    ylim(c(0, 1)) +
    scale_fill_manual(values = c("#8de04e",	"#f8dd00", "#fb6a59"),
                      guide = guide_legend(reverse = TRUE)) +
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "lightgrey"),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.title = element_text(size = 11),
          panel.grid.major.y = element_line(colour = "lightgrey"),
          panel.grid.minor.y = element_line(colour = "lightgrey")) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0.0, 1.05)) +
    labs(title = tytul) +
    ylab("Odsetek absolwentów") %>%
    return()
}
