# biblioteki
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(MLASZraportyAdm4)

# parametry
progLiczebnosci = params$progLiczebnosci
typDokumentu = params$typDokumentu
wyrownanieTabWykr = params$wyrownanieTabWykr
rocznik = paste0(params$rok - 1, "/", params$rok)
zawodowa = ifelse(szk$typ_szk[[1]] %in% c("Branżowa szkoła I stopnia",
                                          "Branżowa szkoła II stopnia",
                                          "Technikum",
                                          "Szkoła policealna"),
                  1, 0)

# dane
szk = get(params$obiektWskazniki)
god = get(params$obiektWskaznikiPorownanie) %>%
  filter(id_szk %in% szk$id_szk)
if (zawodowa == 1) {
  szkozaw = szkozaw %>% 
    filter(id_szk %in% szk$id_szk)
}
data("powiaty")

# obiekt powiaty
powiaty_dop = powiaty %>%
  filter(teryt_pow %in% szk$teryt_pow[[1]]) %>%
  select(starts_with(szk$odniesieniePoziom[[1]])) %>%
  as.character()

# knitr options
knitr::opts_chunk$set(
  echo = FALSE,
  results = 'asis',
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = wyrownanieTabWykr
)
