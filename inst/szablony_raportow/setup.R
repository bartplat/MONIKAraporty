# biblioteki
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(MONIKAraporty)

# parametry
progLiczebnosci <- params$progLiczebnosci
typDokumentu <- params$typDokumentu
wyrownanieTabWykr <- params$wyrownanieTabWykr
rocznik <- paste0(params$rok - 1, "/", params$rok)

# dane
szk <- get(params$obiektWskazniki)
god <- get(params$obiektWskaznikiPorownanie) %>%
  filter(id_szk %in% szk$id_szk)

zawodowa <- ifelse(szk$typ_szk[[1]] %in% c("Branżowa szkoła I stopnia",
                                          "Branżowa szkoła II stopnia",
                                          "Technikum",
                                          "Szkoła policealna"),
                  1L, 0L)

if (zawodowa == 1L) {
  szkozaw <- szkozaw %>% 
    filter(id_szk %in% szk$id_szk)
}
data("powiaty")

# obiekt powiaty
powiaty_dop <- powiaty %>%
  filter(teryt_pow %in% szk$teryt_pow_szk[[1]]) %>%
  select(starts_with(szk$odniesieniePoziom[[1]])) %>%
  as.character()

# knitr options
knitr::opts_chunk$set(
  echo = FALSE,
  results = "asis",
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  fig.align = wyrownanieTabWykr,
  dev = "cairo_pdf"
)

rap_odmiany <- list(
  typ_szk_mian_lm = case_when(
  szk$typ_szk[[1]] == "Branżowa szkoła I stopnia" ~ "branżowe szkoły I stopnia", 
  szk$typ_szk[[1]] == "Branżowa szkoła II stopnia" ~ "branżowe szkoły II stopnia",
  szk$typ_szk[[1]] == "Technikum" ~ "technika",
  szk$typ_szk[[1]] == "Szkoła policealna" ~ "szkoły policealne",
  szk$typ_szk[[1]] == "Liceum ogólnokształcące" ~ "licea ogólnokształcące dla młodzieży",
  szk$typ_szk[[1]] == "Liceum dla dorosłych" ~ "licea ogólnokształcące dla dorosłych"),
typ_szk_dop_lm = case_when(
  szk$typ_szk[[1]] == "Branżowa szkoła I stopnia" ~ "branżowych szkół I stopnia", 
  szk$typ_szk[[1]] == "Branżowa szkoła II stopnia" ~ "branżowych szkół II stopnia",
  szk$typ_szk[[1]] == "Technikum" ~ "techników",
  szk$typ_szk[[1]] == "Szkoła policealna" ~ "szkół policealnych",
  szk$typ_szk[[1]] == "Liceum ogólnokształcące" ~ "liceów ogólnokształcących dla młodzieży",
  szk$typ_szk[[1]] == "Liceum dla dorosłych" ~ "liceów ogólnokształcących dla dorosłych")
)
