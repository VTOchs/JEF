library("data.table")
library(httr)
library(ggparliament)
library(janitor)
library(lubridate)
library(polite)
library(readr)
library(rvest)
library(stringr)
library(tidyverse)


# Seats -------------------------------------------------------------------


url_seats <- "https://www.europarl.europa.eu/meps/de/search/table"

df_all <- url_seats |> 
          read_html(as.data.frame = T, stringsAsFactors = TRUE) |> 
          html_nodes("table") %>%
          .[[1]] %>%
          html_table()

df_seats <-  df_all |>
              select(EVP, 'S&D', Renew, 'Grüne/EFA', ID)


url_pres <- "https://en.wikipedia.org/wiki/Political_groups_of_the_European_Parliament"

df_pres <- url_pres |> 
            polite::bow() |>
            polite::scrape() |> 
            html_nodes("table.wikitable") %>%
            .[[1]] %>%
            html_table() |> 
            clean_names() |> 
            select(group_8_2, leader_s) |> 
            rename(caucus = group_8_2,
                   leader = leader_s) |> 
            filter(str_detect(caucus, paste(c("EPP", "S&D", "Renew", "Greens", "ID"), collapse = "|")))


df_caucus <- data.frame(total = c(df_seats[nrow(df_seats),]) |> unlist(),
                        countries = 27 - df_seats |> sapply(function(x) sum(is.na(x))),
                        presidents = sapply(df_pres$leader, function(x) gsub("\\[|\\]|\\d", "", x, perl = TRUE) %>%
                                              gsub("\n", " \\& ", ., perl = TRUE)))
df_caucus <- df_caucus |> 
  mutate(rank = order(df_caucus$total, decreasing = T))


write_csv(df_caucus, "caucus_data.csv", quote = "none")



# Committees --------------------------------------------------------------

translation_data <- data.frame(
  en = c("EPP", "S&D", "Renew", "G / EFA", "ECR", "ID", "GUE  /  NGL"),
  de = c("EVP", "S&D", "Renew", "Grüne", "EKR", "ID", "GUE/NGL")
)

translate_group <- function(group){
  translation_data[translation_data$en == group, "de"]
}


url_com <- "https://en.wikipedia.org/wiki/Committees_of_the_European_Parliament"

df_com <- url_com |> 
  polite::bow() |>
  polite::scrape() |> 
  html_nodes("table.wikitable") %>%
  .[[1]] %>%
  html_table() |> 
  clean_names() |>
  select(committee_2, committee_3, members, chair_2, chair_7) |> 
  rename(name = committee_2,
         short = committee_3,
         party = chair_2,
         vorsitz = chair_7) |> 
  distinct(short, .keep_all = TRUE) |>
  rows_delete(tibble(short = "Committee")) |> 
  arrange(short) |> 
  filter(short %in% c("AFET", "AGRI", "BUDG", "DROI", "ITRE", "LIBE", "SEDE", "TRAN"))

# Falls Anmerkungen vor Vorsitzender
{df_com$vorsitz <- sapply(df_com$vorsitz|> unname(), function(x){
  if (grepl("\n", x)) {
    x <- strsplit(x, "\n")[[1]][2]
  } else{
    x <- x
  }
})
df_com$vorsitz <- unname(df_com$vorsitz)}

# write_csv(df_com, "committee_data.csv", quote = "none")
df_com |> select(-name)
