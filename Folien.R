library(httr)
library(janitor)
library(lubridate)
library(polite)
library(readr)
library(rvest)
library(stringr)
library(tidyverse)

url_seats <- "https://www.europarl.europa.eu/meps/de/search/table"

df_seats <-  url_seats |> 
              read_html(as.data.frame = T, stringsAsFactors = TRUE) |> 
              html_nodes("table") %>%
              .[[1]] %>%
              html_table() |>
              select(EVP, 'S&D', Renew, 'Grüne/EFA', ID)


url_pres <- "https://en.wikipedia.org/wiki/Political_groups_of_the_European_Parliament"

df_pres <- url_pres |> 
            polite::bow() |>
            polite::scrape() |> 
            html_nodes("table.wikitable") %>%
            .[[1]] %>%
            html_table() |> 
            clean_names() |> 
            select(group_9_2, leader_s) |> 
            rename(caucus = group_9_2,
                   leader = leader_s) |> 
            filter(str_detect(caucus, paste(c("EPP", "S&D", "Renew", "Greens", "ID"), collapse = "|")))


df_caucus <- data.frame(total = c(df_seats[nrow(df_seats),]) |> unlist(),
                        countries = 27 - df_seats |> sapply(function(x) sum(is.na(x)))) |> 
                        #presidents = sapply(df_pres$leader, function(x) gsub("\\[|\\]|\\d", "", x, perl = TRUE) %>%
                         #                     gsub("\n", " \\& ", ., perl = TRUE))) |> 
              mutate(rank = order(df_caucus$total, decreasing = T))


write_csv(df_caucus, "caucus_data.csv", quote = "none")