rm(list = ls())

library(data.table)
library(haven)
library(jsonlite)
library(readr)
library(rvest)
library(stringr)
library(tidyverse)
library(WDI)


# manuelles Übersetzen der Ländernamen
translation_data <- data.frame(
  en = c("Austria", "Belgium", "Croatia", "Denmark", "Estonia",
         "European Union", "Finland", "France", "Germany", "Greece",
         "Hungary", "Ireland", "Italy", "Poland", "Romania", "Spain"),
  de = c("Österreich", "Belgien", "Kroatien", "Dänemark", "Estland",
         "Europäische Union", "Finnland", "Frankreich", "Deutschland", "Griechenland",
         "Ungarn", "Irland", "Italien", "Polen", "Rumänien", "Spanien")
)

translate_country <- function(country){
  translation_data[translation_data$en == country, "de"]
}

# Asyl- und Migration -----------------------------------------------------

# NY.GDP.PCAP.CD # GDP/capita
# SP.POP.TOTL # pop total
# SM.POP.REFG # refugee population
# SP.POP.DPND # % working-age population

set_point <- function(x){
  formatC(x, digits = nchar(as.integer(x)), big.mark=".", decimal.mark = ",")
}

df_asyl <- WDI(country = c("EU", "AT", "BE", "HR", "DK", "EE", "ES", "FI", "FR", "DE", "GR",
                           "HU", "IE", "IT", "PL", "RO"),
               indicator = c("NY.GDP.PCAP.CD", "SP.POP.TOTL", "SM.POP.REFG", "SP.POP.DPND"),
               start = 2022, end = 2022) |>
  rename(gdpCap = NY.GDP.PCAP.CD,
         totPop = SP.POP.TOTL,
         refPop = SM.POP.REFG,
         workAgePerc = SP.POP.DPND) |> 
  mutate(refPerc = (refPop*100)/totPop,
         flag = paste0("Flaggen/", iso3c, ".png"),
         country = translate_country(country)) |> 
  mutate(across(gdpCap, round, -2)) |> 
  mutate(across(c(workAgePerc, refPerc), round, 1)) |> 
  mutate(across(refPerc, ~gsub("\\.", ",", formatC(., digits = 2, decimal.mark = ","))))|>
  mutate(across(workAgePerc, ~gsub("\\.", ",", formatC(., digits = 3, decimal.mark = ","))))

df_asyl$gdpCap <- df_asyl$gdpCap |> 
  sapply(function(x) set_point(x))

df_asyl <- df_asyl |> 
  mutate(gdpCapEu = df_asyl[df_asyl$country == "Europäische Union", "gdpCap"],
         refPercEu = df_asyl[df_asyl$country == "Europäische Union", "refPerc"],
         workAgePercEu = df_asyl[df_asyl$country == "Europäische Union", "workAgePerc"]) |> 
  select(country, gdpCap, refPerc, workAgePerc, flag, gdpCapEu, refPercEu, workAgePercEu) |> 
  filter(country != "Europäische Union")

# manuelle Zuweisung EU-Außengrenze
df_asyl$border <- c("Nein", # AUT
                    "Nein", # BEL
                    "Ja", # HRV
                    "Nein", # DNK
                    "Ja", # EST
                    "Ja", # FIN
                    "Ja", # FRA
                    "Nein", # DEU
                    "Ja", # GRC
                    "Ja", # HUN
                    "Ja", # IRL
                    "Ja", # ITA
                    "Ja", # POL
                    "Ja", # ROU
                    "Ja") # ESP

write_csv(df_asyl, "data_asyl.csv")

# Green Deal --------------------------------------------------------------

# NY.GDP.PCAP.CD # GDP/capita
# EN.ATM.CO2E.PC # CO2/capita
# NV.AGR.TOTL.ZS # agriculture share gdp

df_GD <- WDI(country = c("EU", "AT", "BE", "HR", "DK", "EE", "ES", "FI", "FR", "DE", "GR",
                         "HU", "IE", "IT", "PL", "RO"),
             indicator = c("NY.GDP.PCAP.CD", "NV.AGR.TOTL.ZS"),
             start = 2022, end = 2022) |> 
  left_join(WDI(country = c("EU", "AT", "BE", "HR", "DK", "EE", "ES", "FI", "FR", "DE", "GR",
                            "HU", "IE", "IT", "PL", "RO"),
                indicator = c("EN.ATM.CO2E.PC"),
                start = 2020, end = 2020)[, c("iso2c", "EN.ATM.CO2E.PC")], by = "iso2c") # latest CO2 data for 2020
df_GD <- df_GD |> 
  rename(gdpCap = NY.GDP.PCAP.CD,
         agriPerc = NV.AGR.TOTL.ZS,
         co2Cap = EN.ATM.CO2E.PC) |>
  mutate(flag = paste0("Flaggen/", iso3c, ".png"))

# Scrape Our World in Data
links <- xml2::read_html("https://ourworldindata.org/grapher/share-elec-by-source?tab=table&time=2021") |>
# links <- xml2::read_html("https://ourworldindata.org/grapher/share-elec-by-source?tab=table&time=2022") |>
  html_nodes("link")
all_urls <- links[html_attr(links, "rel") == "preload"] |> 
  html_attr("href")
json_urls <- grep("json$", all_urls, value = TRUE)


for (i in seq(1, length(json_urls), 2)) {
  data <- fromJSON(json_urls[i])|> data.frame()
  meta <- fromJSON(json_urls[i+1])
  
  data <- data |> 
    left_join(meta$dimensions$entities$values, join_by(entities == id))
  data[data$name == "European Union (27)", "code"] <- "EUU"
  
  col_name <- str_split(str_split(meta$name, pattern = " \\(%")[[1]][1], pattern = " ")[[1]][1]
  
  df_GD <- data |> 
    filter(years == 2021) |> # latest available data
    select(values, code) |> 
    rename(!!col_name := values, iso3c := code) |> 
    right_join(df_GD, join_by(iso3c == iso3c))
}


df_GD <- df_GD |> 
  mutate(across(c(gdpCap), round, -2)) |> 
  mutate(across(c(agriPerc, co2Cap), round, 1)) |> 
  mutate(country = translate_country(country)) |> 
  mutate(fossilShare = round((Oil + Gas + Coal), 0),
         nuclearShare = round(Nuclear, 0),
         renewShare = round((Bioenergy + Other + Wind + Solar+ Hydro), 0))|> 
  mutate(across(c(agriPerc, co2Cap), ~gsub("\\.", ",", formatC(., digits = 2, decimal.mark = ","))))

df_GD$gdpCap <- df_GD$gdpCap |> 
  sapply(function(x) set_point(x))

df_GD <- df_GD |> 
         mutate(gdpCapEu = df_GD[df_GD$country == "Europäische Union", "gdpCap"],
               agriPercEu = df_GD[df_GD$country == "Europäische Union", "agriPerc"],
               co2CapEu = df_GD[df_GD$country == "Europäische Union", "co2Cap"],
               fossilShareEu = df_GD[df_GD$country == "Europäische Union", "fossilShare"],
               nuclearShareEu = df_GD[df_GD$country == "Europäische Union", "nuclearShare"],
               renewShareEu = df_GD[df_GD$country == "Europäische Union", "renewShare"]) |> 
  select(country, gdpCap, agriPerc, co2Cap, 
         flag, fossilShare, nuclearShare, renewShare,
         gdpCapEu, agriPercEu, co2CapEu,
         fossilShareEu, nuclearShareEu, renewShareEu) |> 
  filter(country != "Europäische Union")

write_csv(df_GD, "data_gd.csv")

# Armee -------------------------------------------------------------------

# NY.GDP.PCAP.CD # GDP/capita
# MS.MIL.XPND.GD.ZS # Military % GDP

# Ansicht zu Verteidigungsausgaben

# Download "ZA8764_v1-0-0.dta" from https://search.gesis.org/research_data/ZA8764
df_eurob <- read_dta("ZA8764_v1-0-0.dta")
df_eurob <- df_eurob |>
              select(isocntry, q6_2) |>
              # Russia’s invasion of Ukraine shows the EU needs to
              # increase military cooperation between Member States
              rename(country = isocntry,
              incr_coop = q6_2)
num_resp <- df_eurob$country |> table() 

{df_coop <- df_eurob |> 
              group_by(country) |> 
              summarise(agree = sum(incr_coop %in% c(1, 2)),
              disagr = sum(incr_coop %in% c(3, 4))) |> 
              mutate(agree = round(agree * 100/num_resp, 0),
                     disagr = round(disagr * 100/num_resp, 0)) |>
              as_tibble()
df_coop <- rbind(df_coop, list("EU", round(mean(df_coop$agree), 0), round(mean(df_coop$disagr), 0))) |> 
            filter(country %in% c("EU", "AT", "BE", "HR", "DK", "EE", "ES", "FI", "FR", "DE", "GR",
                                  "HU", "IE", "IT", "PL", "RO"))}


df_mil <- WDI(country = c("EU", "AT", "BE", "HR", "DK", "EE", "ES", "FI", "FR", "DE", "GR",
                "HU", "IE", "IT", "PL", "RO"),
            indicator = c("NY.GDP.PCAP.CD", "MS.MIL.XPND.GD.ZS"),
            start = 2022, end = 2022)

df_mil <- df_mil |> 
            rename(gdpCap = NY.GDP.PCAP.CD,
                  milPerc = MS.MIL.XPND.GD.ZS) |>
            mutate(flag = paste0("Flaggen/", iso3c, ".png"))

# Wehrpflicht

webpage <- "https://worldpopulationreview.com/country-rankings/countries-with-mandatory-military-service" |> 
  read_html()
df_subs <- html_table(html_nodes(webpage, ".mb-5"), fill = TRUE)[[2]] |> 
  filter(Country %in% c(
    "Austria",
    "Belgium",
    "Croatia",
    "Denmark",
    "Estonia",
    "Spain",
    "Finland",
    "France",
    "Germany",
    "Greece",
    "Hungary",
    "Ireland",
    "Italy",
    "Poland",
    "Romania"
  )) |> 
  rename(subscription = `Mandatory Military Service`) |> 
  select(Country, subscription)

# manuelles Übersetzen von Yes/No
translation_data_sub <- data.frame(
  en = c("Yes", "No", "De jure"),
  de = c("Ja", "Nein", "Nein (de facto)")
)

translate_sub <- function(varx){
  translation_data_sub[translation_data_sub$en == varx, "de"]
}

df_mil <- df_mil |> 
          left_join(df_subs, join_by(country==Country)) |> 
          left_join(df_coop, join_by(iso2c==country)) |> 
          mutate(across(c(gdpCap), round, -2)) |> 
          mutate(across(c(milPerc), round, 2)) |> 
          mutate(country = translate_country(country)) |> 
          mutate(across(milPerc, ~gsub("\\.", ",", formatC(., digits = 3, decimal.mark = ","))))

df_mil$gdpCap <- df_mil$gdpCap |> 
  sapply(function(x) set_point(x))

        
df_mil <- df_mil |> 
  mutate(gdpCapEu = df_mil[df_mil$country == "Europäische Union", "gdpCap"],
         milPercEU = df_mil[df_mil$country == "Europäische Union", "milPerc"],
         agreeEU = df_mil[df_mil$country == "Europäische Union", "agree"],
         disagrEU = df_mil[df_mil$country == "Europäische Union", "disagr"]) |> 
  select(country, gdpCap, milPerc, subscription, 
         agree, disagr, flag,
         gdpCapEu, milPercEU,
         agreeEU, disagrEU) |> 
  filter(country != "Europäische Union")

df_mil$subscription <- df_mil$subscription |> 
                        sapply(function(x) translate_sub(x)) |> 
                        unlist() |>
                        unname()

write_csv(df_mil, "data_mil.csv")
