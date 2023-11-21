library(data.table)
library(readr)
library(jsonlite)
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
  mutate(across(c(workAgePerc, refPerc), round, 1))

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

write_csv(df_asyl, "data_asyl.csv", quote = "none")

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



links <- xml2::read_html("https://ourworldindata.org/grapher/share-elec-by-source?tab=table&time=2021") |>
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
         renewShare = round((Bioenergy + Other + Wind + Solar+ Hydro), 0))

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

write_csv(df_GD, "data_gd.csv", quote = "none")
