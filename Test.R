library(ggplot2)
library(dplyr)
library(grid)
library(rlang)

plot_result_circle <- function(df, party){
  
  hsize <- 3
  df_plot <- df %>%
    mutate(x = hsize) |> 
    filter(cat == "Ja" | cat == "Nein")
  
  party_wo <- enquo(party) # needed for the ggplot syntax
  
  abst <- df %>% 
    filter(cat == "Enthaltung") %>% 
    pull(!!party_wo)
  
  main_plot <- ggplot(df_plot, aes(x = hsize, y = !!party_wo, fill = cat)) +
    geom_col() +
    coord_polar(theta = "y", start = pi, direction = -1) +
    xlim(c(0.2, hsize + 0.5)) +
    theme_void() +
    scale_fill_manual(values = c('#00A86B', '#D32F2F')) +
    labs(fill='') +
    theme(plot.title = element_text(hjust = 0.5, size = 16), legend.position = "none") +
    annotate("text", x = 0.3, y = 0, 
             label = paste("Enthaltungen: \n", abst), 
             hjust = 0.5, vjust = 0.5, size = 5)
  
  # Display the plot with the line
  grid.newpage()
  print(main_plot)
  grid.lines(x = c(0.5, 0.5), y = c(0.79, 0.89), gp = gpar(col = "black", lwd = 4, lty = "solid", alpha = 0.8))
  
}

plot_result_circle(test, evp)
test <- data.frame(cat = c("Ja", "Nein", "Enthaltung"),
                   evp = c(35, 34, 8))
# Früher ------------------------------------------------------------------

library(diffobj)
library(stringr)
library(pkgmaker)

old <- "Während 37 Prozent des Fonds für den Aufbau der Armee genutzt werden sollen, sollen 50 Prozent in die militärische Forschung und den Ausbau der Europäischen Verteidigungsagentur fließen sowie der Schaffung von Arbeitsplätzen in der Europäischen Rüstungsindustrie dienen und 13 Prozent in die friedliche Intervention und diplomatische Konfliktbewältigung investiert werden und in die diplomatische Schulung und Ausbildung von Diplomaten."
new <- "Während 50 Prozent des Fonds für den Aufbau der Armee genutzt werden sollen, sollen 50 Prozent in die militärische Forschung und den Ausbau der Europäischen Verteidigungsagentur fließen sowie der Schaffung von Arbeitsplätzen in der Europäischen Rüstungsindustrie dienen."
med <- "Während 80 Prozent des Fonds für den Aufbau der Armee genutzt werden sollen, sollen 50 Prozent in die militärische Forschung und den Ausbau der Europäischen Verteidigungsagentur fließen sowie der Schaffung von Arbeitsplätzen in der Europäischen Rüstungsindustrie dienen."

diffOldNew <- as.character(diffChr(old, new, pager="off"))[1]
splitOldNew <- strsplit(diffOldNew, "")[[1]]
startMat <- str_locate_all(diffOldNew, "<span class='diffobj-trim'></span>")[[1]]


newBold <- splitOldNew[(startMat[3,2]+1):(startMat[4,1]-1)]
newBold <- gsub("<span class='diffobj-word insert'>", "<b>", paste(newBold, collapse = ""))
newBold <- gsub("</span>", "</b>", paste(newBold, collapse = ""))
newBold

oldBold <- splitOldNew[(startMat[1,2]+1):(startMat[2,1]-1)]
oldBold <- gsub("<span class='diffobj-word delete'>", "<b>", paste(oldBold, collapse = ""))
oldBold <- gsub("</span>", "</b>", paste(oldBold, collapse = ""))
oldBold


collapse_diff <- function(str_list){
  for (i in seq_along(str_list)) {
    if (names(str_list)[i] == "Diff") {
      str_list[i] <- paste0("<b>", str_list[i], "</b>")
    }
  }
  str_list |> unlist() |> paste(collapse = "")
}

# old <- "Der Asylfonds wird erhöht"
# new <- "Der Asylfonds wird verringert"







# format = "ansi8",

diffChr(old, new, format = "ansi8") |> attributes()#@cur.dat$eq

equal_string <- diffChr(old, new, format = "ansi8")@cur.dat$eq |> trimws()
eq_mat <- str_locate(new, strsplit(equal_string, " ")[[1]])
evp_new_split <- str_split(new, "")[[1]]

evp_diff_list <- list()

if (eq_mat[1,1]!=1) {
  evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[1:(eq_mat[1,1]-1)] |> paste(collapse = "")
}
for (i in 1:nrow(eq_mat)) {
  evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[eq_mat[i,1]:eq_mat[i,2]] |> paste(collapse = "")
  if (i < nrow(eq_mat)) {
    evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[(eq_mat[i,2]+1):(eq_mat[(i+1),1]-1)] |> paste(collapse = "")
  }else{
    if (eq_mat[i,2]!=length(evp_new_split)) {
      evp_diff_list[length(evp_diff_list)+1] <- evp_new_split[(eq_mat[i,2]+1):length(evp_new_split)] |> paste(collapse = "")
    }
  }
}

if (eq_mat[1,1]!=1) { # String B fängt anders an
  for (i in seq_along(evp_diff_list)) {
    if (i%%2==1) { # erster Part anders
      names(evp_diff_list)[i] <- "Diff"
    }else{
      names(evp_diff_list)[i] <- "Same"
    }
  }
}else{
  for (i in seq_along(evp_diff_list)) {
    if (i%%2==1) { # erster Part gleich
      names(evp_diff_list)[i] <- "Same"
    }else{
      names(evp_diff_list)[i] <- "Diff"
    }
  }
}
HTML(collapse_diff(evp_diff_list))
