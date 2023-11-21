library(tidyverse)
library(ggplot2)
library(ggparliament)

df <- data.frame(
  party_short = c("Sumar (Grüne/Linke)", "PSOE (S&D)", "Sonstige", "PP (EVP)", "VOX (EKR)"),
  seats = c(31, 122, 7 + 7 + 6 + 5 + 1 + 2, 136, 33),
  seats19 = c(38, 120, 13 + 8 + 5 + 6 + 1 + 18, 89, 52)
) |> 
  mutate(change = seats - seats19)



calc_coordinates <- function (N, M, limits, segment = 0.5) {
    radii <- seq(limits[1], limits[2], len = M)
    counts <- numeric(M)
    pts <- do.call(rbind, lapply(1:M, function(i) {
      counts[i] <<- round(N * radii[i]/sum(radii[i:M]))
      theta <- seq(0, segment * 2 * pi, len = counts[i])
      N <<- N - counts[i]
      data.frame(x = radii[i] * cos(theta), y = radii[i] * 
                   sin(theta), row = i, theta = theta)
    }))
    pts <- pts[order(-pts$theta, -pts$row), ]
    pts
}

df_coord <- calc_coordinates(sum(df$seats), 10, c(1, 2))

parl_data <- as.data.frame(df[rep(row.names(df), df$seats), ])

df_plot <- cbind(parl_data, df_coord)


ggplot(df_plot, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  theme_ggparliament() +
  theme(plot.title = element_text(hjust = 0.5, size = 18), legend.position = "bottom") + 
  scale_colour_manual(values = c("#00A859", "#FF0000", "grey", "#6699FF", "#00008B"),
                      limits = df$party_short) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 2.1), color = "black", size = 2) +
  labs(color = "")



df$party_short <- factor(df$party_short, levels = unique(df$party_short))
ggplot(df, aes(x = party_short, y = change)) +
  geom_bar(stat = "identity", fill = c("#00A859", "#FF0000", "grey", "#6699FF", "#00008B")) + 
  theme_void()
