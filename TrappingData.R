set.seed(123)
coords <- data.frame(
  lon = rnorm(500, mean = -73.95, sd = 0.01),
  lat = rnorm(500, mean = 40.75, sd = 0.01)
)
ggplot(coords, aes(x = lon, y = lat)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5, color = NA) +
  geom_point(color = "black", size = 0.3, alpha = 0.5) +
  scale_fill_viridis_c(option = "inferno") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Coordinate Density Map", x = "Longitude", y = "Latitude")




install.packages("leaflet")
install.packages("leaflet.extras")

library(leaflet)
library(leaflet.extras)
leaflet(coords) %>%
  addTiles() %>%
  addHeatmap(lng = ~lon, lat = ~lat, radius = 15, blur = 20, max = 0.05, gradient = "YlOrRd")

mice.table<- data.frame(
  Species = c("Myodes","Apodemus","Microtus")
)
mice.table$Species[1]<- data.frame(
  Species = c("Myodes"),
  Sample.ID = c("BST1","BST3","BST5","BST10")
)
install.packages("palmerpenguins")
library(tidyverse)
library(palmerpenguins)
library(ggthemes)
glimpse(penguins)

ggplot(trapping, aes(x = fct_infreq(genus_field))) +
    geom_bar(mapping = aes(fill = genus_field)) +
  scale_color_colorblind()

trapping<- read_csv("TrappingR.csv")
glimpse(trapping)
df$genus_field[is.na(df$genus_field)] <- 0
library(stringr)
df$ticks <- as.numeric(df$ticks)


library(dplyr)
library(ggplot2)
library(forcats)
library(ggthemes)  # for colorblind palettes
library(tidyr)
trapping %>%
  filter(!is.na(genus_field)) %>%
  ggplot(aes(x = fct_infreq(genus_field))) +
  geom_bar(aes(fill = genus_field)) +
  scale_color_colorblind()
#BarChart of species count
trapping %>%
  filter(!is.na(genus_field)) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = fct_infreq(genus_field))) +
  geom_bar(aes(fill = sex)) +
  scale_color_colorblind()
#BarChart focusing on sex distribution
trapping %>%
  filter(!is.na(`weight (g)`), `weight (g)` <= 100) %>%
  ggplot(aes(x = `weight (g)`)) +
  geom_histogram(binwidth = 1, aes(fill = genus_field))
#Distribution of weight between species
trapping %>%
  filter(!is.na(`weight (g)`), `weight (g)` <= 100) %>%
  ggplot(aes(x = `weight (g)`, fill = genus_field)) +
  geom_density(linewidth = 0.75, alpha = 0.5)
#distribution of weight within species
trapping %>%
  filter(!is.na(genus_field)) %>%
  ggplot(aes(x = Location)) +
  geom_bar(aes(fill = genus_field)) +
  scale_color_colorblind()
#distribution of species biodiversity within different locations
trapping %>%
  filter(!is.na(genus_field)) %>%
  ggplot(aes(x = Location)) +
  geom_bar(position = "fill", aes(fill = genus_field)) +
  scale_color_colorblind()
#same as before but as percentage

#Could find out restoration age for each site, do species biodiversity vs restoration gradient in years


#Ecto per site
trapping %>%
  mutate(ticks = replace_na(ticks, 0)) %>%
  filter(!is.na(genus_field)) %>%
  ggplot(aes(x = genus_field, y = ticks)) +
  geom_bar() +
  scale_color_colorblind()
  