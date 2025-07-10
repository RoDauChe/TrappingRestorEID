

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
#Fleas per species
trapping %>%
  filter(!is.na(genus_field)) %>%
  mutate(fleas = replace_na(fleas, 0)) %>%
  ggplot(aes(x = reorder(genus_field, -fleas), fill = genus_field), y = fleas) +
  geom_bar() +
  scale_fill_colorblind()
#flea average !!! -> count 1 for all?
trapping %>%
  filter(!is.na(genus_field)) %>%
  mutate(fleas = replace_na(fleas, 0)) %>%
  group_by(genus_field) %>%
  summarise(mean_fleas = mean(fleas)) %>%
  ggplot(aes(x = reorder(genus_field, -mean_fleas), fill = genus_field), y = mean_fleas) +
  geom_bar() +
  scale_fill_colorblind()
