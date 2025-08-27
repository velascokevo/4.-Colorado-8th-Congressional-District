############################################################
### Loading libraries
############################################################

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(forcats) 
library(treemapify)
library(ggplotify)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggtext)
library(webshot2)
library(lubridate)
library(ggh4x)
library(tidycensus)
library(tigris)
library(ggiraph)
library(htmlwidgets)
library(sf)
library(stringr)
library(here)

############################################################
### Setting up directories
############################################################

setwd("YOUR WORKING DIRECTORY HERE/8th Congressional District")

############################################################
### Loading in and cleaning data
############################################################

### Presidential elections
# Data from the Colorado Secretary of State Historical Election Database 

# 2024 presidential election
pres_2024_raw <- read_csv("Data/presidential_election_precinct_2024.csv") |> 
  rename(area_type = "...1",
         id = "...2",
         dem = "Democratic...3",
         rep = Republican,
         total_votes = `Total Votes Cast`,
         total_ballots = `Total Ballots Cast`) |> 
  mutate(third = Unaffiliated + Libertarian + Green + `Unity Party of Colorado` + `American Constitution` + 
           `Approval Voting` + `American Solidarity` + `Socialist Labor` + `No Party Affiliation` + 
           Independent + `Unaffiliated/Republican` + `Democratic...16`,
         election = "pres",
         year = 2024) |> 
  select(area_type, id, year, election, dem, rep, third, total_votes, total_ballots)

### Representative elections

# 2022: 8th congressional district
rep_2022_raw <- read_csv("Data/rep_election_8th_precinct_2022.csv") |> 
  rename(area_type = "...1",
         id = "...2",
         dem = Democratic,
         rep = Republican,
         total_votes = "Total Votes Cast",
         total_ballots = "Total Ballots Cast") |> 
  mutate(third = Libertarian + `Colorado Center`,
         election = "rep",
         year = 2022,
         co_di = 8) |> 
  select(area_type, id, year, election, dem, rep, third, total_votes, total_ballots, co_di)

# 2024: 8th congressional district
rep_2024_raw <- read_csv("Data/rep_election_8th_precinct_2024.csv") |> 
  rename(area_type = "...1",
         id = "...2",
         dem = Democratic,
         rep = Republican,
         total_votes = "Total Votes Cast",
         total_ballots = "Total Ballots Cast") |> 
  mutate(third = `Unity Party of Colorado` + `Approval Voting` + Unaffiliated,
         election = "rep",
         year = 2024,
         co_di = 8) |> 
  select(area_type, id, year, election, dem, rep, third, total_votes, total_ballots, co_di)

### Combining datasets

election_data <- bind_rows(rep_2022_raw, rep_2024_raw, pres_2024_raw) |> 
  filter(area_type == "Precinct",
         str_sub(id, 1, 1) == 8) |> 
  mutate(id = as.character(id))

### Map files

# Weld
# Derives from Weld Conuty's official website
weld <- st_read(here("Data",
                     "Weld Boundaries",
                     "Precincts_open_data.shp")) |> 
  rename(id = PRECINCT,
         shape_leng = Shape_Leng) |> 
  mutate(county = "weld") |> 
  select(id, county, geometry)

# Larimer
# Derives from Larimer County's official website
larimer <- st_read(here("Data",
                        "Larimer Boundaries",
                        "VoterPrecinct.shp")) |> 
  rename(id = PRECINCT,
         shape_leng = SHAPE_LEN) |> 
  mutate(county = "larimer") |> 
  select(id, county, geometry)

larimer <- st_transform(larimer, st_crs(weld))
larimer <- st_cast(larimer, "MULTIPOLYGON")

# Adams
# Derives from https://data-adcogov.opendata.arcgis.com/datasets/ADCOGOV::voter-precincts/about
adams <- st_read(here("Data",
                      "Adams Boundaries",
                      "Voter_Precincts.shp")) |> 
  rename(id = Full_Numbe) |> 
  mutate(county = "adams") |> 
  select(id, county, geometry)

adams <- st_transform(adams, st_crs(weld))

### Combining geometry and election data

election_geom_data <- bind_rows(weld, adams, larimer) |> 
  filter(str_sub(id, 1, 1) == 8) |> 
  mutate(id = as.character(id)) |> 
  left_join(election_data, by = "id")

### Removing redundant dataframes

rm(pres_2024_raw, rep_2022_raw, rep_2024_raw, election_data, adams, larimer, weld)

############################################################
### Map of CO-08
############################################################

fig1 <- election_geom_data |> 
  filter(election == "rep",
         year == 2024) |> 
  st_transform(4326) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(county)),
          alpha = 0.6) + 
  scale_fill_manual(values = c(adams = "#FDB462",
                               weld = "#80B1D3",
                               larimer = "#B3DE69")) + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  guides(fill = "none") + 
  labs(title = "1. Map of CO-08",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig1.png", fig1, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Election results
############################################################

fig2 <- election_geom_data |> 
  filter(election == "rep") |> 
  group_by(year) |> 
  summarize(total_dem = sum(dem),
            total_rep = sum(rep),
            total_third = sum(third)) |> 
  st_drop_geometry() |> 
  pivot_longer(cols = c(total_dem, total_rep, total_third)) |> 
  mutate(label = comma(value),
         name = recode(name,
                       "total_dem" = "Democrat",
                       "total_rep" = "Republican",
                       "total_third" = "Third Party")) |> 
  ggplot() + 
  geom_bar(mapping = aes(x = year, y = value, fill = name),
           stat = "identity",
           position = "dodge",
           color = "black",
           width = 1.2) + 
  geom_text(mapping = aes(x = year, 
                          y = value, 
                          label = label, 
                          group = name),
            position = position_dodge(width = 1.2),
            fontface = "bold",
            hjust = 0.5,
            vjust = -1,
            size = 4.5)  + 
  scale_fill_manual(values = c(
    "Democrat" = "dodgerblue2",
    "Republican" = "red2",
    "Third Party" = "goldenrod2")) + 
  scale_y_continuous(labels = scales::label_number(scale = 1/1000,
                                                   suffix = "k")) + 
  labs(title = "2. CO-08 Congressional House Races",
       fill = "") + 
  theme_fivethirtyeight() + 
  scale_x_continuous(
    breaks = c(2022, 2024),
    labels = c("2022", "2024")) + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_line(color = "grey80"),  # horizontal lines
        panel.grid.major.x = element_blank(),                 # still hide vertical
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig2.png", fig2, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Voting preference scatterplot
############################################################

fig4 <- election_geom_data |> 
  filter(year == 2024) |> 
  mutate(dem_pct = round(((dem / (dem + rep)) * 100), 1),
         color = if_else(rep > dem, "Republican Leaning", "Democratic Leaning")) |> 
  ggplot() + 
  geom_point(mapping = aes(x = dem_pct,
                           y = total_votes,
                           size = total_votes,
                           color = color),
             alpha = 0.25) +
  geom_smooth(mapping = aes(x = dem_pct,
                            y = total_votes),
              color = "goldenrod2",
              method = "lm",
              se = FALSE) + 
  scale_color_manual(values = c("Republican Leaning" = "red2",
                                "Democratic Leaning" = "dodgerblue2")) + 
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70),
                     labels = scales::label_number(suffix = "%")) + 
  scale_y_continuous(labels = label_comma()) + 
  scale_size_continuous(range = c(1, 9),
                        guide = "none") + 
  labs(title = "4. Precincts by Total Number of Votes and Democratic \nShare",
       size = "",
       x = "Share of Democratic Vote",
       y = "Total Number of Votes",
       color = "",
       caption = "Democratic vote percentage is relative to Democratic plus Republican votes and does not consider third party votes.") + 
  theme_fivethirtyeight() + 
  theme(legend.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig4.png", fig4, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Voter turnout size
############################################################

# 2024
centroids <- election_geom_data |> 
  filter(election == "rep",
         year == 2024) |> 
  st_transform(4326) |> 
  st_centroid() |> 
  mutate(`Democratic vote` = round(dem / (dem + rep) * 100))

fig3 <- election_geom_data |> 
  filter(election == "rep",
         year == 2024) |> 
  st_transform(4326) |> 
  ggplot() + 
  geom_sf(fill = "grey90",
          color = "grey95") + 
  geom_sf(data = centroids,
          mapping = aes(size = total_votes,
                        color = `Democratic vote`),
          alpha = 0.7) + 
  scale_size_continuous(range = c(1, 9)) + 
  scale_color_steps2(
    low = "red2",
    high = "dodgerblue3",
    midpoint = 50,
    labels = function(x) paste0(x, "%"),
    breaks = seq(20, 80, 10),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 15)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  guides(size = "none") + 
  labs(title = "3. C0-08's Concentration of Votes in 2024, \nby Precinct",
       subtitle = "Size of bubble relative to number of votes per precinct.",
       size = "Total number \nof votes",
       caption = "Democratic vote percentage is relative to Democratic plus \nRepublican votes and does not consider third party votes.") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig3.png", fig3, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Growth in voter turnout
############################################################

election_geom_data |> 
  filter(election == "rep") |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = total_votes - lag(total_votes)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ungroup() |> 
  group_by(county) |> 
  summarize(sum = sum(diff))

############################################################
### Percentage of Democratic voters
############################################################

# 2022
election_geom_data |> 
  filter(year == 2022,
         election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep),
         classification = case_when(
           dem_pct > 0.65 ~ "Heavy Democrat",
           dem_pct <= 0.65 & dem_pct > 0.58 ~ "Likely Democrat",
           dem_pct <= 0.58 & dem_pct > 0.52 ~ "Lean Democrat",
           dem_pct <= 0.52 & dem_pct >= 0.48 ~ "Neutral",
           dem_pct < 0.48 & dem_pct >= 0.42 ~ "Lean Republican",
           dem_pct < 0.42 & dem_pct >= 0.35 ~ "Likely Republican",
           dem_pct < 0.35 ~ "Heavy Republican"),
         classification = factor(classification,
                                   levels = c(
                                     "Heavy Democrat", "Likely Democrat", "Lean Democrat",
                                     "Neutral",
                                     "Lean Republican", "Likely Republican", "Heavy Republican"
                                   ))) |>
  filter(!is.na(classification)) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(classification)),
          color = "black",
          linewidth = 0.2,
          alpha = 0.9) + 
  scale_fill_manual(values = c(
    "Heavy Democrat" = "#08306b",
    "Likely Democrat" = "#2171b5",
    "Lean Democrat" = "#6baed6",
    "Neutral" = "#f0f0f0",
    "Lean Republican" = "#fcbba1",
    "Likely Republican" = "#fb6a4a",
    "Heavy Republican" = "#99000d")) + 
  labs(title = "",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

# 2024
fig5 <- election_geom_data |> 
  filter(year == 2024,
         election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep),
         classification = case_when(
           dem_pct > 0.65 ~ "Heavy Democrat",
           dem_pct <= 0.65 & dem_pct > 0.58 ~ "Likely Democrat",
           dem_pct <= 0.58 & dem_pct > 0.52 ~ "Lean Democrat",
           dem_pct <= 0.52 & dem_pct >= 0.48 ~ "Neutral",
           dem_pct < 0.48 & dem_pct >= 0.42 ~ "Lean Republican",
           dem_pct < 0.42 & dem_pct >= 0.35 ~ "Likely Republican",
           dem_pct < 0.35 ~ "Heavy Republican"),
         classification = factor(classification,
                                 levels = c(
                                   "Heavy Democrat", "Likely Democrat", "Lean Democrat",
                                   "Neutral",
                                   "Lean Republican", "Likely Republican", "Heavy Republican"
                                 ))) |> 
  filter(!is.na(classification)) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(classification)),
          color = "black",
          linewidth = 0.2,
          alpha = 0.9) + 
  scale_fill_manual(values = c(
    "Heavy Democrat" = "#08306b",
    "Likely Democrat" = "#2171b5",
    "Lean Democrat" = "#6baed6",
    "Neutral" = "#f0f0f0",
    "Lean Republican" = "#fcbba1",
    "Likely Republican" = "#fb6a4a",
    "Heavy Republican" = "#99000d")) + 
  labs(title = "5. Precincts by Political Affiliation in House Race",
       subtitle = "2024",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig5.png", fig5, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Closeups
############################################################

### Solid Democrats

election_geom_data |> 
  filter(year == 2024,
         election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep),
         classification = case_when(
           dem_pct > 0.65 ~ "Heavy Democrat",
           dem_pct <= 0.65 & dem_pct > 0.58 ~ "Likely Democrat",
           dem_pct <= 0.58 & dem_pct > 0.52 ~ "Lean Democrat",
           dem_pct <= 0.52 & dem_pct >= 0.48 ~ "Neutral",
           dem_pct < 0.48 & dem_pct >= 0.42 ~ "Lean Republican",
           dem_pct < 0.42 & dem_pct >= 0.35 ~ "Likely Republican",
           dem_pct < 0.35 ~ "Heavy Republican"),
         class2 = if_else(classification == "Heavy Democrat" | classification == "Likely Democrat",
                          classification,
                          "Other")) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(class2)),
          color = "grey80",
          linewidth = 0.2) + 
  scale_fill_manual(values = c(
    "Heavy Democrat" = "dodgerblue4",
    "Likely Democrat" = "dodgerblue1",
    "Other" = "grey80")) + 
  labs(title = "",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

### Solid Republicans

election_geom_data |> 
  filter(year == 2024,
         election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep),
         classification = case_when(
           dem_pct > 0.65 ~ "Heavy Democrat",
           dem_pct <= 0.65 & dem_pct > 0.58 ~ "Likely Democrat",
           dem_pct <= 0.58 & dem_pct > 0.52 ~ "Lean Democrat",
           dem_pct <= 0.52 & dem_pct >= 0.48 ~ "Neutral",
           dem_pct < 0.48 & dem_pct >= 0.42 ~ "Lean Republican",
           dem_pct < 0.42 & dem_pct >= 0.35 ~ "Likely Republican",
           dem_pct < 0.35 ~ "Heavy Republican"),
         class2 = if_else(classification == "Heavy Republican" | classification == "Likely Republican",
                          classification,
                          "Other")) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(class2)),
          color = "grey90",
          linewidth = 0.2,
          alpha = 0.9) + 
  scale_fill_manual(values = c(
    "Heavy Republican" = "firebrick4",
    "Likely Republican" = "firebrick1",
    "Other" = "grey90")) + 
  labs(title = "",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

### Toss-ups

election_geom_data |> 
  filter(year == 2024,
         election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep),
         classification = case_when(
           dem_pct > 0.65 ~ "Heavy Democrat",
           dem_pct <= 0.65 & dem_pct > 0.58 ~ "Likely Democrat",
           dem_pct <= 0.58 & dem_pct > 0.52 ~ "Lean Democrat",
           dem_pct <= 0.52 & dem_pct >= 0.48 ~ "Neutral",
           dem_pct < 0.48 & dem_pct >= 0.42 ~ "Lean Republican",
           dem_pct < 0.42 & dem_pct >= 0.35 ~ "Likely Republican",
           dem_pct < 0.35 ~ "Heavy Republican"),
         class2 = if_else(classification == "Lean Republican" | classification == "Lean Democrat" | classification == "Neutral",
                          classification,
                          "Other")) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = factor(class2)),
          color = "grey90",
          linewidth = 0.2) + 
  scale_fill_manual(values = c(
    "Lean Republican" = "lightcoral",
    "Lean Democrat" = "skyblue1",
    "Neutral" = "purple4",
    "Other" = "grey90")) + 
  labs(title = "",
       fill = "") + 
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

############################################################
### PP change in preferred candidate
############################################################

fig6 <- election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.2) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2.5),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25),
    limits = c(-17.84, 10.05)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "6. Change in Share of Votes For Democratic \nCandidate in CO-08",
       subtitle = "2022 to 2024",
       fill = "Percentage point \ndifference") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig6.png", fig6, width = 10, height = 8, units = "in", dpi = 300)

election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") 

# Adams County
fig6a <- election_geom_data |> 
  filter(election == "rep",
         county == "adams") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.3) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2.5),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25),
    limits = c(-17.84, 10.05)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "6a. Change in Share of Votes For CO-08 Democratic \nCandidate in Adams County",
       subtitle = "2022 to 2024",
       fill = "Percentage point \ndifference") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig6a.png", fig6a, width = 10, height = 8, units = "in", dpi = 300)

# Larimer County
election_geom_data |> 
  filter(election == "rep",
         county == "larimer") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.3) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2.5),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25),
    limits = c(-17.84, 10.05)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "X",
       fill = "") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

# Weld County
fig6b <- election_geom_data |> 
  filter(election == "rep",
         county == "weld") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.3) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2.5),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25),
    limits = c(-17.84, 10.05)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "6b. Change in Share of Votes For CO-08 Democratic \nCandidate in Weld County",
       subtitle = "2022 to 2024",
       fill = "Percentage point \ndifference") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig6b.png", fig6b, width = 10, height = 8, units = "in", dpi = 300)

# Top 10 gains and losses
top10 <- election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ungroup() |> 
  slice_max(order_by = diff, n = 10) |> 
  mutate(status = "top10")

bottom10 <- election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ungroup() |> 
  slice_min(order_by = diff, n = 10) |> 
  mutate(status = "bottom10")

election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2)) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ungroup() |> 
  left_join(top10, by = "id")

############################################################
### PP change in preferred candidate: scatterplot
############################################################

fig7 <- election_geom_data |> 
  filter(election == "rep") |> 
  mutate(dem_pct = dem / (dem + rep)) |> 
  arrange(id, year) |> 
  group_by(id) |> 
  mutate(diff = dem_pct - lag(dem_pct),
         diff = round(diff * 100, 2),
         maj = if_else(rep > dem, "rep", "dem"),
         prev_maj = lag(maj),
         color = if_else(prev_maj == "rep", "Republican Leaning", "Democratic Leaning")) |> 
  filter(!is.na(diff),
         diff != "NaN") |> 
  ggplot() + 
  geom_point(mapping = aes(x = diff,
                           y = total_votes,
                           size = total_votes,
                           color = color),
             alpha = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "grey50",
             size = 0.8) +
  scale_color_manual(values = c("Democratic Leaning" = "dodgerblue2",
                                "Republican Leaning" = "red2")) + 
  scale_y_continuous(labels = label_comma()) + 
  scale_x_continuous(breaks = c(-20, -15, -10, -5, 0, 5, 10, 15)) + 
  scale_size_continuous(range = c(1, 10),
                        guide = "none") + 
  labs(title = "7. Change in Support for Democratic Candidate \nin CO-08",
       subtitle = "2022 to 2024",
       caption = "Political party majority based on 2022 results.",
       x = "Percentage point difference",
       y = "Total number of votes",
       color = "") + 
  theme_fivethirtyeight() + 
  theme(legend.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.ticks.y = element_line(color = "black"),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig7.png", fig7, width = 10, height = 8, units = "in", dpi = 300)

############################################################
### Difference in candidate popularity
############################################################

fig8a <- election_geom_data |> 
  filter(year == 2024) |> 
  mutate(dem_share = dem / total_votes) |> 
  select(id, county, election, geometry, dem_share) |> 
  pivot_wider(id_cols = c(id, geometry),
              names_from = election,
              values_from = dem_share) |> 
  mutate(diff = rep - pres,
         diff = round(diff * 100, 1)) |> 
  filter(!is.na(diff)) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.2) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "8a: Difference in Support Between Democratic \nPresidential and House Candidates",
       subtitle = "2024",
       fill = "Percentage point \ndifference") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig8a.png", fig8a, width = 10, height = 8, units = "in", dpi = 300)

fig8b <- election_geom_data |> 
  filter(year == 2024) |> 
  mutate(rep_share = rep / total_votes) |> 
  select(id, county, election, geometry, rep_share) |> 
  pivot_wider(id_cols = c(id, geometry),
              names_from = election,
              values_from = rep_share) |> 
  mutate(diff = rep - pres,
         diff = round(diff * 100, 1)) |> 
  filter(!is.na(diff)) |> 
  ggplot() + 
  geom_sf(mapping = aes(fill = diff),
          color = "black",
          linewidth = 0.2) + 
  scale_fill_steps2(
    low = "red2",
    mid = "white",
    high = "dodgerblue2",
    midpoint = 0,
    breaks = seq(-10, 10, 2),
    guide = guide_colorsteps(barwidth = 0.85,
                             barheight = 25)) +
  coord_sf(default_crs = sf::st_crs(4326),
           expand = FALSE,
           datum = NA) + 
  labs(title = "8b: Difference in Support Between Republican \nPresidential and House Candidates",
       subtitle = "2024",
       fill = "Percentage point \ndifference") + 
  theme_fivethirtyeight() + 
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA))

ggsave("Graphs/fig8b.png", fig8b, width = 10, height = 8, units = "in", dpi = 300)

election_geom_data |> 
  filter(year == 2024) |> 
  select(id, county, election, geometry, dem, total_votes) |> 
  pivot_wider(id_cols = c(id, geometry, county),
              names_from = election,
              values_from = c(dem, total_votes)) |> 
  group_by(county) |> 
  summarize(sum_dem_rep = sum(dem_rep),
            sum_total_rep = sum(total_votes_rep),
            sum_dem_pres = sum(dem_pres),
            sum_total_pres = sum(total_votes_pres)) |> 
  mutate(share_dem_rep = sum_dem_rep / sum_total_rep,
         share_dem_pres = sum_dem_pres / sum_total_pres,
         diff = share_dem_rep - share_dem_pres) |> 
  select(county, diff, share_dem_rep, share_dem_pres, everything())

election_geom_data |> 
  filter(year == 2024) |> 
  select(id, county, election, geometry, rep, total_votes) |> 
  pivot_wider(id_cols = c(id, geometry, county),
              names_from = election,
              values_from = c(rep, total_votes)) |> 
  group_by(county) |> 
  summarize(sum_rep_rep = sum(rep_rep),
            sum_total_rep = sum(total_votes_rep),
            sum_rep_pres = sum(rep_pres),
            sum_total_pres = sum(total_votes_pres)) |> 
  mutate(share_rep_rep = sum_rep_rep / sum_total_rep,
         share_rep_pres = sum_rep_pres / sum_total_pres,
         diff = share_rep_rep - share_rep_pres) |> 
  select(county, diff, share_rep_rep, share_rep_pres, everything())

############################################################
### Election results
############################################################

election_geom_data |> 
  filter(election == "rep") |> 
  group_by(year) |> 
  summarize(total_dem = sum(dem),
            total_rep = sum(rep),
            total_third = sum(third))

rep_2024_total <- election_geom_data |> 
  st_drop_geometry() |> 
  filter(election == "rep") |> 
  group_by(year) |> 
  arrange(year) |> 
  mutate(county = as.character(county)) |> 
  summarize(county = "CO-08",
            total_dem = sum(dem),
            total_rep = sum(rep),
            total_votes = sum(total_votes)) |> 
  ungroup() |> 
  mutate(pct_dem = total_dem / total_votes,
         pct_rep = total_rep / total_votes) |> 
  arrange(year) |> 
  mutate(diff = (pct_dem - lag(pct_dem)),
         diff_rep = (pct_rep - lag(pct_rep)),
         hypothetical = round(diff * total_votes),
         diff = diff * 100,
         diff_rep = diff_rep * 100,
         total_dem = comma(total_dem),
         total_rep = comma(total_rep),
         total_votes = comma(total_votes),
         pct_dem = paste0(round(pct_dem * 100, 1), "%"),
         pct_rep = paste0(round(pct_rep * 100, 1), "%"),
         diff = round(diff, 1),
         diff_rep = round(diff_rep, 1),
         hypothetical = comma(hypothetical)) |> 
  filter(year == 2024) |> 
  mutate(hypothetical = "")

election_geom_data |> 
  st_drop_geometry() |> 
  filter(election == "rep") |> 
  group_by(county, year) |> 
  summarize(total_dem = sum(dem), -1,
            total_rep = sum(rep),
            total_third = sum(third),
            total_votes = sum(dem, rep, third)) |> 
  mutate(pct_dem = total_dem / total_votes,
         diff = (pct_dem - lag(pct_dem)),
         pct_rep = total_rep / total_votes,
         diff_rep = (pct_rep - lag(pct_rep)),
         hypothetical = round(diff * total_votes),
         diff = diff * 100,
         diff_rep = diff_rep * 100,
         total_dem = comma(total_dem),
         total_rep = comma(total_rep),
         total_votes = comma(total_votes),
         pct_dem = paste0(round(pct_dem * 100, 1), "%"),
         pct_rep = paste0(round(pct_rep * 100, 1), "%"),
         diff = round(diff, 1),
         diff_rep = round(diff_rep, 1),
         hypothetical = comma(hypothetical),
         county = recode(county,
                         "adams" = "Adams",
                         "larimer" = "Larimer", 
                         "weld" = "Weld")) |> 
  rbind(rep_2024_total) |> 
  rename(`Votes for Democratic Party` = total_dem,
         `Votes for Republican Party` = total_rep,
         `Total Number of Votes` = total_votes,
         `Total Vote % for Democrat` = pct_dem,
         `Total Vote % for Republican` = pct_rep,
         `2022-24 pp Difference in Votes for Dem` = diff,
         `2022-24 pp Difference in Votes for Rep` = diff_rep,
         `Proportional Gain/Loss for Dem` = hypothetical,
         County = county) |> 
  ungroup() |> 
  filter(year == 2024) |> 
  select(County, `Votes for Democratic Party`, `Votes for Republican Party`, `Total Number of Votes`, `Total Vote % for Democrat`,
         `2022-24 pp Difference in Votes for Dem`, `Total Vote % for Republican`, `2022-24 pp Difference in Votes for Rep`,
         `Proportional Gain/Loss for Dem`) |> 
  gt() |> 
  tab_header(title = md("**9. 2024 CO-08 House Race Results**")) |> 
  tab_options(heading.align = "left") |> 
  tab_style(style = cell_text(size = px(26),
                              weight = "bold"),
            locations = cells_title(groups = "title")) |> 
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(everything())) |> 
  cols_label(County = md("")) |> 
  tab_style(style = list(cell_text(color = "navyblue")),
            locations = cells_body(columns = County)) |> 
  tab_style(style = list(cell_text(color = "goldenrod3", weight = "bold")),
            locations = cells_body(rows = County == "CO-08", columns = County)) |> 
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = c(County,
                                               `2022-24 pp Difference in Votes for Dem`,
                                               `Proportional Gain/Loss for Dem`,
                                               `2022-24 pp Difference in Votes for Rep`))) |> 
  tab_style(style = list(cell_text(color = "chartreuse4")), 
            locations = cells_body(columns = c(`2022-24 pp Difference in Votes for Dem`, 
                                               `Proportional Gain/Loss for Dem`), 
                                   rows = (`2022-24 pp Difference in Votes for Dem` > 0))) |>
  tab_style(style = list(cell_text(color = "chartreuse4")), 
            locations = cells_body(columns = c(`2022-24 pp Difference in Votes for Rep`), 
                                   rows = (`2022-24 pp Difference in Votes for Rep` > 0))) |>
  tab_style(style = list(cell_text(color = "red2")),
            locations = cells_body(columns = c(`2022-24 pp Difference in Votes for Dem`,
                                               `Proportional Gain/Loss for Dem`),
                                   rows = (`2022-24 pp Difference in Votes for Dem` < 0))) |> 
  tab_style(style = list(cell_text(color = "red2")),
            locations = cells_body(columns = c(`2022-24 pp Difference in Votes for Rep`),
                                   rows = (`2022-24 pp Difference in Votes for Rep` < 0))) |> 
  opt_table_font(font = list(google_font("Lato"),
                             default_fonts())) |> 
  gt::gtsave("Graphs/fig9.png") 

