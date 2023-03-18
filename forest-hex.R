# load packages
pacman::p_load(tidyverse,
               geojsonio,
               rgdal,
               sp,
               rgeos,
               broom)

# load data
forest <- read_csv("data/forest.csv")

states <- read_csv("data/states.csv")

# wrangle and clean data
## clean up percent col
forest$forest_coverage <- gsub("%","",forest$forest_coverage)

forest$forest_coverage <- as.numeric(forest$forest_coverage)

forest$forest_coverage <- forest$forest_coverage / 100

## clean up acre col
forest$acres <- gsub(" ac", "", forest$acres)

forest$acres <- gsub(",", "", forest$acres)

forest$acres <- as.numeric(forest$acres)

## create df
forest_cov <- forest %>%
  select(-acres)

## wrangle and join with states.csv data
states$state <- str_to_title(states$state)

forest_cov |>
  inner_join(states, by = c("state" = "state"))

# prepare geo data
hex <- geojson_read("data/us_states_hexgrid.geojson", what = "sp")

hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

hex_fortify <- tidy(hex, region = "google_name")

hex_fortify <- hex_fortify |>
  filter(id != "District of Columbia")

# join geo and forest data
hex_fortify <- hex_fortify %>%
  left_join(forest_cov, by=c("id"="state"))

# prepare data for visual
## hex labels
centers <- cbind.data.frame(data.frame(gCentroid(hex, byid=TRUE), id=hex@data$iso3166_2))

centers <- centers |>
  filter(id != "DC")

## look into forest data
range(forest_cov$forest_coverage)
mean(forest_cov$forest_coverage)
median(forest_cov$forest_coverage)

# forest_cov |>
#   ggplot(aes(x = forest_coverage)) +
#   geom_boxplot()

## create bins
hex_fortify$bin <- cut(hex_fortify$forest_coverage, breaks = c(0, .2, 0.4, 0.6, 0.8, 1.0), labels = c("< 20%", "20-40%", "40-60%", "60-80%", "> 80%"))

## create color pal
my_palette <- paletteer::paletteer_d("RColorBrewer::Greens", n = 5)

## create visual
ggplot() +
  geom_polygon(data = hex_fortify, aes(x = long, y = lat, group = group, fill = bin), color="#f7f7f7") +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "#252525", size = 5) + #Add our labels
  theme_void() +
  coord_map () +
  scale_fill_manual(values = my_palette) +
  labs(fill = "Forest Coverage") +
  guides(fill = guide_legend(reverse = TRUE))

