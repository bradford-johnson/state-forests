#---- load packages ----
pacman::p_load(tidyverse,
               ggpath,
               geojsonio,
               rgdal,
               sp,
               rgeos,
               broom,
               ggtext,
               showtext,
               htmltools)

showtext_auto()

#---- load data ----
forest <- read_csv("data/forest.csv")

states <- read_csv("data/states.csv")

local_image_path <- "data/pine-forest.png"

#---- wrangle and clean data ----
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

#---- prepare geo data ----
hex <- geojson_read("data/us_states_hexgrid.geojson", what = "sp")

hex@data = hex@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))

hex_fortify <- tidy(hex, region = "google_name")

hex_fortify <- hex_fortify |>
  filter(id != "District of Columbia")

#---- join geo and forest data ----
hex_fortify <- hex_fortify %>%
  left_join(forest_cov, by=c("id"="state"))

#---- prepare data for visual ----
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

#---- load visual dependencies ----
## font
## load fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Sahitya", family = "Sahitya")

font_add_google(name = "Jost", family = "Jost")

font_1 <- "Jost"

## load caption
caption = paste0("<span style='font-family:fb;color:#082b3b;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#e1dbd3;'>.</span>",
                 "<span style='font-family:Jost;color:#082b3b;'>bradfordjohnson<span style='font-family:sans;color:#e1dbd3;'><b>'</b></span> | <b>Data</b> Wisevoter</span>")

## load subtitle
# subtitle = paste0("<span>Forest coverage in 2023</span><br>")

## colors
bg_col <- "#e1dbd3" #fffcf0"
text_col <- "#082b3b"
#---- create visual ----
ggplot() +
  geom_polygon(data = hex_fortify, aes(x = long, y = lat, group = group, fill = bin), color="#e1dbd3") +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = text_col, size = 8, family = font_1) +
  theme_void() +
  coord_map () +
  scale_fill_manual(values = my_palette) +
  
  labs(title = local_image_path,
       subtitle = "Forest coverage in 2023 by state",
       caption = caption,
       fill = "Forest Coverage") +
  guides(fill = guide_legend(override.aes = aes(size = 5))) +
  # guides(fill = guide_legend(reverse = TRUE)) +
  theme(plot.caption = ggtext::element_textbox_simple(margin = margin(2,0,0,0,"mm"), size = 17),
        plot.subtitle = element_text(vjust = 13, hjust = .65, size = 50, color = text_col, family = font_1, lineheight = .5),
        plot.title = element_path(hjust = 0, vjust = 0, size = 2, alpha = 0.9),
        plot.background = element_rect(fill = bg_col, color =  bg_col),
        legend.text = element_text(family = font_1, color = text_col, size = 17),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.background = element_rect(fill = bg_col, color =  bg_col),
        plot.margin = unit(c(4,2,2,1),"mm"))

# ggsave("forest.png", width = 7, height = 5)

ggsave("forest.png", width = 6, height = 5)
