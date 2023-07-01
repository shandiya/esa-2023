
library(galah)
library(tidyverse)
library(ggridges)
library(arrow)
library(sf)
library(rmapshaper)
library(ozmaps)
library(showtext)
library(monochromeR)
library(magick)
library(geomtextpath)


# data ------
# ecoassets datasets
bd <- open_dataset("data/biodiversity.csv", format = "csv")
events <- open_dataset("data/monitoring.csv", format = "csv")
ibra_griis <- readRDS("data/ibra_griis.rds")

#spatial
ibra_sf <- readRDS("data/ibra_sf.rds")
eez_sf <- readRDS("data/eez.rds")
capad_terrestrial <- readRDS("data/capad_terrestrial.rds")
capad_marine <- readRDS("data/capad_marine.rds")

# plotting functions
source("scripts/functions.R")

font_add_google("Poppins", "poppins")
showtext_auto()
showtext_opts(dpi = 300)

# 01. title () -----
bd |> 
  filter(speciesName == "Zanda funerea",
         ibraRegion != "") |> 
  group_by(year, ibraRegion) |> 
  summarise(count = sum(occurrenceCount), .groups = "drop") |> 
  collect() -> zf

ggplot(zf) +
  geom_jitter(aes(x = year, 
                 y = reorder(ibraRegion, count),
                 size = count), 
             colour = "#ffb703",
             alpha = 0.7, width = 0.25) +
  scale_size_area(max_size = 15) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        plot.margin = margin(1, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill =  "#fffaf3", colour = "#fffaf3"))

ggsave("images/title.png", height = 9, width = 16, units = "in")

bckgrnd <- ggplot(zf) +
  geom_jitter(aes(x = year, 
                  y = reorder(ibraRegion, count),
                  size = count), 
              colour = "#ffb703",
              alpha = 0.7, width = 0.25) +
  xlim(c(1900, 1970)) +
  scale_size_area(max_size = 15) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        plot.margin = margin(1, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill =  "#fffaf3", colour = "#fffaf3"))


ggsave("images/bckgrnd.png", bckgrnd, height = 9, width = 16, units = "in")  

# 02. acknowledgement (ridgeline) -----
regions <- c("Darwin Coastal", 
             "Pine Creek", 
             "Arnhem Coast")

saltie <- galah_call() |> 
  galah_identify("Crocodylus porosus") |> 
  galah_filter(cl1048 == regions) |> 
  galah_group_by(cl1048, month) |> 
  atlas_counts() 

ggplot(saltie, 
       aes(x = count, y = reorder(cl1048, count))) +
  geom_density_ridges(scale = 1.2,
                      fill = "#a47823",
                      colour = "#fffaf3",
                      size = 0.5) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0.5, 0, 0, 0, "cm"),
        plot.background = element_rect(fill = "#fffaf3", colour = "NA"),
        panel.background = element_rect(fill =  "#fffaf3", colour = "NA"))

ggsave("images/ack_ridges.png",
       width = 6,
       height = 1.5,
       units = "in")

# 03. monitoring -------
ibra_year <- ibra_sf |> 
  as_tibble() |> 
  expand(ibraRegion, 2010:2022) |> 
  rename(year = `2010:2022`)

ibra_biol_class  <- events |> 
  filter(featureFacet1 == "Biological Classification", 
         ibraRegion != "NA") |> 
  count(year, ibraRegion) |> 
  collect() |> 
  full_join(ibra_sf, by = join_by(ibraRegion)) |> 
  rowwise() |> 
  mutate(propCounts = log10(`n`/area),
         cutCounts = cut(propCounts, 
                         breaks = c(-6, -4, -3, -2, -1, 0),
                         labels = c(0.2, 0.4, 0.6, 0.8, 1.0),
                         include.lowest = TRUE)) |> 
  right_join(ibra_year, by = join_by(year, ibraRegion)) |> 
  st_as_sf(sf_column_name = "geometry") |>  
  group_by(year) |> 
  group_split(year) %>% 
  set_names(map(., ~.x$year[1]))

all_plots <- map(ibra_biol_class, plot_choropleth)

plotnames <- map(names(all_plots), ~ paste0("images/monitoring_", ., ".png")) 

walk2(plotnames, all_plots, ~ggsave(filename = .x, 
                                    plot = .y, 
                                    height = 10, 
                                    width = 10, 
                                    units = "in"))

list.files(path = "./images", pattern = "^monitoring_.*png$", full.names = TRUE) |>
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./images/monitoring.gif")


# 04. biodiversity  ---------
### channel-billed cuckoo -------
cbc_counts <- bd |> 
  filter(speciesName == "Scythrops novaehollandiae",
         between(year, 1970, 2020), 
         ibraRegion != "") |> 
  group_by(year) |> 
  summarise(count_year = sum(occurrenceCount),
            count_region = n_distinct(ibraRegion)) |> 
  collect()

### count of records -----
cbc_records <- ggplot(cbc_counts) + 
  geom_line(aes(x = year, y = count_year), colour = "#7E98A8") +
  geom_point(aes(x = year, y = count_year), colour = "#7E98A8", size = 3) +
  labs(title = "Count of records: 1970—2020") +
  ylim(c(0, 8000)) +
  theme_minimal() +
  theme(text = element_text(family = "poppins"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(face = "bold", size = 20, colour = "#273b50"),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background =  element_rect(fill = NA, colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("images/cbc_records.png", cbc_records, height = 8, width = 8, units = "in")

### count of regions ------
cbc_regions <- ggplot(cbc_counts) + 
  geom_line(aes(x = year, y = count_region), colour = "#7E98A8") +
  geom_point(aes(x = year, y = count_region), colour = "#7E98A8", size = 3) +
  labs(title = "Count of regions: 1970—2020") +
  ylim(c(0, 40)) +
  theme_minimal() +
  theme(text = element_text(family = "poppins"),
        axis.title = element_blank(),
        axis.text = element_text(size = 16),
        plot.title = element_text(face = "bold", size = 20, colour = "#273b50"),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background =  element_rect(fill = NA, colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("images/cbc_regions.png", cbc_regions, height = 8, width = 8, units = "in")

### range change -------
decades_ref <- tibble(year = 1970:2019, 
                      decade = rep(seq(1970, 2010, by = 10), each = 10))

cbc_range <- bd |> 
  filter(speciesName == "Scythrops novaehollandiae",
         between(year, 1970, 2019),
         ibraRegion != "") |> 
  select(year, occurrenceCount, ibraRegion) |> 
  collect() |> 
  left_join(decades_ref, by = join_by(year)) |> 
  full_join(ibra_sf, by = join_by(ibraRegion)) |> 
  filter(!is.na(decade)) |> 
  st_as_sf(sf_column_name = "geometry") |> 
  group_by(decade) |> 
  group_split(decade) %>% 
  set_names(map(., ~.x$decade[1]))

blues_pal <- generate_palette("#2c7da0",
                              modification = "go_both_ways",
                              n_colours = 5,
                              view_palette = TRUE, 
                              view_labels = FALSE)

cbc_range_plots <- map2(cbc_range, blues_pal, plot_range)

cbc_range_plotnames <- map(names(cbc_range_plots),
                           ~ paste0("images/cbc_range_", ., ".png")) 

walk2(cbc_range_plotnames, 
      cbc_range_plots, 
      ~ggsave(filename = .x,
              plot = .y,
              height = 10,
              width = 10,
              units = "in"))

list.files(path = "./images", pattern = "^cbc_range_.+\\.png$", full.names = TRUE) |>
  map(image_read) |>  # reads each path file
  image_join() |>  # joins image
  image_animate(delay = 100, optimize = TRUE) |>  # animates, can opt for number of loops
  image_write("./images/cbc_range.gif")

### invasive heatmap -------

heatmap <- ibra_griis |> 
  filter(yearStart >= 1971) |> 
  select(ibraRegion, 
         yearStart, 
         yearEnd, 
         griisStatus, 
         speciesCount) |> 
  pivot_wider(names_from = griisStatus, 
              values_from = speciesCount, 
              values_fill = 0) |> 
  rowwise() |> 
  mutate(timePeriod = paste0(yearStart, "—", yearEnd)) |> 
  group_by(ibraRegion, timePeriod) |> 
  summarise(propInvInt = (sum(Invasive, Introduced)) / 
              (sum(Invasive, Introduced, Native)), .groups = "drop") |>
  mutate(ibraRegionAbbr = abbreviate(ibraRegion, minlength = 3)) |> 
  plot_heatmap(legend_title = "Proportion of introduced/invasive\nspecies in IBRA regions",
               pal = "YlOrBr")
 
ggsave("images/heatmap.png", heatmap, height = 10, width = 18, units = "in")

### protected areas map -------

island_labels <- tibble(island = c("Cocos\n(Keeling)\nIslands", 
                                   "Christmas\nIsland", 
                                   "Norfolk\nIsland", 
                                   "Macquarie Island\nCommonwealth\nMarine Reserve"),
                        x = c(89, 105, 169, 172),
                        y = c(-12, -17, -35, -55))

protected_area_map <- ggplot() +
  geom_sf(data = eez_sf, fill = "#8eabb6", colour = NA) +
  geom_sf(data = ozmap_country, fill = "#FFE99A", colour = NA) +
  geom_sf(data = capad_terrestrial, fill = "#ffc803", colour = NA) +
  geom_sf(data = capad_marine, fill = "#0d4b63", colour = NA) +
  geom_text(data = island_labels, 
            aes(x = x, y = y, label = island), 
            size = 4, colour = "#273b50", family = "poppins") +
  coord_sf(xlim = c(85, 175), ylim = c(-60, 0)) +
  theme_void()

ggsave("images/protected_area_map.png", protected_area_map, 
       height = 8, width = 12, units = "in")

### waffle chart ---------
coris_picta <- bd |> 
  filter(speciesName == "Coris picta") |> 
  group_by(capadStatus) |> 
  summarise(count = sum(occurrenceCount)) |> 
  collect() 
  
df <- tibble(x = rep(seq(1:10), each = 10),
             y = rep(seq(1:10), times = 10))

wrasse_df <- df |> 
  rowid_to_column() |> 
  mutate(category = if_else(rowid <= 64, "protected area", "unprotected area"))

wrasse_points <- ggplot(wrasse_df) +
  geom_point(aes(x = x, y = y, fill = category),
             size = 8, 
             colour = "#7E98A8", 
             shape = 21, 
             stroke = 1.5) +
  scale_fill_manual(values = c("#7E98A8", "#00000000")) +
  coord_equal() +
  theme_void() +
  theme(legend.title = element_blank(),
        text = element_text(family = "poppins", 
                            size = 12, 
                            colour = "#273b50"))
  
ggsave("images/wrasse_points.png",
       wrasse_points, height = 5, width = 5, units = "in")

# 05. limitations --------
 
# get counts per year and save to avoid querying repeatedly
# bor_vals <- search_fields("basisOfRecord") |>
#   show_values() |>
#   filter(category != "OCCURRENCE") |> 
#   pull(category)
# 
# get_counts <- function(basis_of_record, ...) {
# 
#   galah_call() |>
#     galah_filter(year >= 1900,
#                  year <= 2022,
#                  year != "",
#                  basisOfRecord == basis_of_record) |>
#     galah_group_by(year) |>
#     atlas_counts() |>
#     mutate(type = {{basis_of_record}})
# }
# 
# bor_vals |>
#   map(get_counts) |>
#   list_rbind() |> 
#   mutate(year = as.numeric(year)) |> 
#   saveRDS("data/counts_per_year.rds")

counts_per_year <- readRDS("data/counts_per_year.rds")

cumul_counts <- counts_per_year |> 
  mutate(type = case_when(
    type == "MACHINE_OBSERVATION" ~ "Machine Observations",
    type %in% c("HUMAN_OBSERVATION", "OBSERVATION") ~ "Human Observations",
    TRUE ~ "Collections")) |> 
  group_by(year, type) |> 
  summarise(count = sum(count)) |> 
  group_by(type) |> 
  arrange(year) |> 
  mutate(cumul_count = cumsum(count)) 

bor_types <- c("Collections", "Human Observations", "Machine Observations")
bor_pal <- c("#4f772d", "#a47823", "#7E98A8")

coll <- cumul_counts |> 
  filter(type %in% bor_types[1]) |> 
  ggplot() +
  geom_textpath(aes(x = year, 
                    y = cumul_count,
                    label = type, 
                    colour = type, 
                    hjust = 0.9),
                lwd = 1.2) + 
  scale_color_manual(values = bor_pal[1]) +
  scale_y_continuous(name = NULL, 
                     labels = label_comma(),
                     limits = c(0, 100000000)) +
  scale_x_continuous(name = NULL, 
                     limits = c(1900, 2022),
                     breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        text = element_text(family = "poppins", colour = "#273b50"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("images/coll.png", coll, height = 6, width = 9, units = "in")
  
hobs <- cumul_counts |> 
  filter(type %in% bor_types[1:2]) |> 
  ggplot() +
  geom_textpath(aes(x = year, 
                    y = cumul_count,
                    label = type, 
                    colour = type, 
                    hjust = 0.9),
                lwd = 1.2) + 
  scale_color_manual(values = bor_pal[1:2]) +
  scale_y_continuous(name = NULL, 
                     labels = label_comma(),
                     limits = c(0, 100000000)) +
  scale_x_continuous(name = NULL, 
                     limits = c(1900, 2022),
                     breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        text = element_text(family = "poppins", colour = "#273b50"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("images/hobs.png", hobs, height = 6, width = 9, units = "in")

mobs <- cumul_counts |> 
  filter(type %in% bor_types[1:3]) |> 
  ggplot() +
  geom_textpath(aes(x = year, 
                    y = cumul_count,
                    label = type, 
                    colour = type, 
                    hjust = 0.9),
                lwd = 1.2) + 
  scale_color_manual(values = bor_pal[1:3]) +
  scale_y_continuous(name = NULL, 
                     labels = label_comma(),
                     limits = c(0, 100000000)) +
  scale_x_continuous(name = NULL, 
                     limits = c(1900, 2022),
                     breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        panel.background = element_rect(fill = "#fffaf3", colour = "#fffaf3"),
        text = element_text(family = "poppins", colour = "#273b50"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("images/mobs.png", mobs, height = 6, width = 9, units = "in")

