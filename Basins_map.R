library(usethis)

libs <- c(
  "tidyverse", "sf",
  "giscoR"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if (any(installed_libraries == F)) {
  install.packages(
    libs[!installed_libraries]
  )
}

invisible(
  lapply(
    libs, library,
    character.only = T
  )
)

# 1. GET COUNTRY BORDERS
#-----------------------
print("GET COUNTRY BORDERS")
get_country_borders <- function() {
  country_borders <- giscoR::gisco_get_countries(
    resolution = "3",
    country = "COL"
  )
  
  return(country_borders)
}

country_borders <- get_country_borders()

# 2. GET BASINS
#---------------
print("GET BASINS")
# https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_sa_lev03_v1c.zip

get_basins <- function() {
  url <- "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_sa_lev03_v1c.zip"
  file_name <- "hybas_sa_lev03_v1c.zip"
  
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_basins()
print("basin downloaded")

list.files()

print("load basin")
load_basins <- function() {
  print("loading filenames")
  filenames <- list.files(
    pattern = ".shp$",
    full.names = TRUE
  )
  print(filenames)
  samerica_basin <- sf::st_read(
    filenames
  )
  
  return(namerica_basin)
}

samerica_basin <- load_basins()
print("basin loaded")

print("Intersect Basin with only the wanted Country Boundary.")
sf::sf_use_s2(F)

colombia_basin <- samerica_basin |>
  sf::st_intersection(
    country_borders
  ) |>
  dplyr::select(
    HYBAS_ID
  )

# Check basins

colombia_basin %>% 
  ggplot() +
  geom_sf()

# 3. GET RIVERS DATA
#-------------------
print("Get Rivers")
# https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip

get_rivers <- function() {
  url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip"
  file_name <- "sa-rivers.zip"
  
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_rivers()
print("Getting Rivers")
list.files()

load_rivers <- function() {
  filenames <- list.files(
    path = "HydroRIVERS_v10_sa_shp", #Now we look for the folder name
    pattern = ".shp$",
    full.names = T
  )
  print(filenames)
  samerica_rivers <- sf::st_read(
    filenames
  )
  
  return(samerica_rivers)
}

samerica_rivers <- load_rivers()

colombia_rivers <- samerica_rivers |>
  dplyr::select(
    ORD_FLOW
  ) |>
  sf::st_intersection(
    country_borders
  )

# 4. DETERMINE BASIN FOR EVERY RIVER
#-----------------------------------

colombia_river_basin <- sf::st_intersection(
  colombia_rivers,
  colombia_basin
)

# colombia_river_basin %>% 
#   ggplot() +
#   geom_sf()

# 5. RIVER WIDTH

# Here we map every order of a river to a specified width, for the 
#visualization to work

#---------------

unique(colombia_river_basin$ORD_FLOW)

colombia_river_basin_width <- colombia_river_basin |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      # width == 1 ~ .8,
      width == 2 ~ .7,
      width == 3 ~ .6,
      width == 4 ~ .45,
      width == 5 ~ .35,
      width == 6 ~ .25,
      width == 7 ~ .2,
      width == 8 ~ .1,
      # width == 9 ~ .1,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()



# 6. PLOT
#--------

unique(
  colombia_river_basin_width$HYBAS_ID
)

hcl.pals("qualitative")

p <- ggplot() +
  geom_sf(
    data = colombia_river_basin_width,
    aes(
      color = factor(
        HYBAS_ID
      ),
      size = width,
      alpha = width
    )
  ) +
  scale_color_manual(
    name = "",
    values = hcl.colors(
      6, "Dark 3",
      alpha = 1
    )
  ) +
  scale_size(
    range = c(.1, .7)
  ) +
  scale_alpha(
    range = c(.01, .7)
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.caption = element_text(
      size = 9, color = "grey60",
      hjust = .1, vjust = 10
    ),
    plot.margin = unit(
      c(
        t = 0, r = 0,
        b = 0, l = 0
      ),
      "lines"
    ),
    plot.background = element_rect(
      fill = "black",
      color = NA
    ),
    panel.background = element_rect(
      fill = "black",
      color = NA
    )
  ) +
  labs(
    title = "",
    x = "",
    y = "",
    # caption = "Source: ©World Wildlife Fund, Inc. (2006-2013) HydroSHEDS database http://www.hydrosheds.org"
  )

ggsave(
  filename = "colombia-river-basins.png",
  width = 7, height = 7.75, dpi = 600,
  bg = "white", device = "png", p
)

# st_write(colombia_river_basin_width, "colombia-river-basins.geojson", layer = NULL, driver = "GeoJson")

