library(sf)
library(tidyverse)
library(tmap)

mtl_fn <- read_csv("data/surface_data/montreal/montreal_fishnet_landuse.csv")

new_mtl_dat <- read_csv("data/marshal_predictors_montreal2.csv")
read_csv("data/marshal_predictors.csv")


mtl_fn$my_id0 %>% summary()

new_mtl_dat$FID %>% summary()
new_mtl_dat$id %>% summary()

try_sf <- 
  new_mtl_dat %>%
  rename(my_id0 = id, id_Residential_300m = Residential_300m, id_Roads_300m = Roads_300m) %>%
  select(my_id0, id_Residential_300m, id_Roads_300m) %>%
  left_join(., mtl_fn) %>%
  left_join(., new_mtl_dat %>%
              rename(my_id0 = FID, FID_Residential_300m = Residential_300m, FID_Roads_300m = Roads_300m) %>%
              select(my_id0, FID_Residential_300m, FID_Roads_300m)) %>%
  filter(!is.na(point_lon)) %>%
  st_as_sf(., coords = c("point_lon", "point_lat"), crs = 4326)

try_sf_long <-
  try_sf %>%
  pivot_longer(., cols = c(id_Residential_300m, id_Roads_300m, FID_Residential_300m, FID_Roads_300m, Residential_100m, Roads_200m))

tm_shape(try_sf_long) +
  tm_dots("value") +
  tm_facets(by = "name", ncol = 3, free.scales = TRUE)
# IT'S id
new_mtl_dat %>% 
  select(my_id0 = id, Residential_300m:Roads_300m) %>%
  left_join(mtl_fn, .) %>%
  write_csv(., "data/surface_data/montreal/montreal_fishnet_landuse_05012023.csv")

# do for Toronto as well
to_fn <- read_csv("data/surface_data/toronto/toronto_fishnet_landuse.csv")

new_to_dat <- read_csv("data/marshal_predictors.csv")

to_fn$my_id0 %>% summary()

new_to_dat$FID %>% summary()
new_to_dat$id %>% summary()

to_try_sf <- 
  new_to_dat %>%
  rename(my_id0 = id, id_Residential_300m = Residential_300m, id_Roads_300m = Roads_300m) %>%
  select(my_id0, id_Residential_300m, id_Roads_300m) %>%
  left_join(., to_fn) %>%
  left_join(., new_to_dat %>%
              rename(my_id0 = FID, FID_Residential_300m = Residential_300m, FID_Roads_300m = Roads_300m) %>%
              select(my_id0, FID_Residential_300m, FID_Roads_300m)) %>%
  filter(!is.na(point_lon)) %>%
  st_as_sf(., coords = c("point_lon", "point_lat"), crs = 4326)

to_try_sf_long <-
  to_try_sf %>%
  pivot_longer(., cols = c(id_Residential_300m, id_Roads_300m, FID_Residential_300m, FID_Roads_300m, Residential_100m, Roads_200m))

tm_shape(to_try_sf_long) +
  tm_dots("value") +
  tm_facets(by = "name", ncol = 3, free.scales = TRUE)
# it's id for toronto as well
new_to_dat %>% 
  select(my_id0 = id, Residential_300m:Roads_300m) %>%
  left_join(to_fn, .) %>%
  write_csv(., "data/surface_data/toronto/toronto_fishnet_landuse_05012023.csv")
