####################################################################################
####################################################################################

#パッケージ
library(ggplot2)
library(tmap)
library(tmaptools)
# library(tidyverse)
library(shinyjs)
library(sf)

# install.packages("tmap")
# install.packages("tmaptools")
# install.packages("shinyjs")
# install.packages("sf")
# install.packages("jpndistrict")
# install.packages("remotes")
# remotes::install_github("uribo/jpndistrict")

library(jpndistrict)

colnames(kokudo_df)


test_border <- st_read("/Users/ito_hiroki/01.Research/Railway-project/02.raw/N03-140401_GML/N03-14_140401.shp") 

test_border_geo <- test_border %>% 
  dplyr::group_by(N03_001) %>% 
  summarise()

# jpn <- st_read("jpn_pref.shp", options = "ENCODING=UTF-8")

jpn_simple <- st_simplify(test_border_geo, dTolerance = 0.1)


kokudo_df <- st_read ("/Users/ito_hiroki/01.Research/Railway-project/02.raw/shape_file",
                      options = c("ENCODING=CP932")) 
  # dplyr::filter(name_local != "沖縄県")

help("read_sf")

help("st_read")
# tm_shape(test_border_geo) +
#   tm_borders()

ggplot(test_border_geo) +
  geom_sf()

kokudo_df <- st_simplify(kokudo_df, dTolerance = 0.1)


japan_geo <- read_sf(here::here('02.raw',
                                'shapefile',
                                'japan_ver84'))

colnames(japan_geo)


dummy_df <- master_data %>%
  dplyr::select(city_id, dummy) %>% 
  distinct()
  

japan_geo_2 <- japan_geo %>% 
  dplyr::filter(KEN != "沖縄県",
                SIKUCHOSON != "小笠原村") %>% 
  dplyr::rename(city_id = JCODE) %>% 
  dplyr::mutate(city_id = as.numeric(city_id))  

japan_geo3 <- dplyr::left_join(japan_geo_2, dummy_df) %>% 
  dplyr::mutate_at(c('dummy'), ~replace(., is.na(.), "NA")) %>%
  dplyr::mutate(dummy = as.character(dummy)) 
  
install.packages("maps")

library(maps)

world_map <- map_data("world")
  
colnames(world_map)

jpn_map <- world_map %>% 
  dplyr::filter(region == "Japan") 

  # ggplot(aes(x = long, y = lat, group = group)) +
  # geom_polygon(fill = "lightgray", colour = "black", size = 0.1)




tm_test <-  tm_shape(japan_geo3) +
  tm_borders(lwd = 0.1) +
  tmap_options(check.and.fix = TRUE) +
  tm_fill(
    col =  "dummy",
    style = "order",
    title = "City Type",
    # title = "density",
    breaks = c("0", "1", "NA"),
    palette = MyPalette) 
  

tm_test

tmap_save(tm_test, filename = here::here('04.analyze','synthetic_control',
                                         'add_outcome_predictor',
                                         'mapping', 'japan_map.png'))



# ggsave(tm_test, here::here('04.analyze','synthetic_control',
#                            'add_outcome_predictor',
#                            'mapping', 'japan_map.png'))


sf::st_is_valid(japangeo)


here::here('02.raw','shape_world','data')


test_jp <- read_sf(here::here('02.raw',
                              'shape_world',
                              'data')) %>% 
  dplyr::filter(sov_a3 == "JPN") %>% 
  dplyr::select(name,geometry) %>% 
  dplyr::rename(prefecture = name) %>% 
  dplyr::mutate(dummy = "NA") %>% 
  dplyr::filter(prefecture != "Okinawa")

japangeo_df <- japan_geo3 %>% 
  dplyr::filter(dummy %in% c("0","1")) %>% 
  dplyr::select(KEN, geometry, dummy) %>% 
  dplyr::rename(prefecture = KEN) %>% 
  dplyr::mutate(dummy = ifelse((dummy == "1"),"Treatment","Control"))


japangeo_df <- m



japan_map_df <- bind_rows(japangeo_df, test_jp)
  # dplyr::mutate(dummy = as.numeric(dummy))

colnames(test_jp)



MyPalette <- c("gray", "white","black")

tmap_japan <- tm_shape(japan_map_df) +
  tm_borders() +
  tm_fill(
    col =  "dummy",
    style = "order",
    title = "City Type",
    # title = "density",
    breaks = c("NA", "Control", "Treatment"),
    palette = MyPalette) +
  tm_layout(legend.show = FALSE) +
  tm_shape(japangeo_df) +
  tm_fill(
    col =  "dummy",
    style = "order",
    title = "City",
    breaks = c("Control", "Treatment"),
    palette = c("gray", "black")) +
    tm_layout(legend.show = TRUE,
              frame = FALSE,
              legend.outside = TRUE) 
    # tmap_options(check.and.fix = TRUE) 
  
  # tmap_options_reset()
    
tmap_japan

tmap_save(tmap_japan, filename = here::here('04.analyze','synthetic_control',
                                            'add_outcome_predictor',
                                            'mapping', 'japan_map_legend.png'))





