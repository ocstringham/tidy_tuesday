library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(leaflet)
library(CoordinateCleaner)


# load data
df = read.csv('data/women.csv', encoding = "UTF-8", stringsAsFactors = F) %>% 
  mutate(across(where(is.character), str_trim)) # trim whitespace

## make one column for each associated country
df = df %>% separate_rows(country, sep = '/')  

# geocode countries

## view unique countries
unique_countries = df %>% distinct(country)

## load in country references
data(countryref)

## prelminary matches

### by name
geo_countries1 = 
  df %>% 
  select(country) %>% 
  distinct() %>% 
  inner_join(countryref, by = c('country' = 'name')) %>% 
  group_by(country) %>% 
  slice_head(1)

### manually do the rest
unique_countries %>% 
  filter(!country %in% geo_countries1$country)

geo_countries2 = 
  tribble( ~country, ~centroid.lon, ~centroid.lat,
           "Worldwide", -39.792959, 29.792110, #gmaps
           "UAE", 54.0000 ,24.000,
           "UK", -2.0, 54.00,
           "Hong Kong", 114.1290, 22.4230,
           "US", -97.0000, 38.000000,
           "Republic of Ireland", -8.000, 53.33333,
           "Exiled Uighur from Ghulja (in Chinese, Yining)", 81.791248, 43.946838, # gmaps
           "Northern Ireland",  -6.808009, 54.745124, #gmaps
           "DR Congo", 24.063806, -3.680396, #gmaps
           "Myanmar", 98.000, 22.000,
           "Wales, UK",  -3.660452, 52.349727 #gmaps
           )

## combine
geo_countries = bind_rows(geo_countries1 %>% select(country, centroid.lon, centroid.lat), 
                          geo_countries2)


# add back in coords to df
df2 = df %>% left_join(geo_countries) %>% select()

# create label for popup
df3 = 
  df2 %>% 
  mutate(label = paste(paste0(
                      paste0('<img src="', img, '"', 'class="pic-center" align="center" style="width:30%;height:30%;"', '</img>'),
                      "<br></br>",
                      "<b class = 'woman-name'>", name, "</b>"), 
                      paste0("<font class = 'country'>", country, "</font>"),
                      paste0("<b class = 'category'>Category: </b>", category),
                      paste0("<b class = 'role'>Role: </b>", role),
                      paste0("<font class = 'description'>", description, "</font>"),
                       sep = '<br/>')) 

# make icons
temp = iconList(makeIcon())
for(i in 1:nrow(df3)) temp[[i]] = makeIcon(df3$img, df3$img, 35, 35)
womenIcons = temp %>% set_names(df3$name)


# create map
map1 = 
  leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(data = df3, ~centroid.lon, ~centroid.lat, 
             # clusterOptions = markerClusterOptions(),
                   clusterOptions =
               markerClusterOptions(spiderfyDistanceMultiplier = 1.25,
                                    freezeAtZoom = 6), #
                   icon = ~womenIcons[name],
                   label = ~name, popup = ~ label)
map1

htmlwidgets::saveWidget(map1, file="women-of-2020.html")
