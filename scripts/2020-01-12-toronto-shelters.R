library(dplyr)
# library(ggplot2)
library(stringr)
library(tidygeocoder)
library(lubridate)
library(leaflet)



# load in data
df = read.csv('data/shelters.csv')


# get # of unique shelters & add unique ID
shelters = 
  df %>% 
  select(shelter_name, shelter_address, shelter_city, shelter_postal_code, shelter_province) %>% 
  distinct() %>% 
  arrange(shelter_name) %>% 
  mutate(shelter_id = row_number())


# add back in shelter id & Parse time
df2 = df %>% left_join(shelters) %>% 
  mutate(date = as.POSIXct(occupancy_date))


# names and addresses
shelter_addrs =
  shelters %>%
  group_by(shelter_name, shelter_id) %>%
  summarise(address = paste0(shelter_address, ", ", shelter_city, " ",
                             # shelter_postal_code, 
                             ", ", shelter_province))

# get lat and longs via tidygeocoder::geocode
geocode_temp = 
  shelter_addrs %>% 
  geocode(address, method = 'osm', lat = latitude , long = longitude)

# parse results
lat_long = 
  tibble(
      address = as.vector(geocode_temp$.tbl[[3]]), 
      lat = as.vector(geocode_temp$results[[1]]), 
      long = as.vector(geocode_temp$results[[2]]))

## add in names and IDs
lat_long = 
  lat_long %>% 
  left_join(shelter_addrs, by = 'address') %>% 
  distinct(shelter_id, .keep_all = TRUE)




# summary data

# # average by month
# sum1 = 
#   df2 %>% 
#   mutate(day = day(date),
#          month = month(date),
#          year = year(date)) %>% 
#   arrange(shelter_id, date) %>% 
#   group_by(shelter_id, year, month, day) %>% 
#   summarise(occupancy = sum(occupancy, na.rm = TRUE), 
#             capacity = sum(capacity, na.rm = TRUE), 
#             n = n()) %>% 
#   ungroup() %>% 
#   group_by(shelter_id, year, month) %>% 
#   summarise(avg_occ = mean(occupancy, na.rm = TRUE),
#             max_cap = max(capacity, na.rm = TRUE)) %>% 
#   arrange(shelter_id, year, month) %>% 
#   mutate(date = ymd(paste(year, month, 1, sep = '-')),
#          perc_occ = avg_occ/max_cap)

# average by week
sum2 = 
  df2 %>% 
  mutate(day = day(date),
         week = week(date),
         month = month(date),
         year = year(date)) %>% 
  arrange(shelter_id, date) %>% 
  group_by(shelter_id, year, week) %>% 
  summarise(occupancy = sum(occupancy, na.rm = TRUE), 
            capacity = sum(capacity, na.rm = TRUE), 
            n = n()) %>% 
  ungroup() %>% 
  mutate(date = as.Date(paste(year, week, 1, sep = '-'), "%Y-%U-%u"),
         perc_occ = occupancy/capacity)



# leaflet map

## summarize for all weeks, create label for map
## also remove rows with no lat/long or occupancy data
sum3 = 
  sum2 %>% 
  group_by(shelter_id) %>% 
  summarise(avg_occ = mean(occupancy, na.rm = TRUE) %>% round(0),
            perc_occ = mean(perc_occ, na.rm = TRUE) %>% round(2) * 100) %>% 
  left_join(lat_long) %>% 
  
  filter(!is.na(lat), avg_occ != 0) %>% 
  
  mutate(label = paste(paste0("<b style='color:blue'>", shelter_name, "</b>"), 
                       # address, 
                       paste0("Average weekly Occupancy: ", avg_occ),
                       paste0("Average weekly Percent Maximum Capacity: ", perc_occ,  "%"),
                       sep = '<br/>'))

## create color palette for map
pal <- colorBin(
  palette = colorRampPalette(c('orange','red' ))(length(sum3$avg_occ)), 
  domain = sum3$avg_occ, bins = 3)


# create map
map1 = 
  leaflet(title = "test") %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(data = sum3, ~long, ~lat, 
                   color = ~pal(avg_occ),
                   radius = ~avg_occ/200, #~perc_occ/10, #~avg_occ/200,
                   label = ~shelter_name, popup = ~ label)
map1

# add Cart tiles and legend
map2 = 
  map1  %>%   
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = (sum3$avg_occ),
            title = "Weekly\nAverage\nOccupancy",
            # labFormat = labelFormat(prefix = "$"),
            opacity = 1) 

map2

htmlwidgets::saveWidget(map2, file="toronto-shelters.html")

