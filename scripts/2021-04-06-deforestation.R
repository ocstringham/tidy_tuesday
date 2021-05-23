library(tidyverse)
# library(broom)
library(tidylog)
library(leaflet)
library(leaflet.extras)
# library(mapview)
# library(htmltools)
library(htmlwidgets)
library(rnaturalearth)
library(scales)
library(sf)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-06/readme.md

# load in data
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
# forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
# brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
# soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
# vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


# filter df and get yearly average forest change
forest %>% distinct(year)
df = 
  forest %>% 
  group_by(code, entity) %>% 
  mutate(n = n()) %>% 
  summarise(estimate = mean(net_forest_conversion)/(n*5)) %>% # times 5 bc in 5 year intervals
  ungroup() %>% 
  distinct()

# get first year of data for label in map
first_year = 
  forest %>% 
  group_by(code, entity) %>% 
  slice_min(year) %>% 
  select(code, year)


# load in world shapefile
world = ne_countries(scale = 110, returnclass = "sf")

# 
# ggplot() +
#   geom_sf(data = world)

# join in geo data and first year
df2 = 
  world %>% 
  select(geounit, admin, gu_a3) %>% 
  left_join(df, by = c('gu_a3' = 'code')) %>% 
  filter(!admin %in% c("Antarctica", "Fiji")) %>% 
  st_set_crs(st_crs(54030)) %>% 
  left_join(first_year)

# # see which ones didn't join
# df2b = 
#   df %>% 
#   anti_join(world, by = c('code' = 'gu_a3'))



# to leaflet


## color ramp

# hist(df2$estimate)
# 
# ## Create a continuous palette function
# pal <- colorNumeric(
#   palette = "RdYlGn",
#   domain = df2$estimate)

## discrete
pal <- colorBin("RdYlGn", df2$estimate, 8, pretty = TRUE)



# ## title
# tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title { 
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px; 
#     padding-right: 10px; 
#     background: rgba(255,255,255,0);
#     font-family: Helvetica;
#     font-weight: bold;
#     font-size: 26px;
#   }
# "))
# 
# title <- tags$div(
#   tag.map.title, HTML("Change in Forested Land 2010")
# )  


## create label ... have to use map and leave as list to work with HTML
df2$label = 
  pmap(list(df2$geounit, df2$estimate, df2$year),
       function(x,y,z){
         if(is.na(y)){
           HTML(paste0("<div class='map_tooltip'>",
                       "<b>", x, "</b>",
                       "<br>",
                       "No data",
                       "</div>"))
           
         }else if(y > 0){
           HTML(paste0("<div class='map_tooltip'>",
                       "<b>", x, "</b>",
                       "<br>",
                       "Rate of forestation: ", comma(y), 
                       " heactares per year",
                       "<br>",
                       "Since ", z,
                       "</div>"))
         }else if(y <=0){
           HTML(paste0( "<div class='map_tooltip'>",
                       "<b>", x, "</b>",
                       "<br>",
                       "Rate of deforestation: ", comma(y), 
                       " hectares per year",
                       "<br>",
                       "Since ", z,
                       "</div>"))
         }
         
       })

## define projection
world_robinson = leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:54030",
                            proj4def = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs",
                            resolutions = 2.028^(16:12), # 8192 down to 0.5
                            origin = c(0, 0)
)


## map
m = 
  df2 %>% 
    leaflet(options = leafletOptions(crs = world_robinson)) %>% 
    addPolygons(fillColor = ~pal(estimate),
                label= ~label,
                color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5) %>% 
    setView(0, 10, zoom = 1) %>% 
  setMapWidgetStyle(list(background= "transparent")) %>% 
    addLegend("bottomleft", pal = pal, values = ~estimate,
              title = "Avg. (De)Forestation Rate<br>[Hectares]<br>",
              labFormat = labelFormat(between = " to "),
              na.label = "No data",
              opacity = 1
    ) 

m

saveWidget(widget = m, 
           file="plots/deforestation.html", 
           # libdir = "plots/",
           title = "Average Deforestation Rate",
           selfcontained = TRUE)

