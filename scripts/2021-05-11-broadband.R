library(tidyverse)
library(ggiraph)
library(tidylog)
library(sf)
library(htmlwidgets)

# load data
broadband <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

## create fields for join
broadband = 
  broadband %>% 
  mutate(STATEFP = str_extract(`COUNTY ID`, "^[0-9]{2}"),
         COUNTYFP = str_extract(`COUNTY ID`, "[0-9]{3}$")) %>% 
  distinct(STATEFP, COUNTYFP, .keep_all = TRUE) %>% 
  mutate(`BROADBAND AVAILABILITY PER FCC` = as.numeric(`BROADBAND AVAILABILITY PER FCC`))

# load in county shapefiles from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
county = read_sf('data/cb_2018_us_county_20m/cb_2018_us_county_20m.shp')

# join & make tooltip
df = 
  county %>% 
  select(STATEFP, COUNTYFP, NAME) %>% 
  left_join(broadband) %>% 
  mutate(state_id = as.numeric(STATEFP)) %>% 
  filter(state_id == 22) %>% 
  # filter(state_id <= 56,
  #        state_id != 15 , state_id != 2) %>% 
  mutate(tooltip = paste0(`COUNTY NAME`, "<br>",
                          paste0(`BROADBAND AVAILABILITY PER FCC`*100, "%"), 
                          " people with broadband access")
  ) %>% 
  I()
  


# plot
p = 
  df %>% 
  ggplot(aes(fill = `BROADBAND AVAILABILITY PER FCC`*100)) + 
  geom_sf_interactive(color = 'gray50', size = 0.2, data_id = df$`COUNTY ID`,
                      aes(tooltip = tooltip)) + 
  scale_fill_fermenter(direction = 1, labels = c("25%", "50%", "75%")) + 
  # scale_color_manual_interactive() + 
  labs(fill = "", title = "Acces to Broadband Internet", subtitle = "Louisiana, USA") + 
  theme_void()
p



# to girafe
tooltip_css = paste0("border-radius: 5px;",
                     "border: 2px solid gray;",
                     "background: white;",
                     "padding: 5px;",
                     "text-align: center;",
                     "opactiy: 0.5")
g = girafe(ggobj =  p, 
           options = list(
             opts_tooltip(css = tooltip_css),
             opts_zoom(max = 5),
             opts_hover_inv(css = "opacity:0.5;"),
             opts_hover(css = "stroke:red;stroke-width:2;")
           ) )
g


# save as html
saveWidget(g, file = "plots/broadband.html", selfcontained = TRUE)
