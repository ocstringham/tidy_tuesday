library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggiraph)
library(scales)
library(htmlwidgets)
library(egg)
library(patchwork)


# load in data
artwork = read_csv("data/artwork.csv")
artists = read_csv("data/artists.csv")

# types of art
art_type = artwork %>% distinct(medium) %>% arrange(medium)

# combine to one df
df = artwork %>% 
  left_join(artists, by = c("artistId" = "id"))

# tile plot of yr created vs yr acquired
p2 = 
  df %>% 
  filter(acquisitionYear < 2010) %>% 
  filter(year <= acquisitionYear) %>% 
  mutate(decade_create = floor(year/10)*10,
         decade_acquired = floor(acquisitionYear/10)*10) %>% 
  group_by(decade_create, decade_acquired) %>% 
  summarise(n = n()) %>% 
  mutate(hover = glue("Decade Created: {decade_create}\n",
                      "Decade Acquired: {decade_acquired}\n",
                      "Acquisitions: {comma(n, accuracy = 1)}")) %>% 
  ggplot(aes(x = decade_acquired, y = decade_create, fill = log10(n))) +
  geom_tile_interactive(color = "gray", aes(tooltip = hover)) +
  geom_abline(slope = 1) +
  scale_fill_fermenter(direction = 1, palette = 3, labels = comma(c(10, 100, 1000), accuracy = 1)) +
  labs(fill = "Number of\nAcquisitions",
       y = "Decade of Creation",
       x = "Decade of Acquisition") +
  theme(
    # plot.title = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = "gray50"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    # legend.justification = c(0, 1),
    # legend.position = "bottom", # c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) + 
  NULL

p2

## test ggiraph
x = girafe(code = print(p2))
x


# line plot per decade
p3 = 
  df %>% 
  filter(acquisitionYear < 2010) %>% 
  filter(year <= acquisitionYear) %>% 
  mutate(decade_acquired = floor(acquisitionYear/10)*10) %>% 
  group_by(decade_acquired) %>% 
  summarise(n = n()) %>% 
  mutate(hover = glue("{decade_acquired}s: {comma(n, accuracy = 1)} acquisitions")) %>% 
  ggplot(aes(x = decade_acquired, y = n)) +
  geom_line_interactive() +
  geom_point_interactive(aes(tooltip = hover)) +
  scale_y_continuous(trans = 'log10', labels = comma)  +
  labs(x = "", y = "Number of\nAcquisitions") +
  theme(
    # plot.title = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = "gray50"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    # legend.justification = c(0, 1),
    # legend.position = "bottom", # c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  ) + 
  NULL


p3



# combine to one plot
p_all = 
p3 / p2 + 
  plot_layout(heights = c(1, 4))

p_all

# to girafe
g = girafe(code = print(p_all))
g


# save as html
saveWidget(g, file = "plots/art.html", selfcontained = TRUE)


# # quick plots
# df %>% 
#   group_by(year, acquisitionYear) %>% 
#   summarise(n = n()) %>% 
#   ggplot(aes(x = year, y = acquisitionYear, fill = n)) +
#   geom_tile()
# 


# plot year of creation vs year of acquisition

# p = 
#   df %>% 
#   filter(acquisitionYear <= 2010) %>% 
#   filter(year <= acquisitionYear) %>% 
#   group_by(year, acquisitionYear) %>% 
#   # summarise(n = n()) %>% 
#   rename(`Decade of Acquisition` = acquisitionYear, 
#          `Decade of Creation` = year) %>% 
#   ggplot(aes(x = `Decade of Acquisition`, y = `Decade of Creation`)) +
#   geom_bin2d(binwidth = 10, color = "gray") +
#   # geom_hex(binwidth = 10, color = "gray") +
#   geom_abline(slope = 1) +
#   scale_fill_fermenter(direction = 1, palette = 3) +
#   labs(fill = "Number of\nAcquisitions") +
#   # theme_bw() +
#   # theme_presentation() +
#   theme(
#     # plot.title = element_text(face = "bold", size = 12),
#     panel.background = element_rect(fill = "white", color = "gray50"),
#     legend.background = element_rect(fill = "white", size = 4, colour = "white"),
#     # legend.justification = c(0, 1),
#     # legend.position = "bottom", # c(0, 1),
#     axis.ticks = element_line(colour = "grey70", size = 0.2),
#     panel.grid.major = element_line(colour = "grey70", size = 0.2),
#     panel.grid.minor = element_blank()
#   ) + 
#   NULL

# p

# ggplotly(p)

# # Save img and html
# ggsave(p, filename = 'plots/art.png')
# saveWidget(ggplotly(p), file = "plots/art.html", selfcontained = TRUE)


# 
# 
# # art plot by year, jitter for each picture
# df %>% 
#   filter(year > 1900) %>% 
#   sample_n(25) %>% 
#   group_by(acquisitionYear, thumbnailUrl) %>% 
#   summarise(x = 1:n()) %>% 
#   ungroup() %>% 
#   group_by(acquisitionYear) %>% 
#   mutate(x = row_number()) %>% 
#   ggplot(aes(x = acquisitionYear, y = x)) +
#   ggimage::geom_image(aes(image = thumbnailUrl), size = 0.15)
#   
