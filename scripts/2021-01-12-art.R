library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)
library(egg)


# load in data
artwork = read_csv("data/artwork.csv")
artists = read_csv("data/artists.csv")

# types of art
art_type = artwork %>% distinct(medium) %>% arrange(medium)


# plot year of creation vs year of acquisition
df = artwork %>% 
  left_join(artists, by = c("artistId" = "id"))

p = 
  df %>% 
  filter(acquisitionYear <= 2010) %>% 
  filter(year <= acquisitionYear) %>% 
  group_by(year, acquisitionYear) %>% 
  summarise(n = n()) %>% 
  rename(`Decade of Acquisition` = acquisitionYear, 
         `Decade of Creation` = year) %>% 
  ggplot(aes(x = `Decade of Acquisition`, y = `Decade of Creation`)) +
  geom_bin2d(binwidth = 10, color = "gray") +
  # geom_hex(binwidth = 10, color = "gray") +
  geom_abline(slope = 1) +
  scale_fill_fermenter(direction = 1, palette = 3) +
  labs(fill = "Number of\nAcquisitions") +
  # theme_bw() +
  # theme_presentation() +
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

p

ggplotly(p)

# Save img and html
ggsave(p, filename = 'plots/art.png')
saveWidget(ggplotly(p), file = "plots/art.html");


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
