library(dplyr)
library(ggplot2)
library(ggwordcloud)
library(stringr)
library(tidytext)
library(forcats)

# load data
df = readRDS('data/hike_data.rds')

# get region
df = 
  df %>% 
  mutate(region = ifelse( str_detect(location, '--'), 
                          str_extract(location, '.+(?=--)'), location))

test = df %>% group_by(region) %>% summarise(n = n()) %>% arrange(-n)


# to grams
df2 = 
  df %>% 
    group_by(region) %>% 
    filter(n() > 100) %>% # only get most represented regions
    unnest_tokens(word, description) %>% 
  group_by(region, word) %>% 
  summarise(n = n())



# remove stop words
data("stop_words")

df3 =
  df2 %>%
  anti_join(stop_words)
  
  
# get size by proportional number of words per region
df4 = 
  df3 %>% 
  group_by(region) %>% 
  mutate(n_prop = (n/sum(n)) * 100 )


# plot
df4 %>% 
  group_by(region) %>% 
  top_n(n = 12, wt = n_prop) %>% # top 10 words per region
  ggplot(aes(label = word, size = n_prop)) +
  geom_text_wordcloud(eccentricity = 1) +
  # geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) +
  facet_wrap(~region, nrow = 3) +
  # theme_minimal() +
  labs()
  NULL

ggsave(filename = 'plots/2020-11-25-washington-hiking1.png', dpi = 300, plot = last_plot())

# explore tf idf

df5 = 
  df4 %>% 
  bind_tf_idf(word, region, n)



# plot
df5 %>%
  ungroup() %>%
  group_by(region) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~region, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)

ggsave(filename = 'plots/2020-11-25-washington-hiking2.png', dpi = 300, plot = last_plot())
