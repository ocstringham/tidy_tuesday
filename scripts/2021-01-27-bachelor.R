library(dplyr)
library(ggplot2)
library(ggridges)
library(scales)
library(ggiraph)
library(glue)
library(htmlwidgets)


# load data from https://data.world/aerispaha/the-bachelor-contestants/workspace/file?filename=historical_bachelor_contestants.csv
df = read.csv('data/historical_bachelor_contestants.csv')


# get median age per year
df 

  
# ridge
df %>% 
  ggplot(aes(x = Age, y = as.factor(Season))) + 
  geom_density_ridges(scale = 6,
                      quantile_lines = TRUE, quantiles = 2)

# boxplots
df %>% 
  ggplot(aes(x = Age, y = as.factor(Season))) + 
  geom_boxplot()

# dot
p = 
df %>% 
  group_by(Season) %>% 
  summarise(mean_age = mean(Age, na.rm = TRUE),
            sd = sd(Age, na.rm = TRUE),
            lower = quantile(Age, probs = 0.025, na.rm = TRUE),
            upper = quantile(Age, probs = 0.975, na.rm = TRUE)) %>% 
  mutate(hover = glue("Season: {Season}\nMean Age: {round(mean_age,1)}")) %>% 
  ggplot(aes(y = mean_age, x = Season)) +
  geom_pointrange_interactive(aes(ymin = lower, ymax = upper,
                                  data_id = mean_age, tooltip = hover)) +
  # geom_pointrange(aes(ymin = lower, ymax = upper)) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  scale_y_continuous(breaks = scales::breaks_width(4)) +
  labs(y = "Age") +
  # coord_flip() + 
  theme_bw()

p

x = girafe(code = print(p))
x

saveWidget(x, file = 'plots/bachelor_age.html', selfcontained = TRUE)

# save html

# regression
df %>% 
  ggplot(aes(x = Season, y = Age)) +
  geom_jitter() +
  geom_smooth(method='lm', formula= y~x, se = FALSE)

# sd over time
df %>% 
  group_by(Season) %>% 
  summarise(sd = sd(Age, na.rm = TRUE),
            mean = mean(Age, na.rm = TRUE),
            cv = sd/mean) %>% 
  ggplot(aes(x = Season, y = cv)) +
  geom_point() +
  # geom_smooth(method='lm', formula= y~x) +
  NULL
  # geom_line()

df %>% 
  group_by(Season) %>% 
  summarise(sd = sd(Age, na.rm = TRUE),
            mean = mean(Age, na.rm = TRUE),
            cv = sd/mean) %>% 
  lm(formula = cv ~ Season) %>% 
  summary()

df %>% 
  lm(formula = Age ~ Season) %>% 
  summary()

df %>% 
  lm(formula = Age ~ factor(Season) + 0) %>% 
  summary()


# week elimnated vs age




