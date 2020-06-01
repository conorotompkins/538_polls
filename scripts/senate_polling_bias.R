library(tidyverse)
library(janitor)

theme_set(theme_bw())

df <- list.files("data/senate_polls_error", full.names = TRUE, recursive = TRUE) %>% 
  set_names() %>% 
  map_dfr(read_tsv, col_names = FALSE, .id = "year") %>% 
  mutate(year = str_remove(year, "^data/senate_polls_error/538_polling_error/"),
         year = str_remove(year, ".txt$")) %>% 
  rename(state = X1, 
         number_of_polls = X2,
         weighted_avg_error = X3,
         weighted_avg_statistical_bias = X4) %>% 
  separate(weighted_avg_statistical_bias, sep = "\\+", into = c("party", "weighted_avg_statistical_bias")) %>% 
  mutate(weighted_avg_statistical_bias = as.numeric(weighted_avg_statistical_bias),
         election_cycle = dense_rank(year)) %>% 
  mutate(weighted_avg_statistical_bias = case_when(party == "R" ~ weighted_avg_statistical_bias * -1,
                                                   party == "D" ~ weighted_avg_statistical_bias)) %>%
  mutate(year = str_replace(year, "_", "-")) %>% 
  select(election_cycle, year, everything())


df

unique(df$year)

axis_labels <- df %>% distinct(year) %>% pull(year) %>% unlist()

plot_1 <- df %>% 
  ggplot(aes(election_cycle, weighted_avg_error, size = number_of_polls)) +
  geom_jitter(alpha = .25, width = .1) +
  geom_smooth(group = 1, color = "black", linetype = 2, se = FALSE, show.legend = FALSE) +
  scale_size_continuous(range = c(2, 5)) +
  scale_x_continuous(breaks = c(1:11),
                     labels = axis_labels) +
  #coord_flip() +
  labs(title = "FiveThirtyEight U.S. Senate polling bias",
       subtitle = "Data and original analysis from FiveThirtyEight and Nathaniel Rakich \nhttps://fivethirtyeight.com/features/how-accurate-have-senate-polls-been-and-what-could-that-mean-for-november/",
       x = "Election cycle",
       y = "Weighted average error",
       size = "Number of polls",
       caption = "@conor_tompkins")

ggsave(filename = "images/senate_polling_bias.png", plot = plot_1, width = 12, height = 8)

plot_2 <- df %>% 
  ggplot(aes(election_cycle, weighted_avg_statistical_bias, size = number_of_polls)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 0, ymax = Inf,
           fill = "blue", alpha = .2) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 0, ymax = -Inf,
           fill = "red", alpha = .2) +
  geom_hline(yintercept = 0, linetype = 1, size = .1) +
  geom_jitter(alpha = .3, width = .1) +
  geom_smooth(group = 1, se = FALSE, color = "black", linetype = 2, show.legend = FALSE) +
  scale_size_continuous(range = c(1, 4)) +
  scale_x_continuous(breaks = c(1:11),
                     labels = axis_labels) +
  #coord_flip() +
  labs(title = "FiveThirtyEight U.S. Senate polling bias direction",
       subtitle = "Data and original analysis from FiveThirtyEight and Nathaniel Rakich \nhttps://fivethirtyeight.com/features/how-accurate-have-senate-polls-been-and-what-could-that-mean-for-november/",
       x = "Election cycle",
       y = "Weighted average statistical bias",
       size = "Number of polls")

ggsave(filename = "images/senate_polling_bias_direction.png", plot = plot_2, width = 12, height = 8)
