library(tidyverse)
library(lubridate)
library(scales)

URL <- "https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv"

theme_set(theme_bw(base_size = 18))

set.seed(1234)

my_data <- read_csv(URL) %>% 
  mutate(startdate = mdy(startdate),
         enddate = mdy(enddate),
         poll_length = as.numeric(enddate - startdate))

write_csv(my_data, paste("data/538_polls", Sys.Date()), ".csv")

df <- my_data %>% 
  select(president, startdate, enddate, approve, disapprove, pollster, weight, poll_length) %>% 
  gather(key = poll_answer, value = poll_value, -c(president, startdate, enddate, weight, pollster, poll_length))
  
group.colors <- c(approve = "#03bfc4", disapprove = "#f88179")

pollsters <- my_data %>% 
  count(pollster) %>% 
  arrange(desc(n)) %>% 
  select(pollster) %>% 
  unlist()

df %>% 
  mutate(poll_value = poll_value / 100, 
         pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(startdate, poll_value, color = poll_answer, fill = poll_answer)) +
  geom_hline(yintercept = .50, size = .25) +
  geom_segment(aes(x = startdate, xend = enddate, y = poll_value, yend = poll_value, alpha = weight)) +
  geom_smooth(aes(weight = weight)) +
  geom_vline(xintercept = as.numeric(ymd(Sys.Date())), linetype = 2) +
  #geom_smooth(linetype = 2, se = FALSE) + #unweighted geom_smooth
  scale_color_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_fill_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_x_date(date_breaks = "month", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format()) +
  guides(alpha = FALSE,
         color = guide_legend(title = "Poll Response"),
         fill = guide_legend(title = "Poll Response")) +
  labs(title = "Donald Trump Approval Ratings",
       subtitle = "Data from 538",
       x = NULL,
       y = NULL,
       caption = "@conor_tompkins")
ggsave("images/Donald Trump Approval Ratings.png", width = 15, height = 9)

pollsters_top10 <- my_data %>% 
  count(pollster) %>% 
  top_n(20) %>% 
  arrange(desc(n)) %>% 
  select(pollster) %>% 
  unlist()

df %>% 
  filter(pollster %in% pollsters_top10) %>% 
  mutate(poll_value = poll_value / 100, 
         pollster = factor(pollster, levels = pollsters)) %>% 
  group_by(pollster) %>% 
  mutate(weight_mean = mean(weight)) %>% 
  ggplot(aes(startdate, poll_value, color = poll_answer, fill = poll_answer)) +
  geom_hline(yintercept = .50, size = .25) +
  geom_segment(aes(x = startdate, xend = enddate, y = poll_value, yend = poll_value), alpha = .1) +
  geom_smooth(aes(weight = weight, alpha = weight_mean), linetype = 3) +
  geom_vline(xintercept = as.numeric(ymd(Sys.Date())), linetype = 2) +
  #geom_smooth(linetype = 2, se = FALSE) + #unweighted geom_smooth
  facet_wrap(~pollster, ncol = 4) +
  scale_color_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_fill_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  scale_y_continuous(labels = percent_format()) +
  scale_alpha_continuous(range = c(.2, .8)) +
  guides(alpha = FALSE,
         color = guide_legend(title = "Poll Response"),
         fill = guide_legend(title = "Poll Response")) +
  labs(title = "Donald Trump Approval Ratings",
       subtitle = "Data from 538",
       x = NULL,
       y = NULL,
       caption = "@conor_tompkins") +
  theme(panel.grid.minor = element_blank())
ggsave("images/Donald Trump Approval Ratings faceted.png", width = 15, height = 9)

#experimental
df %>% 
  mutate(pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(pollster, weight)) +
  geom_jitter(alpha = .1, width = .25, height = 0) +
  #geom_boxplot() +
  coord_flip()

df %>% 
  ggplot(aes(weight, poll_value, color = poll_answer)) +
  geom_hline(yintercept = 50) +
  geom_point(alpha = .1) +
  geom_smooth() +
  facet_wrap(~poll_answer,
             ncol = 1)

df %>%
  mutate(pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(weight, poll_length)) +
  geom_jitter(alpha = .1, width = 0, height = .25)
  #facet_wrap(~pollster)
  #scale_y_discrete(breaks = c(1:20))