library(tidyverse)
library(lubridate)
library(scales)

URL <- "https://projects.fivethirtyeight.com/trump-approval-data/approval_polllist.csv"

theme_set(theme_bw())

set.seed(1234)

my_data <- read_csv(URL) %>% 
  mutate(startdate = mdy(startdate),
         enddate = mdy(enddate))

write_csv(my_data, paste("538_polls", Sys.Date()), ".csv")

pollsters <- my_data %>% 
  count(pollster, sort = TRUE) %>% 
  select(pollster) %>% 
  unlist()

my_data %>% 
  mutate(pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(pollster, weight)) +
  geom_jitter(alpha = .25) +
  coord_flip()

df <- my_data %>% 
  select(president, startdate, enddate, approve, disapprove, pollster, weight) %>% 
  gather(key = poll_answer, value = poll_value, -c(president, startdate, enddate, weight, pollster))
  
group.colors <- c(approve = "#03bfc4", disapprove = "#f88179")

df %>% 
  mutate(poll_value = poll_value / 100, 
         pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(startdate, poll_value, color = poll_answer, fill = poll_answer)) +
  geom_hline(yintercept = .50) +
  geom_segment(aes(x = startdate, xend = enddate, y = poll_value, yend = poll_value, alpha = weight)) +
  geom_smooth(aes(weight = weight)) +
  #geom_smooth(linetype = 2, se = FALSE) + #unweighted geom_smooth
  facet_wrap(~pollster) +
  scale_color_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_fill_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_x_date(date_breaks = "month", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format()) +
  guides(alpha = FALSE,
         color = guide_legend(title = "Poll Response"),
         fill = guide_legend(title = "Poll Response")) +
  labs(title = "Donald Trump Approve Ratings",
       x = NULL,
       y = NULL)
ggsave(paste0("images/", "Donald Trump Approval Ratings ", Sys.Date(), ".png"), width = 12, height = 9)
