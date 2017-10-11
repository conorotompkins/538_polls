library(cowplot)

#create main plot
plot_1 <- df %>% 
  mutate(poll_value = poll_value / 100, 
         pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(startdate, poll_value, color = poll_answer, fill = poll_answer)) +
  geom_hline(yintercept = .50, size = .25) +
  geom_segment(aes(x = startdate, xend = enddate, y = poll_value, yend = poll_value, alpha = weight)) +
  geom_smooth(aes(weight = weight)) +
  #geom_vline(xintercept = as.numeric(ymd(Sys.Date())), linetype = 2) +
  #geom_smooth(linetype = 2, se = FALSE) + #unweighted geom_smooth
  scale_color_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_fill_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_alpha_continuous(range = c(.05, .4)) +
  scale_x_date(date_breaks = "week", date_labels = "%d %b") +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(xlim = c(ymd(Sys.Date() - 60), ymd(Sys.Date()))) +
  guides(alpha = FALSE,
         color = guide_legend(title = "Poll Response"),
         fill = guide_legend(title = "Poll Response")) +
  labs(title = "Donald Trump Approval Ratings",
       subtitle = "Past 60 Days",
       x = NULL,
       y = NULL,
       caption = "@conor_tompkins, Data from 538") +
  theme(legend.position = "bottom")

#create inset plot
plot_2 <- df %>% 
  mutate(poll_value = poll_value / 100, 
         pollster = factor(pollster, levels = pollsters)) %>% 
  ggplot(aes(startdate, poll_value, color = poll_answer, fill = poll_answer)) +
  geom_hline(yintercept = .50, size = .25) +
  #geom_segment(aes(x = startdate, xend = enddate, y = poll_value, yend = poll_value, alpha = weight)) +
  geom_smooth(aes(weight = weight)) +
  #geom_vline(xintercept = as.numeric(ymd(Sys.Date())), linetype = 2) +
  #geom_smooth(linetype = 2, se = FALSE) + #unweighted geom_smooth
  scale_color_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  scale_fill_manual(values = group.colors, labels = c("Approve", "Disapprove")) +
  #scale_x_date(date_breaks = "month", date_labels = "%b %Y") +
  scale_y_continuous(labels = percent_format(), 
                     breaks = c(.4, .5, .6)) +
  guides(alpha = FALSE,
         color = FALSE,
         fill = FALSE) +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL,
       y = NULL,
       caption = NULL) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12))

  
ggdraw() + 
  draw_plot(plot_1, 0, 0, 1, 1, 1) +
  draw_plot(plot_2, width = .7, height = 1, scale = .25, x = .55, y = .35)

ggsave("images/inset_approval_ratings_plot.png")
