library(tidyverse)
library(lubridate)

activity_data <- read.csv('activity.csv')
activity_data$date <- ymd(activity_data$date)

# 1
steps_data <- activity_data %>%
  group_by(date) %>%
  summarize(spd = sum(steps)) %>%
  drop_na()

ggplot(steps_data, aes(x = spd)) +
    geom_histogram(fill = "darkgreen", binwidth = 500) +
    labs(title = "Histogram of steps per day", x = "Steps", y = "Frequency")

mean_spd <- mean(steps_data$spd)
median_spd <- median(steps_data$spd)

# 2
avg_act_pattern <- activity_data %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarize(steps = mean(steps))

ggplot(avg_act_pattern, aes(x=interval, y=steps)) +
  geom_line(color = "blue")

max_intervall <- avg_act_pattern[which.max(avg_act_pattern$steps),]

# 3
no_of_missing <- sum(is.na(activity_data$steps))
clean_data <- activity_data %>%
  mutate(steps=replace(steps, is.na(steps), mean(avg_act_pattern$steps)))
  # mutate_all(replace_na, mean(avg_act_pattern$steps))

steps_clean_data <- clean_data %>%
  group_by(date) %>%
  summarize(spd = sum(steps))

ggplot(steps_clean_data, aes(x = spd)) +
    geom_histogram(fill = "darkgreen", binwidth = 500) +
    labs(title = "Histogram of steps per day", x = "Steps", y = "Frequency")

mean_clean_spd <- mean(steps_clean_data$spd)
median_clean_spd <- median(steps_clean_data$spd)

# 4
clean_data <- mutate(clean_data, daytype = ifelse(weekdays(clean_data$date) == "Saturday" |
                            weekdays(clean_data$date) == "Sunday", "weekend", "weekday"))

avg_act_pattern_clean <- clean_data %>%
  group_by(daytype, interval) %>%
  summarize(steps = mean(steps))

panel_graph <- ggplot(avg_act_pattern_clean, aes(x=interval, y=steps, color=daytype)) +
  geom_line() +
  facet_wrap(~daytype, ncol = 1, nrow = 2)
print(panel_graph)



