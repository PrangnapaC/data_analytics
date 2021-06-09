library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(stats)

#read and select data
dailyActivity <- read_csv("dailyActivity_merged.csv")
#View(dailyActivity)
#str(dailyActivity)
#head(dailyActivity)

#clean column names
cleaned_daily_activity <- clean_names(dailyActivity)
#str(cleaned_daily_activity)
#glimpse(cleaned_daily_activity)
#summary(cleaned_daily_activity)

# create new df
daily_activity_minutes = select(cleaned_daily_activity ,id, activity_date,
                                very_active_minutes,fairly_active_minutes,
                                lightly_active_minutes, sedentary_minutes)

daily_activity_minutes$total_minutes = daily_activity_minutes$very_active_minutes + 
  daily_activity_minutes$fairly_active_minutes + 
  daily_activity_minutes$lightly_active_minutes + 
  daily_activity_minutes$sedentary_minutes

daily_activity_minutes$total_hrs = daily_activity_minutes$total_minutes/60

#summary(daily_activity_minutes)

# Classify each user as either light, medium, or heavy based on their activity
daily_activity_minutes$user = case_when(daily_activity_minutes$very_active_minutes >= 30 &
                                          daily_activity_minutes$fairly_active_minutes >= 30 ~ "heavy",
                                        daily_activity_minutes$very_active_minutes >= 15 ~ "moderate",
                                        TRUE ~ "light")

daily_activity_minutes$user <- factor(daily_activity_minutes$user,
                                      levels = c("light" , "moderate", "heavy"))

# univariate plot
ggplot(data = daily_activity_minutes) +
  geom_bar(mapping = aes(x = user, fill = user)) +
  ylab("n") +
  xlab("Type of users") +
  ggtitle("Types of users") +
  theme(text = element_text(size = 15),
        legend.title = element_text("Users"))

daily_activity_minutes$calories = cleaned_daily_activity$calories
daily_activity_minutes$total_steps = cleaned_daily_activity$total_steps

# Relationship between user type and calories burned
ggplot(data = daily_activity_minutes) +
  geom_boxplot(mapping = aes(x = user, y = calories, fill = user)) +
  ylab("Calories burned") +
  xlab("Type of users") +
  ggtitle("Number of calories burned by type of users") +
  theme(text = element_text(size = 15),
        legend.title = element_text("Users"))

#read and select sleep data
sleepDay <- read_csv("sleepDay_merged.csv")
cleaned_sleep_day <- clean_names(sleepDay)
#str(cleaned_sleep_day)

sleep_edit <- cleaned_sleep_day %>%
  separate(sleep_day, c("date","time")," ")

#left join on id and date
activity_sleep <- left_join(daily_activity_minutes, sleep_edit, 
                            by = c("id"="id", "activity_date" = "date"))

#summary(activity_sleep)

# Relationship between user type and sleep
ggplot(data = activity_sleep) +
  geom_boxplot(mapping = aes(x = user, y = total_minutes_asleep, fill = user)) +
  ylab("Total sleep time") +
  xlab("Type of users") +
  ggtitle("Sleep habit by type of users") +
  theme(text = element_text(size = 15),
        legend.title = element_text("Users"))

# univariate plots
activity_sleep %>% 
  summarise(id , calories, user) %>% 
  ggplot(aes(calories,fill=user)) + 
  geom_histogram(bins=50) +
  xlab("Calories burned") +
  ylab("n") +
  ggtitle("Calories burned")

activity_sleep$total_hrs_asleep = activity_sleep$total_minutes_asleep/60

activity_sleep %>% 
  summarise(id , total_hrs_asleep, user) %>% 
  ggplot(aes(total_hrs_asleep)) + 
  geom_histogram(aes(fill=..count..),bins=50) +
  scale_x_continuous(breaks = seq(1, 30, by = 1)) +
  xlab("Sleep hours") +
  ylab("n") +
  ggtitle("Sleep hours")

#summary(activity_sleep)

#classified sleep habit
activity_sleep$sleep = case_when(activity_sleep$total_hrs_asleep >= 9 ~ "oversleep",
                                 activity_sleep$total_hrs_asleep < 9 &
                                   activity_sleep$total_hrs_asleep >= 6 ~ "enough sleep",
                                 is.na(activity_sleep$total_hrs_asleep) ~ "NA",
                                 TRUE ~ "not enough sleep")

activity_sleep %>%
  group_by(sleep) %>%
  count(is.na(sleep))

#sleep bivariate plot
ggplot(subset(activity_sleep, sleep %in% c("enough sleep", "not enough sleep", "oversleep"))) +
  geom_bar(mapping = aes(x = sleep, fill = user), position = "dodge") +
  ylab("n") +
  xlab("Sleep habit") +
  ggtitle("Sleep habit by type of users") +
  theme(text = element_text(size = 15),
        legend.title = element_text("Users"))

ggplot(subset(activity_sleep, sleep %in% c("enough sleep", "not enough sleep", "oversleep"))) +
  geom_bar(mapping = aes(x = sleep, fill = user)) +
  ylab("n") +
  xlab("Sleep habit") +
  theme(text = element_text(size = 15),
        legend.title = element_text("Users"))
