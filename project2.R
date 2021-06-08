library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)
library(riem)
library(weathermetrics)
library(magrittr)
library(comf)
library(extrafont)
library(ggrepel)

#read and select data
thai_airports <- read_xlsx("thai_airports.xlsx")

#create new df from riem weather data
summer_weather <- map_df(thai_airports$ICAO, riem_measures,
                         date_start = "2020-03-01",
                         date_end = "2020-06-30")

cool_weather <- map_df(thai_airports$ICAO, riem_measures,
                       date_start = "2020-11-01",
                       date_end = "2021-02-28")

#filter na
summer_weather <- filter(summer_weather,
                         !is.na(feel))

cool_weather <- filter(cool_weather,
                       !is.na(feel))

#adding celsius columns
summer_weather <- mutate(summer_weather,
                           feelc = convert_temperature(feel,
                                                       old_metric = "f", new_metric = "c")) 

cool_weather <- mutate(cool_weather,
                         feelc = convert_temperature(feel,
                                                     old_metric = "f", new_metric = "c"))

#creating new df
summer_values <- summer_weather %>%
  group_by(station) %>%
  summarise(summer_feel = mean(feelc, na.rm = TRUE))

cool_values <- cool_weather %>%
  group_by(station) %>%
  summarise(cool_feel = mean(feelc, na.rm = TRUE))

#left join
climates <- left_join(cool_values, summer_values,
                        by = "station")

climates <- left_join(climates, thai_airports,
                        by = c("station" = "ICAO"))

##bivariate plot
ggplot(climates,
       aes(summer_feel, cool_feel, color=summer_feel)) +
  scale_color_gradient(low="royalblue", high="red", name ="Temperature") +
  geom_point(shape = 19, size = 2) +
  geom_text_repel(aes(label = City, fontface = "bold"),
                  max.iter = 50000, max.overlaps = 100) +
  ggtitle("Places and season to visit based on your weather preferences",
          subtitle = "In Celsius (C)") +
  labs(caption = "Data source: Airports weather stations in Thailand, 2020-2021") +
  xlab("Summer season apparent temperature")+
  ylab("Cool season apparent temperature") +
  theme_gray() +
  theme(text = element_text(size = 15),
        plot.title = element_text(size = 21,hjust=0.35, vjust = -0.3, face = "bold"),
        plot.subtitle=element_text(size=12, face="italic", color="gray40", vjust = -0.5),
        plot.caption = element_text(color = "gray40", face = "italic"),
        legend.title = element_text(size = 10, face = "bold", color = "gray40"))

# Humidity
#filtering na for new dfs
cool_weather_2 <- filter(cool_weather,
                         !is.na(relh))

summer_weather_2 <- filter(summer_weather,
                           !is.na(relh))

#creating new dfs for relh value
cool_values_2 <- cool_weather_2 %>%
  group_by(station) %>%
  summarise(cool_relh = mean(relh, na.rm = TRUE))

summer_values_2 <- summer_weather %>%
  group_by(station) %>%
  summarise(summer_relh = mean(relh, na.rm = TRUE))

#left join
humidity <- left_join(cool_values_2, summer_values_2,
                      by = "station")

humidity <- left_join(humidity, thai_airports,
                      by = c("station" = "ICAO"))

#plot
ggplot(humidity,
       aes(summer_relh, cool_relh, color = summer_relh + cool_relh)) +
  geom_point(shape = 19, size = 2) +
  scale_color_gradient(low="darkorange", high="royalblue", name ="Humidity") +
  geom_text_repel(aes(label = City, fontface = "bold"),
                  max.iter = 50000) +
  ggtitle("Most humid cities during non-rainy seasons in Thailand",
          subtitle = "In percentage (%)") +
  labs(caption = "Data source: Airports weather stations in Thailand, 2020-2021") +
  xlab("Relative humidity in summer season") +
  ylab("Relative humidity in cool season") +
  theme(text = element_text(size = 15),
        plot.title = element_text(size = 21, vjust = -0.3, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic", color = "gray40", vjust = -0.5),
        plot.caption = element_text(color = "gray40", face = "italic"),
        legend.text = element_blank(),
        legend.title = element_text(size = 10, face = "bold", color = "gray40"))
