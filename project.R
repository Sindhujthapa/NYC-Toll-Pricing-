library(tidyverse)
library(lubridate)
library(janitor)

setwd("/Users/Vasco/Desktop/University Things/DynamicPricing/FinalProject")
df = read_csv("MTA_Bridges_and_Tunnels_Hourly_Crossings__Beginning_2019_20250421.csv")
df <- clean_names(df)
df <- df |> mutate(date = as.Date(date, "%m/%d/%Y"),
                   transit_timestamp = gsub(" ", "", transit_timestamp),
                   transit_timestamp = as.POSIXct(transit_timestamp, format = "%m/%d/%Y%I:%M:%S%p"))

df |> arrange(date)

################################################

df = read_csv("crossings.csv")
df <- clean_names(df)
df <- df |> mutate(date = as.Date(date, "%m/%d/%Y"),
                   transit_timestamp = gsub(" ", "", transit_timestamp),
                   transit_timestamp = as.POSIXct(transit_timestamp, format = "%m/%d/%Y%I:%M:%S%p"))

tolls = read_csv("tollrates.csv")
tolls <- clean_names(tolls)
tolls <- tolls |> mutate(date = as.Date(date, "%m/%d/%Y"))

df <- rename(df, Date = date)
df <- df |> mutate(date = as.Date(ifelse(Date >= ymd(20230806), ymd(20230806), ifelse(Date < ymd(20230806) & Date >= ymd(20210411), ymd(20210411), ifelse(Date < ymd(20210411) & Date >= ymd(20190331), ymd(20190331), ymd(20190330))))))

data <- left_join(df, tolls, by = c("facility", "vehicle_class_description", "payment_method", "date"), relationship = "many-to-one")
data <- data |> select(!vehicle_class_category.y) |> rename(vehicle_class_category = vehicle_class_category.x)

## Henry Hudson -> restrict the vehicle type to those who have toll prices, some vehicle class without toll price go through
data <- data |> filter(!(facility == "Henry Hudson Bridge" & (vehicle_class_category == "Truck" | vehicle_class_category == "Bus"))) 

## Drop the toll by mail buses
data <- data |> filter(!(vehicle_class_category == "Bus" & payment_method == "Tolls by Mail"))

## Collapse motorcycle type to one

data <- rbind(data |> filter(!(vehicle_class_category == "Motorcycle")), data |> group_by(transit_timestamp, facility, direction, vehicle_class_category, payment_method) |> mutate(traffic_count = ifelse(vehicle_class_category == "Motorcycle", sum(traffic_count), traffic_count)) |> ungroup() |> filter(vehicle_class_category == "Motorcycle") |>distinct(transit_timestamp, facility, direction, payment_method, vehicle_class_category, .keep_all = TRUE) |> mutate(toll = ifelse(vehicle_class_description == "motorcycle + 1 axle" & date == ymd(20210411) & payment_method == "E-ZPass", toll - 1.7,toll), toll = ifelse(vehicle_class_description == "motorcycle + 1 axle" & date == ymd(20210411) & payment_method == "Tolls by Mail", toll - 1.8,toll)) |> mutate(vehicle_class_description = "motorcycle"))

## Bus transformation 2023 -> split after 2023 change by historical average

bus_hourly_props <- data %>%
  filter(vehicle_class_category == "Bus",
         Date >= as.Date("2023-07-01"),
         Date <= as.Date("2023-08-05")) %>%
  mutate(dow = wday(Date), hour = hour(transit_timestamp)) %>%
  group_by(dow, hour, vehicle_class_description, facility, direction, payment_method) %>%
  summarise(total_count = sum(traffic_count)) %>%
  ungroup() %>%
  group_by(dow, hour, facility, direction, payment_method) %>%
  mutate(proportion = total_count / sum(total_count)) %>%
  ungroup()

bus_2023_post <- data %>%
  filter(vehicle_class_category == "Bus",
         Date >= as.Date("2023-08-06")) %>%
  mutate(dow = wday(Date), hour = hour(transit_timestamp))

bus_2023_split <- bus_2023_post %>%
  left_join(bus_hourly_props, by = c("dow", "hour", "facility", "direction", "payment_method"), relationship = "many-to-many") |> 
  mutate(total_count = round(traffic_count * proportion)) |>
  select(transit_timestamp, Date, hour, facility_id, facility, direction, payment_method, vehicle_class, vehicle_class_description.y, vehicle_class_category, total_count, date, toll) |>
  rename(vehicle_class_description = vehicle_class_description.y, traffic_count = total_count)

data <- rbind(data |> filter(!(vehicle_class_category == "Bus" & Date >= as.Date("2023-08-06"))), bus_2023_split)

# Generates NAs, need more data (NA = days and hours that don't appear before price change but appear after)
#data |> filter(vehicle_class_category == "Bus", year(Date) == 2023) |> ggplot(aes(x= Date, fill=vehicle_class_description)) + geom_bar(position = "fill", width = 1)
#data |> filter(Date >= as.Date("2023-08-06"), vehicle_class_category == "Bus", is.na(vehicle_class_description)) |> mutate(dow = wday(Date)) |> group_by(facility, direction, dow, hour) |> summarize(n = n()) |> view()

## Keep the relevant vehicles
data <- data |> filter(vehicle_class_description == "2-axle passenger car" | vehicle_class_description == "motorcycle" | vehicle_class_description == "2-axle franchise bus" | vehicle_class_description == "3-axle franchise bus" | vehicle_class_description == "2-axle truck" | vehicle_class_description == "3-axle truck" | vehicle_class_description == "4-axle truck" | vehicle_class_description == "5-axle truck") 

## Problematic tunnles for car 2023: "Verrazano Narrows Bridge", "Throgs Neck Bridge", "Triborough Bronx Bridge"
data <- data |> filter(!(facility == "Verrazano Narrows Bridge" | facility == "Throgs Neck Bridge" | facility == "Triborough Bronx Bridge"))


data |> filter(vehicle_class_category == "Car", year(Date) == 2023) |> ggplot(aes(x= Date, fill=vehicle_class_description)) + geom_bar(position = "fill", width = 1)
data |> filter(vehicle_class_category == "Truck", year(Date) == 2023) |> ggplot(aes(x= Date, fill=vehicle_class_description)) + geom_bar(position = "fill", width = 1)
data |> filter(vehicle_class_category == "Motorcycle", year(Date) == 2023) |> ggplot(aes(x= Date, fill=vehicle_class_description)) + geom_bar(position = "fill", width = 1)
data |> filter(vehicle_class_category == "Bus", year(Date) == 2023) |> ggplot(aes(x= Date, fill=vehicle_class_description)) + geom_bar(position = "fill", width = 1)

## Some hours of the day don't have data (data for 1am but not for 2am before and after the price change)

data |> filter(year(Date) == 2021) |> mutate(dow = wday(Date)) |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour, date) |> 
  summarise(Q = mean(traffic_count), P = mean(toll)) |> ungroup() |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour) |> 
  mutate(n = n()) |> filter(n == 2) |> ungroup() |>
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour) |>
  mutate(e = ((Q-lag(Q))/lag(Q))/((P - lag(P))/lag(P)), dow = as.factor(dow)) |>
  filter(facility == "Triborough Manhattan Bridge", payment_method == "E-ZPass") |>
  ggplot(aes(x = hour, y = e, color = vehicle_class_description)) + geom_point() + geom_smooth(method=lm, se = F) + facet_wrap("dow") + theme(legend.position="none")


data |> filter(year(Date) == 2021) |> mutate(dow = wday(Date)) |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour, date) |> 
  summarise(Q = mean(traffic_count), P = mean(toll)) |> ungroup() |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour) |> 
  mutate(n = n()) |> filter(n == 2) |> ungroup() |>
  arrange(vehicle_class_description, payment_method, facility, direction, dow, hour, date) |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, hour) |>
  mutate(e = ((Q-lag(Q))/lag(Q))/((P - lag(P))/lag(P)), dow = as.factor(dow)) |>
  filter(facility == "Triborough Manhattan Bridge", payment_method == "Tolls by Mail") |>
  ggplot(aes(x = hour, y = e, color = vehicle_class_description)) + geom_point() + geom_smooth(method=lm, se = F) + facet_wrap("dow") + theme(legend.position="none")



data |> filter(year(Date) == 2019) |> mutate(dow = wday(Date)) |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow, date) |> 
  summarise(Q = mean(traffic_count), P = mean(toll)) |> ungroup() |> 
  group_by(vehicle_class_description, payment_method, facility, direction, dow) |> 
  mutate(n = n()) |> filter(n == 2) |> ungroup() |>
  group_by(vehicle_class_description, payment_method, facility, direction, dow) |>
  mutate(e = ((Q-lag(Q))/lag(Q))/((P - lag(P))/lag(P))) |> 
  filter(payment_method == "E-ZPass") |>
  ggplot(aes(x = dow , y = e, color = vehicle_class_description)) + geom_point() + geom_smooth(method=lm, se = F) + facet_wrap("facility") + theme(legend.position="none")


## Verrazano Update 2020 direction
## Problematic tunnles for car 2023: "Verrazano Narrows Bridge", "Throgs Neck Bridge", "Triborough Bronx Bridge"

