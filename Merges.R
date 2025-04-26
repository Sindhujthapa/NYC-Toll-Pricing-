
## Easy merges (data includes seasonality EXCEPT VEH_MAINTENANCE)

setwd("C:/Users/Vasco/Desktop/University Things/DynamicPricing/FinalProject")
library(tidyverse)

unemployment <- read_delim("unemployment_NY_state.csv", delim = "\t",locale = locale(encoding = "UTF-16"))
unemployment <- unemployment |> mutate(unemployment_rate = Unemployed/`Labor Force`) |> select(Year, Month, unemployment_rate)

remote_work <- read_csv("NY_remote_work_share.csv")
remote_work <- remote_work |> select(Year, Month, Percent) |> mutate(Percent = Percent/100)
remote_work <- remote_work |> mutate(Month = month.name[match(Month, month.abb)]) |> rename(remote_wk = Percent)

cpi <- read_delim("CPI_NY.txt") 
cpi <- cpi |> select(Year, Period, Value) |> filter(!str_detect(Period, "M13|S01|S02")) |> mutate(Month = as.integer(str_remove(Period, "M")), Month = month.name[Month]) |> select(Year,Month, Value) |> mutate(month_inflation = round(Value/lag(Value) - 1, 4))
cpi <- cpi |> filter(Year > 2018)
cpi <- cpi |> rename(value_cpi = Value)

cpi_vehicles <- read_delim("CPI-NY_VEH_NEWOLD.txt")
cpi_vehicles <- cpi_vehicles |> select(Year, Period, Value) |> filter(!str_detect(Period, "M13|S01|S02")) |> mutate(Month = as.integer(str_remove(Period, "M")), Month = month.name[Month]) |> select(Year,Month, Value) |> mutate(month_inf_veh = round(Value/lag(Value) - 1, 4))
cpi_vehicles <- cpi_vehicles |> filter(Year > 2018)
cpi_vehicles <- cpi_vehicles |> rename(value_cpi_veh = Value)

veh_maintenance <- read_csv("CUSR0000SETD.csv")
veh_maintenance <- veh_maintenance |> rename(date = observation_date) |> filter(year(date) > 2017) |> mutate(maint_inflation = round(CUSR0000SETD/lag(CUSR0000SETD) - 1, 4), Year = year(date), Month = month(date), Month = month.name[Month]) |> select(Year, Month, maint_inflation)

travel <- read_csv("PANYNJ_Airport_Traffic_Data.csv")
travel <- travel |> filter(str_detect(Market, "International")) |> group_by(`Activity Period`) |> summarise(int_passenger = sum(`Revenue Passenger Volume`)) 
travel <- travel |> mutate(Year = year(`Activity Period`), Month = month(`Activity Period`), Month = month.name[Month]) |> select(Year, Month, int_passenger) |> group_by(Year, Month) |> summarise(int_passenger = sum(int_passenger)) |> filter(Year > 2018)

## Work with weekly variables

gasoline <- read_csv("NYC_Weekly_Gas.csv")
glimpse(gasoline)
gasoline <- gasoline |> rename(date = `Date EffectiveActivate to sort column ascending or descending`,
                               fuel_type = `FUEL_TYPEActivate to sort column ascending or descending`,
                               fuel_price = `Fuel Price ($/gal)Activate to sort column ascending or descending`)
gasoline <- gasoline |> select(date, fuel_type, fuel_price) 
gasoline <- gasoline |> mutate(date = as.Date(date, "%d/%m/%Y"), Year = year(date), Month = month(date), Month = month.name[Month])

## Work with hourly variables
weather <- read_csv("nyc_hourly_weather2_to_now.csv")
weather <- weather |> select(time, temp, prcp, coco)
weather <- weather |> mutate(prcp = ifelse(is.na(prcp), 0, prcp),
                             date = as.Date(time))
weather <- weather |> mutate(date = ceiling_date(date, unit = "week") + days(1))

## Merge

controls <- left_join(cpi, cpi_vehicles)
controls <- left_join(controls, remote_work)
controls <- left_join(controls, unemployment)
controls <- left_join(controls, veh_maintenance)
controls <- left_join(controls, travel)
controls <- left_join(gasoline, controls) |> select(!c(Year, Month))
controls <- left_join(weather, controls, relationship = "many-to-many")

## Registration

license <- read_csv("Vehicle__Snowmobile__and_Boat_Registrations_20250421.csv")
license |> pull(State) |> unique()

## maybe loop through every month, then add the 1, and you have the count for one month
## can also daily, but might take to much time

df <- data.frame(date = as.Date(c("2022-01-01", "2022-03-15", 
                                  "2022-07-10", "2022-11-30")),
                 value = c(10, 20, 30, 40))
# Define start and end dates
start_date <- as.Date("2022-02-01")
end_date <- as.Date("2022-10-01")
# Create a new column indicating if date falls between start_date and end_date
df$between <- ifelse(df$date >= start_date & df$date <= end_date, 1, 0)
