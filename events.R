a = c("a", "b", "c")
a = c(a,b)

c = seq(as.POSIXct("1912-02-24 23:00:00"), as.POSIXct("1912-02-25 08:32:00"), by="hour")
b = seq(as.POSIXct("1912-02-24 23:00:00"), as.POSIXct("1912-02-25 22:32:00"), by="hour")
subset(b, !(b %in% c))

setwd("C:/Users/Vasco/Desktop/University Things/DynamicPricing/FinalProject")

library(tidyverse)
library(lubridate)
library(janitor)
events = read_csv("511_events.csv")
events = clean_names(events)
events |> pull(organization_name) |> unique()
events = events |> filter(organization_name %in% c("MTA Bridges and Tunnels", "MTA Bridges and Tunnels - Marine Parkway Bridge" , "MTA Bridges and Tunnels - Verrazano Narrows Bridge"))
events = events |> mutate(create_time = gsub(" ", "", create_time),
                          create_time = as.POSIXct(create_time, format = "%m/%d/%Y%H:%M"))
events = events |> mutate(close_time = gsub(" ", "", close_time),
                          close_time = as.POSIXct(close_time, format = "%m/%d/%Y%H:%M"))

events = events |> mutate(init_date = floor_date(create_time, unit = "hour"))
events = events |> mutate(end_date = floor_date(close_time, unit = "hour"))

# Clean facility names

events = events |> mutate(facility_name = gsub("-", " ", facility_name))
events = events |> mutate(facility_name = ifelse(grepl("Hugh", facility_name) & grepl("Carey", facility_name), "Brooklyn Battery Tunnel", facility_name))
events = events |> mutate(facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name) & grepl("Northbound", direction), "Triborough Bronx Bridge", facility_name))
events = events |> mutate(facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name) & grepl("Southbound", direction), "Triborough Bronx Bridge", facility_name))
events = events |> mutate(facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name) & grepl("Eastbound", direction), "Triborough Manhattan Bridge", facility_name))
events = events |> mutate(facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name) & grepl("Westbound", direction), "Triborough Manhattan Bridge", facility_name))

both = events |> filter((direction == "All directions" | direction == "Both directions" | is.na(direction)) & facility_name %in% c("Bronx Whitestone Bridge", "Brooklyn Battery Tunnel", "Cross Bay Bridge", "Henry Hudson Bridge", "Marine Parkway Bridge", "Queens Midtown Tunnel", "RFK Triborough Bridge", "Throgs Neck Bridge", "Verrazano Narrows Bridge"))

n = both |> filter(!(facility_name %in% c("Queens Midtown Tunnel", "Verrazano Narrows Bridge"))) |> mutate(direction = "Northbound", facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name), "Triborough Bronx Bridge", facility_name))
s = both |> filter(!(facility_name %in% c("Queens Midtown Tunnel", "Verrazano Narrows Bridge"))) |> mutate(direction = "Southbound", facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name), "Triborough Bronx Bridge", facility_name))

w = both |> filter(facility_name %in% c("Queens Midtown Tunnel", "Verrazano Narrows Bridge", "RFK Triborough Bridge")) |> mutate(direction = "Westbound", facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name), "Triborough Manhattan Bridge", facility_name))
e = both |> filter(facility_name %in% c("Queens Midtown Tunnel", "Verrazano Narrows Bridge", "RFK Triborough Bridge")) |> mutate(direction = "Eastbound", facility_name = ifelse(grepl("RFK Triborough Bridge", facility_name), "Triborough Manhattan Bridge", facility_name))

events = rbind(events, n,s,w,e)
events = events |> filter(facility_name %in% c("Bronx Whitestone Bridge", "Brooklyn Battery Tunnel", "Cross Bay Bridge", "Henry Hudson Bridge", "Marine Parkway Bridge", "Queens Midtown Tunnel", "Triborough Bronx Bridge", "Triborough Manhattan Bridge", "Throgs Neck Bridge", "Verrazano Narrows Bridge"))
events = events |> filter(!(direction == "All directions" | direction == "Both directions" | is.na(direction)) & !(facility_name == "Brooklyn Battery Tunnel" & direction == "Westbound"))

events = events |> select(c("event_type", "facility_name", "direction", "create_time", "close_time",
                   "init_date", "end_date"))


events <- events %>%
  mutate(Category = case_when(
    # 1. Incidents & Emergencies
    str_detect(event_type, regex(
      "crash|accident|disabled|vehicle fire|truck fire|spill|debris|ems|police|amber alert|incident|overturned|misplaced",
      ignore_case = TRUE
    )) ~ "IncidentsEmergencies",
    
    # 2. Construction & Maintenance
    str_detect(event_type, regex(
      "roadwork|milling|paving|repaving|pothole|bridge|tunnel|crane|repair|maintenance|construction|rehabilitation",
      ignore_case = TRUE
    )) ~ "ConstructionMaintenance",
    
    # 3. Weather & Road Hazards
    str_detect(event_type, regex(
      "wet pavement|flooding|snow removal|fog|falling ice|high winds|drawbridge|weather related",
      ignore_case = TRUE
    )) ~ "WeatherRoadHazards",
    
    # 4. Special Events & Traffic Controls
    str_detect(event_type, regex(
      "special event|marathon|bicycle event|race event|hov rules|new traffic pattern|speed restriction|specialevent|ban|restrictions",
      ignore_case = TRUE
    )) ~ "SpecialEventsTrafficControls",
    
    # 5. Delays & General Operations
    str_detect(event_type, regex(
      "delays|heavy traffic|rough road|operational activity|power failure|test message|watermain break|fire department activity|malfunction",
      ignore_case = TRUE
    )) ~ "DelaysGeneralOperations",
    
    # Catch‑all
    TRUE ~ NA_character_
  ))

construction = tibble(date = character(), facility_name = character(), direction = character(), Construction_Maintenance = integer())

slice(events, 1)
dim(events)[1]

seq(as.POSIXct("1912-02-24 23:00:00"), as.POSIXct("1912-02-25 08:32:00"), by="hour")
seq(slice(events, 2)$init_date, slice(events, 2)$end_date, by="hour")

cons = tibble(date = seq(slice(events, 2)$init_date, slice(events, 2)$end_date, by="hour"), facility_name = slice(events, 2)$facility_name)

bind_rows(construction, tibble(date = seq(slice(events, 2)$init_date, slice(events, 2)$end_date, by="hour"), facility_name = slice(events, 2)$facility_name))

slice(events, 2)

names <- c("IncidentsEmergencies", "ConstructionMaintenance", "WeatherRoadHazards", "SpecialEventsTrafficControls", "DelaysGeneralOperations")

for (name in names) {
  event = events |> filter(Category == name & !(is.na(init_date)) & !(is.na(end_date))) 
  aux <- tibble(date = seq(slice(event, 1)$init_date, slice(event, 1)$end_date, by="hour"), facility_name = slice(event, 1)$facility_name, direction = slice(event, 1)$direction, !!name := 1)
    for (i in 2:nrow(event)){
      aux2 = tibble(date = seq(slice(event, i)$init_date, slice(event, i)$end_date, by="hour"), facility_name = slice(event, i)$facility_name, direction = slice(event, i)$direction, !!name := 1)
      aux = bind_rows(aux, aux2)
    }
  assign(name, ebola)
}
