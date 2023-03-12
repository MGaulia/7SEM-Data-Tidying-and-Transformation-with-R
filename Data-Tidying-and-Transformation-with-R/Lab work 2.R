library(tidyverse)
library(nycflights13)
dffl <- flights

# 1
dffl %>% 
    filter(month==1 & carrier=='UA') %>% 
    select(air_time) %>% 
    mutate(air_time_minutes = air_time %% 60) %>%
    mutate(air_time_hours = as.integer((air_time-air_time_minutes) / 60)) %>%
    summarise(mean_air_time_hours = mean(air_time_hours,na.rm = T),
              mean_air_time_minutes = mean(air_time_minutes,na.rm = T))
# na.rm nurodo ar prieš skaičiuojant vidurkį reikia ištrinti trūkstamus duomenis
# jei na.rm = FALSE, tai esant trūkstamų duomenų, vidurkis bus NA


# 2

# 2.1)
dffl %>% select(origin, dest) %>%
    unlist(dffl, use.names = FALSE) %>%
    table() %>%
    as.data.frame() %>%
    filter(Freq > 50 & Freq < 100)


# 2.2)
dffl %>%
    mutate(abs_delay = abs(dep_delay + arr_delay)) %>%
    select(flight, abs_delay) %>%
    filter(abs_delay == min(abs_delay, na.rm = T))


# 2.3)
dffl %>%
    drop_na() %>%
    group_by(year, month, tailnum) %>%
    summarise(n = n()) %>%
    group_by(tailnum) %>%
    summarise(mean_flight_count = mean(n, na.rm=TRUE)) %>%
    arrange(desc(mean_flight_count)) %>%
    top_n(1)


# 2.4)
dffl %>% 
    group_by(carrier, dest) %>%
    summarise(n = n()) %>%
    slice_max(n)


# 2.5)
dffl %>% 
    filter(tailnum %in% c( "N14228", "N723MQ")) %>%
    group_by(tailnum,  month) %>% 
    summarise(items = list(dest))


# 3
inner_join(
    dffl,
    dffl %>% 
    group_by(carrier) %>%   
    summarise(x = n()) %>%
    mutate(carrierCat = 
               ifelse(x <= 500, 1, 
               ifelse(x <= 5000, 2,
               ifelse(x <= 15000, 3, 4)))) %>%
    select(carrier, carrierCat), 
    na_matches = "never")





