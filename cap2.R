library(tidyverse)
library(fpp3)
library(tsibble)


# tsibble extends tidy data frames (tibble)
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

y

# A tsibble: 5 x 2 [1Y] <---- "Indica que los datos son anuales"
# Year Observation
# <int>       <dbl>
#   1  2015         123
# 2  2016          39
# 3  2017          78
# 4  2018          52
# 5  2019         110


olympic_running

# A tsibble: 312 x 4 [4Y]
# Key:       Length, Sex [14] <-- La forma en que se "agrupan" las series de tiempo (en este caso hay 14 ST)
# Year Length Sex    Time
# <int>  <int> <chr> <dbl>
#   1  1896    100 men    12  
# 2  1900    100 men    11  
# 3  1904    100 men    11  
# 4  1908    100 men    10.8
# 5  1912    100 men    10.8
# 6  1916    100 men    NA  
# 7  1920    100 men    10.8
# 8  1924    100 men    10.6
# 9  1928    100 men    10.8
# 10  1932    100 men    10.3
# … with 302 more rows
# ℹ Use `print(n = ...)` to see more rows

olympic_running |> distinct(Length)
olympic_running |> distinct(Sex)


PBS

PBS |> 
  filter(ATC2 == "A10")

PBS |> 
  filter(ATC2 == "A10") |> 
  select(Month, Concession, Type, Cost)

# Cost per month
PBS |> 
  filter(ATC2 == "A10") |> 
  select(Month, Concession, Type, Cost) |> 
  summarise(TotalC = sum(Cost))

# Transform units to millions of dollars
PBS |> 
  filter(ATC2 == "A10") |> 
  select(Month, Concession, Type, Cost) |> 
  summarise(TotalC = sum(Cost)) |> 
  mutate(Cost = TotalC / 1e6)


a10 <- PBS |> 
  filter(ATC2 == "A10") |> 
  select(Month, Concession, Type, Cost) |> 
  summarise(TotalC = sum(Cost)) |> 
  mutate(Cost = TotalC / 1e6)




# Ejemplo de lectura y transformación a tsibble
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison



# Time plots ----
melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers/1000)

autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")



# In the next TS there is a clear and increasing trend. There is also a 
# strong seasonal pattern that increases in size as the level of  the series
# increases
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
        title = "Australian antidiabetic drug sales")



# Seasonal plot
a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales")

# Multiple seasonal periods
vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")


vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")


vic_elec |> gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")


# Seasonal subseries
a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )
