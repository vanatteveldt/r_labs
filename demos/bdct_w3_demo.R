library(tidyverse)
temp <- read_csv("https://raw.githubusercontent.com/vanatteveldt/r4css/refs/heads/main/data/temperature_anomaly.csv")

# plotline per region
ggplot(temp, aes(x=year, y=temp_anomaly, color=region)) + 
  geom_line()


# hoe kunnen we verschil berekenen?
# -> pivot wider! 
temp_diff <- temp |> 
  select(year, region, temp_anomaly) |> 
  pivot_wider(names_from=region, values_from=temp_anomaly) |>
  rename(south=`Southern Hemisphere`, north=`Northern Hemisphere`) |>
  mutate(diff=north - south) 

temp_diff |>
  ggplot(aes(x=year, y=diff)) + 
  geom_hline(yintercept=0, color="red", alpha=.2) + 
  geom_line()


### CO2 data

co2 <- read_csv("https://raw.githubusercontent.com/vanatteveldt/r4css/refs/heads/main/data/co2_emissions.csv")

# simple plot

co2 |> 
  group_by(year) |>
  summarize(co2=sum(co2, na.rm=TRUE)) |>
  ggplot(aes(x=year, y=co2)) + geom_line()

# Hoe kunnen we een lijn per bron maken?
# -> pivot_longer!

ghg_sources <- co2 |>
  select(country, year, co2, methane, nitrous_oxide) |>
  pivot_longer(co2:nitrous_oxide, names_to="type")

ghg_sources |> 
  group_by(year, type) |>
  summarize(value=sum(value, na.rm=TRUE)) |>
  ggplot(aes(x=year, y=value, color=type)) + geom_line()

# Wiens schuld is het?

income <- read_csv("https://raw.githubusercontent.com/vanatteveldt/r4css/refs/heads/main/data/income_groups.csv")
combined <- inner_join(co2, income)

combined |>
  filter(year == 2020) |>
  group_by(income_group) |>
  summarize(co2=sum(cumulative_co2, na.rm=T))|>
  ggplot(aes(y=fct_reorder(income_group, co2), x=co2)) + 
  geom_col() 

left_join(co2, income)

left_join(co2, income) |> filter(year == 2020, is.na(income_group))

anti_join(co2, income) |> filter(year == 2020)
