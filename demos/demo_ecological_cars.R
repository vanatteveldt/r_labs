library(tidyverse)
library(cbsodataR)

# Autobezit in nederland: 81845NED
auto_data <- cbs_get_data("81845NED",
                          Perioden = "2015JJ00",
                          AantalVoertuigenInHuishouden = "40000")   # min. 1 voertuig

# De metadata bevat per dimensie een tabel met Key (code) en Title (label)
auto_meta <- cbs_get_meta("81845NED")
huishoudkenmerken <- auto_meta$Huishoudkenmerken |>
  select(Key, kenmerk = Title) |>
  mutate(Key = str_trim(Key))

auto_data |>
  mutate(Huishoudkenmerken = str_trim(Huishoudkenmerken)) |>
  inner_join(huishoudkenmerken, by = c("Huishoudkenmerken" = "Key")) |>
  select(kenmerk,
         pct_auto = HuishoudensInBezitVanAuto_2) |>
  filter(str_starts(kenmerk, "Gestandaardiseerd inkomen"), 
         !str_detect(kenmerk, "onbekend"))  |>
  mutate(groep = str_remove(kenmerk, "Gestandaardiseerd inkomen: ")) |>
  ggplot(aes(groep, pct_auto)) +
  geom_col() +
  labs(x = "Gestandaardiseerd inkomen (20%-groep)", y = "% huishoudens met auto",
       title = "Rijkere huishoudens hebben vaker een auto (2015)")

# Regionale kerncijfers: 85618NED

wijk_meta <- cbs_get_meta("85618NED")$WijkenEnBuurten |>
  select(Key, wijk_naam = Title) |>
  mutate(Key = str_trim(Key))

wijken <- cbs_get_data("85618NED", WijkenEnBuurten = has_substring("WK")) |>
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten)) |>
  inner_join(wijk_meta, by = c("WijkenEnBuurten" = "Key")) |>
  transmute(wijk     = wijk_naam,
            gemeente = str_trim(pick(matches("^Gemeentenaam"))[[1]]),
            autos    = pick(matches("^PersonenautosPerHuishouden"))[[1]],
            inkomen  = pick(matches("^GemiddeldInkomenPerInwoner"))[[1]]) |>
  drop_na(autos, inkomen)

cor.test(wijken$autos, wijken$inkomen) 
ams <- filter(wijken, gemeente == "Amsterdam")
cor.test(ams$autos, ams$inkomen)

ggplot(ams, aes(inkomen, autos)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Gem. inkomen per inwoner (x 1000 euro)",
       y = "Personenauto's per huishouden",
       title = "Amsterdamse wijken: rijkere wijken, niet meer auto's")

ams |> filter(autos > 2)
with(filter(ams, autos < 2), cor.test(autos, inkomen))
