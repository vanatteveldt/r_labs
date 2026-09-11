library(tidyverse)
votes <- read_csv("https://raw.githubusercontent.com/vanatteveldt/r4css/refs/heads/main/data/dutch_elections_2023.csv")
glimpse(votes)
head(votes)

filter(votes, municipality == "Amsterdam") |>
  arrange(desc(votes))


votes_pvda <- filter(party == "PvdA/GL") |>
  arrange(desc(votes))

#














# Simple filter / sort / select
votes |>
  select(town=municipality, party, votes) |>
  filter(town == "Amsterdam") |>
  arrange(desc(votes))

# Note: changes have been saved to memory!

# Simple join
demographics <- read_csv("https://raw.githubusercontent.com/vanatteveldt/r4css/refs/heads/main/data/dutch_demographics.csv")
combined <- inner_join(votes, demographics)  |>
  select(municipality, party, votes, pop=v01_pop, dutch=v43_nl, density=v57_density)

# Simple plot
combined |>
  filter(party == "PVV") |>
  ggplot(mapping = aes(x = dutch, y = votes)) +
  geom_point()

# Simple plot
combined |>
  filter(party == "BBB") |>
  ggplot(mapping = aes(x = dutch, y = votes)) +
  geom_point()

# Beautify
combined |>
  filter(party == "BBB") |>
  ggplot(mapping = aes(x = density, y = votes, size=pop, fill=100-dutch)) +
  geom_point(alpha=.8, shape = 21, color="grey20") +
  scale_fill_gradient(low = "white", high = "darkmagenta") +
  guides(size = "none") +
  labs(title="Support for the BBB party per municipality",
       subtitle="Each point is one municipality; size represents total population",
       x="Population density",
       y="Vote share of BBB",
       fill="% of population\nwith migration history",
       size="Total population",
       caption="Source: CBS (demographics); Kiesraad (voting results)"
  ) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color = "grey50", fill = NA),
        legend.background = element_rect(fill = "white", color = NA))


# Find interesting points
highlight <- combined |>
  filter(party == "BBB",
         votes > 18 | votes < 1.05 | pop>300000 |
           municipality == "Deventer")


# Add them to plot
combined |>
  filter(party == "BBB") |>
  ggplot(mapping = aes(x = density, y = votes, size=pop, fill=100-dutch)) +
  geom_point(alpha=.8, shape = 21, color="grey20") +
  geom_text(aes(label=municipality), data=highlight, size=3, nudge_y=-.2) +
  scale_fill_gradient(low = "white", high = "darkmagenta") +
  guides(size = "none") +
  labs(title="Support for the BBB party per municipality",
       subtitle="Each point is one municipality; size represents total population",
       x="Population density",
       y="Vote share of BBB",
       fill="% of population\nwith migration history",
       size="Total population",
       caption="Source: CBS (demographics); Kiesraad (voting results)"
  ) +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.98, 0.98),
        legend.title.position = "top",
        legend.direction = "horizontal",
        legend.justification = c("right", "top"),
        legend.box.background = element_rect(color = "grey50", fill = NA),
        legend.background = element_rect(fill = "white", color = NA))


