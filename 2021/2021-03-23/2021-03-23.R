library(tidyverse)
library(lubridate)
library(mclust)
library(maps)
library(gganimate)
library(here)


# Read and join data
tuesdata <- tidytuesdayR::tt_load("2021-03-23")

unvotes <- tuesdata$unvotes
issues <- tuesdata$issues
roll_calls <- tuesdata$roll_calls

un_df <-
  full_join(unvotes, roll_calls %>% select(rcid, date), by = "rcid")

un_df <- un_df %>%
  mutate(
    # H/T to Julia Silge
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote),
    rcid = paste0("rcid_", rcid),
    decade = year(floor_date(date, years(10)))
  ) %>%
  select(-country_code, -date)


un_countries <- un_df %>%
  select(country) %>%
  unique()

for (dec in unique(un_df$decade)) {
  un_df_temp <- un_df %>%
    filter(decade == dec) %>%
    select(-decade) %>%
    pivot_wider(
      names_from = "rcid",
      values_from = "vote",
      values_fill = 2
    )
  clust <-
    Mclust(as.matrix(un_df_temp
    %>% select(-country),
    G = 1:20
    ))

  un_df_temp <- un_df_temp %>%
    mutate("cluster_{dec}" := clust$classification) %>%
    select(country, starts_with("cluster_"))

  un_countries <-
    un_countries %>% full_join(un_df_temp, by = c("country"))
}

un_countries <- un_countries %>%
  mutate(
    region = case_when(
      country == "United States" ~ "USA",
      country == "United Kingdom" ~ "UK",
      country == "Myanmar (Burma)" ~ "Myanmar",
      country == "Trinidad & Tobago" ~ "Trinidad",
      country == "Yemen Arab Republic" |
        country == "Yemen People's Republic" ~ "Yemen",
      country == "Federal Republic of Germany" |
        country == "German Democratic Republic" ~ "Germany",
      TRUE ~ country
    )
  ) %>%
  select(-country)


# Plotting
world_map <- map_data("world")

un_world <- left_join(world_map, un_countries) %>%
  pivot_longer(
    cols = starts_with("cluster_"),
    names_to = "decade",
    names_prefix = "cluster_",
    values_to = "cluster"
  ) %>%
  mutate(cluster = as_factor(replace_na(cluster, 0)), 
         decade = as.integer(decade))

p <- ggplot(un_world, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = cluster)) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(
    "#999999",
    RColorBrewer::brewer.pal(10, "Set3")
  )) 

p_anim <- p +
  transition_states(
    decade,
    transition_length = 2,
    state_length = 3,
    wrap = FALSE
  ) +
  labs(title = "UN Voting Clusters",
       subtitle = "Decade: {closest_state}",
       caption = "@TarenSanders")

anim_save(here("2021", "2021-03-23", "unvotes.gif"),
  p_anim,
  width = 480 * 2,
  height = 480
)
