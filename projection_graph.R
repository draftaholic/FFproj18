Poscolors <- c(
  "#ce4b4b", 
  "#009E73", 
  "#8679bf", 
  "#ffb31a",
  "##42f4d9")

names(Poscolors) <- c("QB","RB","TE","WR","D/ST")

sfb8.proj %>%
  filter(avg.Proj > 250, Pos != "QB") %>%
  mutate(Player = reorder(Player, Proj, median)) %>%
  ggplot()+
  aes(x = Proj, y = Player, fill = Pos)+
  geom_density_ridges(
    alpha=.65, jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    quantile_lines = T,
    point_shape = '^', point_size = 1.5
  )+
  scale_fill_manual(values = Poscolors)+
  theme_minimal()+
  labs(
    y = "Projection",
    title = "2018 Fantasy Football Median Projections, Elite Tier",
    subtitle = "#SFB8 Scoring"
  )

sfb8.proj %>%
  filter(avg.Proj > 200, avg.Proj < 251, Pos != "QB") %>%
  mutate(Player = reorder(Player, Proj, median)) %>%
  ggplot()+
  aes(x = Proj, y = Player, fill = Pos)+
  geom_density_ridges(
    alpha=.65, jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    quantile_lines = T,
    point_shape = '^', point_size = 1.5
  )+
  scale_fill_manual(values = Poscolors)+
  theme_minimal()+
  labs(
    y = "Projection",
    title = "2018 Fantasy Football Median Projections, Top Tier",
    subtitle = "#SFB8 Scoring"
  )


sfb8.proj %>%
  filter(avg.Proj > 160, avg.Proj < 201, Pos != "QB") %>%
  mutate(Player = reorder(Player, Proj, median)) %>%
  ggplot()+
  aes(x = Proj, y = Player, fill = Pos)+
  geom_density_ridges(
    alpha=.65, jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    quantile_lines = T,
    point_shape = '^', point_size = 1.5
  )+
  scale_fill_manual(values = Poscolors)+
  theme_minimal()+
  labs(
    y = "Projection",
    title = "2018 Fantasy Football Median Projections, Mid Tier",
    subtitle = "#SFB8 Scoring"
  )


sfb8.proj %>%
  filter(avg.Proj > 120, avg.Proj < 161, Pos != "QB") %>%
  mutate(Player = reorder(Player, Proj, median)) %>%
  ggplot()+
  aes(x = Proj, y = Player, fill = Pos)+
  geom_density_ridges(
    alpha=.65, jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    quantile_lines = T,
    point_shape = '^', point_size = 1.5
  )+
  scale_fill_manual(values = Poscolors)+
  theme_minimal()+
  labs(
    y = "Projection",
    title = "2018 Fantasy Football Median Projections, Low Tier",
    subtitle = "#SFB8 Scoring"
  )

sfb8.proj %>%
  filter(avg.Proj > 90, avg.Proj < 121, Pos != "QB") %>%
  mutate(Player = reorder(Player, Proj, median)) %>%
  ggplot()+
  aes(x = Proj, y = Player, fill = Pos)+
  geom_density_ridges(
    alpha=.65, jittered_points = T,
    position = position_points_jitter(width = 0.05, height = 0),
    quantile_lines = T,
    point_shape = '^', point_size = 1.5
  )+
  scale_fill_manual(values = Poscolors)+
  theme_minimal()+
  labs(
    y = "Projection",
    title = "2018 Fantasy Football Median Projections, Bargain Tier",
    subtitle = "#SFB8 Scoring"
  )