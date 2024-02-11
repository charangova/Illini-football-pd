library(tidyverse)

url = "https://raw.githubusercontent.com/wadefagen/datasets/master/illini-football/illini-football-scores.csv"
football = read_csv(url,show_col_types = FALSE)
write_csv(x=football, file = "~/Desktop/football/data/football.csv")


tail(football)

football %>% 
  mutate(pointDiff = IlliniScore - OpponentScore) %>% 
  ggplot() +
  aes(x=Date, y = pointDiff, fill = OpponentRank) %>% 
  geom_point()
