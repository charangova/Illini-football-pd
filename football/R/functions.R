calc_ptdf = function(data) {
  data %>% 
    mutate(pointDiff = IlliniScore - OpponentScore) %>% 
    select(-Date)
}