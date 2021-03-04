library(stringr)
library(tidybayes)
library(dplyr)
library(readr)

model.files <- dir('data/model-runs/', pattern = "moderator")
moderator.summaries <- NA

for(m in model.files){
  print(m)
  
  model <- readRDS(paste0('data/model-runs/', m))
  
  mod.level <- str_split(m, "-")[[1]][2]
  mod.name <- str_split(str_split(m, "-",3)[[1]][3], pattern='\\.')[[1]][1]
  
  d <- model %>%
    gather_draws(r.sq[incentive]) %>%
    median_hdci(.value, .width=c(0.5,0.95)) %>%
    mutate(moderator = mod.name, moderator.level = mod.level)
  
  if(is.na(moderator.summaries)){
    moderator.summaries <- d
  } else {
    moderator.summaries <- rbind(moderator.summaries, d)
  }
}

write_csv(moderator.summaries, path="data/generated/moderator-r2-summaries.csv")
  

