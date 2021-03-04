library(runjags)
library(stringr)
library(magrittr)

model.files <- dir('data/model-runs/', pattern = "moderator")
moderator.summaries <- NA

for(m in model.files){
  print(m)
  
  model <- readRDS(paste0('data/model-runs/', m))
  
  mod.level <- str_split(m, "-")[[1]][2]
  mod.name <- str_split(str_split(m, "-",3)[[1]][3], pattern='\\.')[[1]][1]
  
  ms <- model$summaries[str_detect(rownames(model$summaries), "moderator.effect\\["),]
      
  r <- nrow(ms)
  if(r == 2){
    moderator.type <- "Continuous"
  } else {
    moderator.type <- "Discrete"
  }
  d <- data.frame(
    moderator = rep(mod.name,r), 
    moderator.level=rep(mod.level,r), 
    moderator.type=rep(moderator.type, r),
    factor.level=rep(1:(r/2),each=2),
    incentive = rep(c("Non-incentivized", "Incentivized"),r/2),
    median = as.numeric(ms[,"Median"]),
    low.95 = as.numeric(ms[,"Lower95"]),
    upper.95 = as.numeric(ms[,"Upper95"])
  )
  
  if(is.na(moderator.summaries)){
    moderator.summaries <- d
  } else {
    moderator.summaries <- rbind(moderator.summaries, d)
  }
}

write_csv(moderator.summaries, path="data/generated/moderator-summaries.csv")
  

