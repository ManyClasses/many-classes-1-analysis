library(runjags)
library(stringr)
library(readr)

model.files <- dir('data/model-runs/', pattern = "moderator")
moderator.chain.summaries <- NA

for(m in model.files){
  print(m)
  
  model <- readRDS(paste0('data/model-runs/', m))
  
  mod.level <- str_split(m, "-")[[1]][2]
  mod.name <- str_split(str_split(m, "-",3)[[1]][3], pattern='\\.')[[1]][1]
  
  min.ess <- min(model$summaries[,'SSeff'])
  worst.psrf <- max(model$summaries[,'psrf'])
  n.samples <- model$sample
      
  
  d <- data.frame(
    moderator = mod.name,
    moderator.level=mod.level, 
    min.ess=min.ess,
    worst.psrf =worst.psrf,
    n.samples.per.chain=n.samples
  )
  
  if(is.na(moderator.chain.summaries)){
    moderator.chain.summaries <- d
  } else {
    moderator.chain.summaries <- rbind(moderator.chain.summaries, d)
  }
}

write_csv(moderator.chain.summaries, path="data/generated/moderator-chain-summaries.csv")
  

