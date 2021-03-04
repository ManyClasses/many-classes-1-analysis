ESS_SAMPLE_SIZE <- 10000
PSRF_TARGET <- 1.1
N_CHAINS <- 48
SAMPLES_PER_CHAIN <- 3000
BURNIN <- 5000
THIN <- 4
OVERWRITE <- F

library(runjags)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
library(forcats)
library(purrr)
library(readr)

runjags.options(force.summary=TRUE, silent.jags=FALSE, silent.runjags=FALSE)

set.seed(47401)

student.data <- read_csv('data/manyclasses_student_outcomes_and_moderators.csv', col_types = "ddfddfffddddddd")

institutions.data <- read_csv('data/many_classes_institutions.csv', col_types = "cdddndddddd") %>% select(-Institution)

classes.data <- read_csv('data/many_classes_classes.csv', col_types = "ddffdddfdfddfdddfddffddddfdf")
classes.data <- classes.data %>% arrange(manyclasses_course_id) %>%
  mutate(IncentiveCondition = factor(IncentiveCondition, levels=c("No", "Yes"))) %>%
  left_join(institutions.data, by=c("manyclasses_institution_id"))

# TODO: check with Ben. temp. filter for columns that are incomplete!
# classes.data <- classes.data %>% select(-Quiz_Difficulty_AveragePercentCorrect, -Quiz_FollowUpValue_ProportionOfClassPoints)


student.class.data <- student.data %>% 
  left_join(classes.data, by=c("manyclasses_course_id", "manyclasses_institution_id"))

delta.z <- student.class.data$outcome
n.classes <- nrow(classes.data)
n.students <- nrow(student.class.data)
class.index <- student.class.data$manyclasses_course_id
class.condition <- as.numeric(classes.data$IncentiveCondition)
n.conditions <- 2

# packaging data for JAGS
jags.data <- list(
  delta.z=delta.z,
  n.students=n.students,
  class.index=class.index,
  class.condition=class.condition,
  n.conditions=n.conditions,
  n.classes=n.classes
)

minimum.ess <- function(runjags.object){
  print(paste0("Smallest ESS: ", min(runjags.object$summaries[,'SSeff'])))
  return(all(runjags.object$summaries[,'SSeff'] >= ESS_SAMPLE_SIZE))
}

good.psrf <- function(runjags.object){
  print(paste0("Worst psrf: ", max(runjags.object$psrf$psrf[,2])))
  return(all(runjags.object$psrf$psrf[,2] <= PSRF_TARGET))
}

run.jags.until.sample <- function(runjags.object){
  while(!minimum.ess(runjags.object) || !good.psrf(runjags.object)){
    print(".....Upping sample length.....")
    runjags.object <- extend.jags(runjags.object,
                                  sample = runjags.object$sample * 2,
                                  thin = THIN,
                                  method="parallel")
  }
  return(runjags.object)
}

# BASE MODEL

if("overall.rds" %in% dir('data/model-runs') && !OVERWRITE){
  print(paste0("Model output already exists for main model"))
} else {

  model.1.result <- run.jags(
    model='models/basic-model-jags.txt', 
    data=jags.data,
    n.chains = N_CHAINS,
    thin = THIN,
    sample = SAMPLES_PER_CHAIN,
    burnin = BURNIN,
    method="parallel",
    monitor = c('mu.class','sigma.class', 'mu.condition.effect', 'sigma.condition.effect', 'sigma.mode', 'sigma.sd', 'mu.condition.diff', 'mu.condition.average')
  )
  
  model.1.result <- run.jags.until.sample(model.1.result)
  
  saveRDS(model.1.result, 'data/model-runs/overall.rds')
}

# CLASS MODERATORS

run.class.moderator.model <- function(moderator.name, overwrite=FALSE){
  
  if(!moderator.name %in% colnames(classes.data)) {
    stop(paste0("Moderator '", moderator.name, "' does not appear in classes data frame"))
  }
  
  if(paste0('moderator-class-', moderator.name, ".rds") %in% dir('data/model-runs') && !overwrite){
    print(paste0("Model output already exists for moderator-class-", moderator.name))
    return(F)
  }
  
  data.filtered <- student.class.data %>% filter(!is.na(!!sym(moderator.name)))
  
  # packaging data for JAGS
  jags.data.moderator <- list(
    delta.z=data.filtered$outcome,
    n.students=nrow(data.filtered),
    class.index=data.filtered$manyclasses_course_id,
    class.condition=as.numeric(data.filtered$IncentiveCondition),
    n.conditions=2,
    n.classes=n.classes,
    moderator.level = as.numeric(data.filtered[[moderator.name]]),
    no.incentive.classes = which(class.condition==1),
    incentive.classes = which(class.condition==2)
  )
  
  if(is.numeric(classes.data[[moderator.name]])){
    model <- 'models/cont-class-moderator-model-jags.txt'
  } else {
    model <- 'models/disc-class-moderator-model-jags.txt'
    jags.data.moderator$n.moderator.levels = length(levels(classes.data[[moderator.name]]))
  }
  
  model.moderator.result <- run.jags(
    model=model, 
    data=jags.data.moderator,
    n.chains = N_CHAINS,
    thin = THIN,
    sample = SAMPLES_PER_CHAIN,
    burnin = BURNIN,
    monitor = c('mu.class','sigma.class', 'mu.condition.effect', 'sigma.condition.effect', 'sigma.mode', 'sigma.sd', 'moderator.effect', 'r.sq')
  )
  
  model.moderator.result <- run.jags.until.sample(model.moderator.result)
  
  saveRDS(model.moderator.result, file=paste0("data/model-runs/moderator-class-",moderator.name,".rds"))
  
  return(T)
}

class.level.moderators <- colnames(classes.data)[3:length(colnames(classes.data))]
class.level.moderators <- class.level.moderators[class.level.moderators != "IncentiveCondition"]

for(m in class.level.moderators){
  print('CLASS MODERATOR')
  print(m)
  run.class.moderator.model(m, overwrite=OVERWRITE)
}

run.student.moderator.model <- function(moderator.name, overwrite=FALSE){
  
  #print(moderator.name)
  
  if(!moderator.name %in% colnames(student.class.data)) {
    stop(paste0("Moderator '", moderator.name, "' does not appear in student.class.data data frame"))
  }
  
  if(paste0('moderator-student-', moderator.name, ".rds") %in% dir('data/model-runs') && !overwrite){
    print(paste0("Model output already exists for moderator-student-", moderator.name))
    return(F)
  }
  
  data.filtered <- student.class.data %>% filter(!is.na(!!sym(moderator.name)))
  
  # packaging data for JAGS
  jags.data.moderator <- list(
    delta.z=data.filtered$outcome,
    n.students=nrow(data.filtered),
    class.index=data.filtered$manyclasses_course_id,
    class.condition=as.numeric(data.filtered$IncentiveCondition),
    n.conditions=2,
    n.classes=n.classes,
    moderator.level = as.numeric(data.filtered[[moderator.name]]),
    no.incentive.classes = which(class.condition==1),
    incentive.classes = which(class.condition==2)
  )
  
  monitor.list <- c('mu.class','sigma.class', 'mu.condition.effect', 'sigma.condition.effect', 'sigma.mode', 'sigma.sd', 'moderator.effect', 'moderator.effect.class', 'moderator.effect.sigma', 'r.sq')
  
  if(is.numeric(student.class.data[[moderator.name]])){
    model <- 'models/cont-student-moderator-model-jags.txt'
  } else {
    model <- 'models/disc-student-moderator-model-jags.txt'
    jags.data.moderator$n.moderator.levels = length(levels(student.class.data[[moderator.name]]))
  }
  
  model.moderator.result <- run.jags(
    model=model, 
    data=jags.data.moderator,
    n.chains = N_CHAINS,
    thin=THIN,
    sample = SAMPLES_PER_CHAIN,
    burnin = BURNIN,
    monitor = monitor.list,
    method="parallel"
  )
  
  model.moderator.result <- run.jags.until.sample(model.moderator.result)
  
  saveRDS(model.moderator.result, file=paste0("data/model-runs/moderator-student-",moderator.name,".rds"))
  
  return(T)
}

student.level.moderators <- colnames(student.data)[5:length(colnames(student.data))]

for(m in student.level.moderators){
  print('STUDENT MODERATOR')
  print(m)
  run.student.moderator.model(m, overwrite = OVERWRITE)
}