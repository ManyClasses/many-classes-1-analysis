n.classes <- 40
metric.factors <- c('class.size', 'proportion.lecture','n.assignments','n.exams','admission.rate')
nominal.factors <- c('discipline', 'format', 'class.level')

classes <- data.frame(
  id=1:n.classes,
  incentive=sample(c("Incentive", "NoIncentive"), n.classes, replace=T),
  discipline=sample(c("STEM", "Non-STEM"), n.classes, replace=T),
  format=sample(c("Classroom", "Online", "Hybrid"), n.classes, replace=T),
  class.size=rbinom(n.classes, 200, 0.5),
  proportion.lecture=rbeta(n.classes, 0.8, 0.8),
  n.assignments=2+rbinom(n.classes, 10, 0.1),
  class.level=sample(c("Intro","Intermediate","Advanced"), n.classes, replace=T),
  n.exams=2+sample(c(0,2),n.classes, prob = c(0.8, 0.2), replace=T),
  admission.rate=rbeta(n.classes,5,2)
)

# convert to numeric factors
classes$incentive <- as.numeric(factor(classes$incentive, levels=c("NoIncentive", "Incentive")))
classes$discipline <- as.numeric(factor(classes$discipline, levels=c("STEM", "Non-STEM")))
classes$format <- as.numeric(factor(classes$format, levels=c("Classroom", "Online", "Hybrid")))
classes$class.level <- as.numeric(factor(classes$class.level, levels=c("Intro", "Intermediate", "Advanced")))

student.data <- NA
for(c in 1:n.classes){
  class.size <- classes$class.size[c]
  one.class <- data.frame(
    student.id=1:class.size,
    class.id=rep(c,class.size),
    delta.z=rnorm(class.size, 0, 1)
  )
  if(c > 1){
    student.data <- rbind(student.data, one.class)
  } else {
    student.data <- one.class
  }
}

# summaries for JAGS
delta.z <- student.data$delta.z
n.students <- nrow(student.data)
class.index <- student.data$class.id
class.condition <- classes$incentive
n.metric.moderators <- 5
n.nominal.moderators <- 3
n.levels.nominal.moderator <- c(2,3,3)
n.conditions <- 2
y.metric <- array(data=t(as.matrix(classes)[,metric.factors]), dim=c(n.metric.moderators, n.classes))
y.nominal <- array(data=t(as.matrix(classes)[,nominal.factors]), dim=c(n.nominal.moderators, n.classes))

# packaging data for JAGS
jags.data <- list(
  delta.z=delta.z,
  n.students=n.students,
  class.index=class.index,
  class.condition=class.condition,
  n.metric.moderators=n.metric.moderators,
  n.nominal.moderators=n.nominal.moderators,
  n.conditions=n.conditions,
  n.classes=n.classes,
  n.levels.nominal.moderator=n.levels.nominal.moderator,
  y.metric=y.metric,
  y.nominal=y.nominal
)

# run jags
library(runjags)
run.jags('institution-and-classroom-level-jags-model.txt', 
         data=jags.data, 
         n.chains=3,
         sample = 1000,
         burnin = 1000,
         monitor = c('mu.class', 'sigma.class','beta.intercept', 'beta.metric', 'beta.nominal', 'sigma.mode', 'sigma.sd'))
