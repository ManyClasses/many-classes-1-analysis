n.classes <- 20

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
n.students <- nrow(student.data)
n.metric.moderators <- 5
n.nominal.moderators <- 3
n.levels.nominal.moderator <- c(2,3,3)

class.index <- student.data$class.id
class.condition <- as.numeric(factor(classes$incentive, levels=c("NoIncentive", "Incentive")))

y.metric <- matrix()
y.nominal <- matrix()
