library(caTools)
library(ggplot2)

dataset = read.csv(file.choose())
dataset = dataset[3:5]

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio=0.75)
training_set = subset(dataset, split==TRUE)
test_set = subset(dataset, split==FALSE)

age_mean = mean(training_set$age)
age_sd = sd(training_set$age)
sal_mean = mean(training_set$EstimatedSalary)
sal_sd = sd(training_set$EstimatedSalary)

training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

classifier = glm(Purchased ~ ., family=binomial, data=training_set)

prob_pred = predicted(classifier, type="response", newdata=test_set[-3])
y_pred = ifelse(prob_pred > 0.5,1,0)
cm = table(test_set[.3], y_pred)
print(cm)

plot_set = function(set, title_name)
{
  x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
  x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
  grid_set = expand.grid(x1,x2)
  colnames(grid_set) = c("Age", "EstimatedSalary")
  
  prob_set = predicted(classifier, type="response", newdata=grid_set)
  y_pred = ifelse(prob_pred > 0.5,1,0)
  
  plot(set[,1:2], main=title_name,xlab="age", ylab="EstimatedSalary", xlim=range(x1),ylim=range(x2))
  contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)), add=TRUE)
  points(set, pch=21, bg=ifelse(set[,3]==1,"green","red"))
}

plot_set(training_set, "LR Training Set")
plot_set(test_set, "LR Test Set")

cat("Enter the age")
user_age = as.numeric(readline())

cat("Enter the salary")
user_salary = as.numeric(readline())

user_age = (user_age - age_mean / age_sd)
user_salary = (user_salary - sal_mean) / sal_sd

new_customer = data.frame(Age = user_age, EstimatedSalary = user_salary)

probability = predict(classifier, type = "response", newdata = new_customer)
prediction = ifelse(probability > 0.5,1,0)

cat("Probability:",
    round(probability, 4),
    "\n")

if(prediction == 1)
{
  cat("Customer WILL buy SUV\n")
} else {
  cat("Customer will NOT buy SUV\n")
}


