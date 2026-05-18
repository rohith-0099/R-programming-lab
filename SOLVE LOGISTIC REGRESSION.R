# Logistic Regression

library(caTools)
library(ggplot2)

# Import dataset
dataset = read.csv(file.choose())

# Select columns
dataset = dataset[3:5]

# Split dataset
set.seed(123)

split = sample.split(dataset$Purchased,
                     SplitRatio = 0.75)

training_set = subset(dataset,
                      split == TRUE)

test_set = subset(dataset,
                  split == FALSE)

# Save mean and sd
age_mean = mean(training_set$Age)
age_sd = sd(training_set$Age)

sal_mean = mean(training_set$EstimatedSalary)
sal_sd = sd(training_set$EstimatedSalary)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Train model
classifier = glm(Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predict test set
prob_pred = predict(classifier,
                    type = "response",
                    newdata = test_set[-3])

y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Confusion Matrix
cm = table(test_set[,3], y_pred)
print(cm)

# Visualization Function
plot_set = function(set, title_name)
{
  X1 = seq(min(set[,1]) - 1,
           max(set[,1]) + 1,
           by = 0.01)
  
  X2 = seq(min(set[,2]) - 1,
           max(set[,2]) + 1,
           by = 0.01)
  
  grid_set = expand.grid(X1, X2)
  
  colnames(grid_set) =
    c("Age", "EstimatedSalary")
  
  prob_set = predict(classifier,
                     type = "response",
                     newdata = grid_set)
  
  y_grid = ifelse(prob_set > 0.5,
                  1,
                  0)
  
  plot(set[,1:2],
       main = title_name,
       xlab = "Age",
       ylab = "Estimated Salary",
       xlim = range(X1),
       ylim = range(X2))
  
  contour(X1,
          X2,
          matrix(as.numeric(y_grid),
                 length(X1),
                 length(X2)),
          add = TRUE)
  
  
  points(set,
         pch = 21,
         bg = ifelse(set[,3] == 1,
                     "green",
                     "red"))
}

# Training Plot
plot_set(training_set,
         "Logistic Regression (Training set)")

# Test Plot
plot_set(test_set,
         "Logistic Regression (Test set)")

# User Input
cat("Enter Age: ")
user_age = as.numeric(readline())

cat("Enter Salary: ")
user_salary = as.numeric(readline())

# Scale input
user_age =
  (user_age - age_mean) / age_sd

user_salary =
  (user_salary - sal_mean) / sal_sd

new_customer = data.frame(
  Age = user_age,
  EstimatedSalary = user_salary
)

# Prediction
probability = predict(classifier,
                      type = "response",
                      newdata = new_customer)

prediction = ifelse(probability > 0.5,
                    1,
                    0)

cat("Probability:",
    round(probability, 4),
    "\n")

if(prediction == 1)
{
  cat("Customer WILL buy SUV\n")
} else {
  cat("Customer will NOT buy SUV\n")
}