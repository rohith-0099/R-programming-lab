# Support Vector Machine (SVM)

library(caTools)
library(e1071)

# Import dataset
dataset = read.csv(file.choose())

# Select columns
dataset = dataset[3:5]

# Convert output into factor
dataset$Purchased =
  factor(dataset$Purchased,
         levels = c(0,1))

# Split dataset
set.seed(123)

split = sample.split(dataset$Purchased,
                     SplitRatio = 0.75)

training_set = subset(dataset,
                      split == TRUE)

test_set = subset(dataset,
                  split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Train SVM Model
classifier = svm(Purchased ~ .,
                 data = training_set,
                 type = "C-classification",
                 kernel = "linear")

# Predict test set
y_pred = predict(classifier,
                 newdata = test_set[-3])

# Confusion Matrix
cm = table(test_set[,3],
           y_pred)

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
  
  y_grid = predict(classifier,
                   newdata = grid_set)
  
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
         "SVM (Training set)")

# Test Plot
plot_set(test_set,
         "SVM (Test set)")