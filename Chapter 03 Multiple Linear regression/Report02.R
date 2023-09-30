# Load the mtcars dataset
# install.packages('datasets')
library('datasets')
data(mtcars)
mtcars

# Fit the initial model with an empty set of predictor variables
initial_model <- lm(mpg ~ 1, data = mtcars) 

# Define the criterion for model evaluation (e.g., AIC)
criterion <- AIC

# Stepwise selection
while (TRUE) {
  # Perform forward selection
  forward_model <- step(initial_model, scope = list(lower = formula(initial_model), upper = formula(mpg ~ .)), direction = "forward", criterion = criterion)
  
  # Perform backward selection
  backward_model <- step(initial_model, scope = list(lower = formula(initial_model), upper = formula(mpg ~ .)), direction = "backward", criterion = criterion)
  
  # Compare the models
  if (criterion(forward_model) < criterion(backward_model)) {
    # If forward model has a lower criterion value, update the initial model with the forward model
    initial_model <- forward_model
    print("Forward Selection:")
    print(summary(initial_model))
  } else if (criterion(backward_model) < criterion(forward_model)) {
    # If backward model has a lower criterion value, update the initial model with the backward model
    initial_model <- backward_model
    print("Backward Selection:")
    print(summary(initial_model))
  } else {
    # If neither model improves the criterion value, break the loop
    break
  }
}

# Final selected model
final_model <- initial_model
print("Final Selected Model:")
print(summary(final_model))


# Generate a synthetic dataset
set.seed(123)
n <- 1000
A <- rnorm(n)
B <- rnorm(n)
C <- A + B + rnorm(n)
# Perform a simple linear regression of A on C
model1 <- lm(A ~ C)
summary(model1)
# Perform a simple linear regression of B on C
model2 <- lm(B ~ C)
summary(model2)
# Perform a multiple linear regression of A and B on C
model3 <- lm(A + B ~ C)
summary(model3)





# Data
Smoking <- c(1, 1, 0, 0, 1, 1, 0, 0)  # Smoking status (1 = smoking, 0 = non-smoking)
LungCancer <- c(1, 1, 0, 0, 1, 1, 0, 0)  # Lung cancer status (1 = yes, 0 = no)
Age <- c(60, 55, 65, 50, 70, 75, 80, 85)  # Age in years
# We can calculate the crude odds ratio (OR) of lung cancer for smoking status without considering age:
  # Crude Odds Ratio (without considering age)
  crude_OR <- (sum(Smoking == 1 & LungCancer == 1) * sum(Smoking == 0 & LungCancer == 0)) /
  (sum(Smoking == 0 & LungCancer == 1) * sum(Smoking == 1 & LungCancer == 0))
# Now, let's calculate the adjusted odds ratio (AOR) of lung cancer for smoking status, adjusting for age using logistic regression:
# Logistic Regression Model (adjusting for age)
model <- glm(LungCancer ~ Smoking + Age, family = binomial())

# Adjusted Odds Ratio (controlling for age)
adjusted_OR <- exp(coef(model)["Smoking"])



















