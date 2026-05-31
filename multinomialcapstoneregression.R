# running the multinomial regression. 

model_multinom_full <- multinom(Food_Insecurity ~
                                  Water_Insecurity +
                                  Urban_Rural+
                                  Age +
                                  Gender +
                                  Employment_Status +
                                  Income_Insecurity +
                                  Primary_Water_Source +
                                  Education_Level +
                                  P.Asset_Score,
                                data = SA_clean)
S(model_multinom_full)








model_multinom.reduced <- multinom(Food_Insecurity ~ 
                             Water_Insecurity +
                             Urban_Rural+
                             Income_Insecurity +
                             Education_Level +
                             P.Asset_Score,
                           data = SA_clean)

S(model_multinom.reduced)





#I am testing the full models between polr and multinom
library(lmtest)     # for lrtest

# Compare multinomial vs ordered logit
lrtest(model_multinom_full, model_polr.1)


library(pscl)
pR2(model_multinom.reduced)


library(effects)
# Generate effects object
effects_multinom <- allEffects(model_multinom.reduced)

# Plot all predictors
plot(effects_multinom,
     main = "Predictor Effects on Food Insecurity Levels",
     rug = TRUE,
     ci.style = "bars")

#Looking at effects by water Insecurity 
water_effect <- Effect("Water_Insecurity", model_multinom.reduced)

# Simple plot with clean lines
plot(water_effect,
     main = "Water Insecurity Effect on Food Insecurity",
     ci.style = "bands",
     lwd = 2,
     xlab = "Water Insecurity Level",
     ylab = "Predicted Probability")



























#Right now I am checking In sample predictive accuracy \
# Load required package
library(nnet)

#STEP 1 where we computing model accuracy 

# Step 3: Get predicted class labels
predicted_class <- predict(model_multinom.reduced)

# Step 4: Get actual class labels (now aligned with model input)
actual_class <- SA_clean$Food_Insecurity

# Step 5: Ensure lengths match
length(predicted_class) == length(actual_class)  # Should return TRUE

# Step 6: Compute mODEL accuracy
model_accuracy <- mean(predicted_class == actual_class)
round(model_accuracy, 3)


# Step 7: Null accuracy (modal class proportion)
null_accuracy <- max(prop.table(table(actual_class)))


# Step 8: PRE calculation
PRE <- (model_accuracy - null_accuracy) / (1 - null_accuracy)
round(PRE, 3)

















# I am now going to do out of sample validation 

install.packages("caret")
library(caret)
set.seed(123)  # for reproducibility

# Step 1: Stratified split (preserving class proportions)
train_index <- createDataPartition(SA_clean$Food_Insecurity, p = 0.7, list = FALSE)

# Step 2: Create training and test sets
train_data <- SA_clean[train_index, ]
test_data  <- SA_clean[-train_index, ]

# Step 3: Fit model on training data
model_multinom.train <- multinom(Food_Insecurity ~ 
                                   Water_Insecurity +
                                   Urban_Rural +
                                   Income_Insecurity +
                                   Education_Level +
                                   P.Asset_Score,
                                 data = train_data)
S(model_multinom.train)
# Step 4: Predict on test data
predicted_test <- predict(model_multinom.train, newdata = test_data)

# Step 5: Actual test labels
actual_test <- test_data$Food_Insecurity

# Step 6: Compute out-of-sample accuracy
test_accuracy <- mean(predicted_test == actual_test)
round(test_accuracy, 3)



#Now I want to see what are the actual predictions in terms of full data 
# Combine actual and predicted labels into a data frame
prediction_results <- data.frame(
  Actual = test_data$Food_Insecurity,
  Predicted = predicted_test
)

# View the first few rows
head(prediction_results)
tail(prediction_results)

# Count how many times each category was predicted
table(prediction_results$Predicted)


new_case <- data.frame(
  Water_Insecurity = factor("No water insecurity", 
                            levels = c("No water insecurity", "Low water insecurity", "High water insecurity")),
  Urban_Rural = factor("Urban", 
                       levels = c("Urban", "Rural")),
  Income_Insecurity = factor("High income insecurity", 
                             levels = c("No income insecurity", "Low income insecurity", "High income insecurity")),
  Education_Level = factor("Higher/Post-secondary education", 
                           levels = c("No formal schooling", "School education", "Higher/Post-secondary education")),
  P.Asset_Score = 4
)


# Predict the food insecurity category
predicted_category <- predict(model_multinom.train, newdata = new_case)
predicted_category






#trying to show the model effect on a graph 

library(broom)
library(ggplot2)
library(dplyr)

tidy_reduced <- tidy(model_multinom.reduced, conf.int = TRUE, exponentiate = TRUE)


# Remove intercepts
tidy_reduced <- tidy_reduced %>% filter(term != "(Intercept)")

#Plotting the grah

ggplot(tidy_reduced, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  facet_wrap(~ y.level, scales = "free_x") +
  labs(title = "Predictor Effects on Food Insecurity (RRR scale)",
       x = "Relative Risk Ratio (exp(beta))", y = "Predictor") +
  theme_minimal()
