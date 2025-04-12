# File:     04_Logistic_Regression.R
# Project:  Inferential Statistics
# Using the Donner Party survival data from lecture slides

# INSTALL AND LOAD PACKAGES ----------------------------------------------------
if (!require("BSDA")) install.packages("BSDA")      # Basic Statistics and Data Analysis
if (!require("ggplot2")) install.packages("ggplot2") # For advanced graphing
if (!require("broom")) install.packages("broom")     # To tidy model outputs
if (!require("car")) install.packages("car")         # For regression diagnostics
if (!require("dplyr")) install.packages("dplyr")     # For data manipulation
if (!require("pROC")) install.packages("pROC")       # For ROC curve analysis

# Load packages (order matters to prevent function conflicts)
library(dplyr)    
library(ggplot2)
library(broom)
library(car)
library(BSDA)
library(pROC)       

# ----------------------------
# 1. DATA PREP.
# ----------------------------

# Create dataset from lecture slides
donner_data <- data.frame(
  Age = c(23, 40, 40, 30, 28, 23, 24, 25, 45, 50, 55, 35),
  Sex = c(0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1),
  Status = c(0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1)
)

# View the data
cat("=== RAW DATA ===\n")
print(donner_data)
cat("\nSTRUCTURE:\n")
str(donner_data)

cat("\nEXPLANATION:\n")
cat("* The dataset contains 12 observations with:\n")
cat("  - Age: Numeric values (23 to 55 years)\n")
cat("  - Sex: Binary (0 = Male, 1 = Female)\n")
cat("  - Status: Binary outcome (0 = Died, 1 = Survived)\n")
cat("* All variables are properly formatted for logistic regression\n")

# ----------------------------
# 2. DATA ANALYSIS
# ----------------------------

# Calculate basic statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
stats <- donner_data %>%
  group_by(Sex) %>%
  summarize(
    Count = n(),
    Survival_Rate = mean(Status),
    Avg_Age = mean(Age)
  )
print(stats)

cat("\nEXPLANATION:\n")
cat("* For Males (Sex=0):\n")
cat("  - 8 individuals with 25% survival rate\n")
cat("  - Average age = 33.5 years\n")
cat("* For Females (Sex=1):\n")
cat("  - 4 individuals with 75% survival rate\n")
cat("  - Average age = 37.5 years\n")
cat("* Initial pattern: Females survived more despite being slightly older\n")

# Visualize the raw data
cat("\n=== RAW DATA PLOT ===\n")
ggplot(donner_data, aes(x = Age, y = Status, color = factor(Sex))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Male", "Female"),
                     name = "Sex") +
  labs(title = "Raw Survival Data by Age and Sex",
       y = "Survival Status (0 = Died, 1 = Survived)") +
  theme_minimal()

cat("\nEXPLANATION:\n")
cat("* The plot shows:\n")
cat("  - Red points = Males (mostly at bottom = died)\n")
cat("  - Blue points = Females (mostly at top = survived)\n")
cat("  - Visual pattern suggests both age and sex affect survival\n")

# ----------------------------
# 3. LOGISTIC MODEL 
# ----------------------------

# Fit logistic regression model
cat("\n=== MODEL FITTING ===\n")
model <- glm(Status ~ Age + Sex, data = donner_data, family = "binomial")

# Model summary
cat("\nMODEL SUMMARY:\n")
print(summary(model))

cat("\nEXPLANATION OF COEFFICIENTS:\n")
cat("* (Intercept) 1.55553: Log-odds of survival when Age=0 and Sex=0 (baseline)\n")
cat("* Age -0.08580: Each year decreases log-odds by 0.086 (p=0.325, not significant)\n")
cat("* Sex 2.90449: Females have 2.90 higher log-odds than males (p=0.111, not significant)\n")
cat("* Note: With small sample size (n=12), p-values may not reach significance\n")

# Odds ratios
cat("\nODDS RATIOS:\n")
cat("
=== Odds Ratios (OR) Interpretation ===

Why we exponentiate coefficients:
- Raw coefficients are in log-odds (hard to interpret)
- OR converts these to multiplicative effects (easier to understand)

Key Formula:
OR = exp(coefficient)

Example for Age:
Raw coefficient (log-odds): -0.08580
OR calculation: exp(-0.08580) ≈ 0.918

What this means:
- OR = 1: No effect
- OR < 1: Decreased odds (negative effect)
- OR > 1: Increased odds (positive effect)

Age OR = 0.918 interpretation:
- Each 1-year increase in age multiplies survival odds by 0.918
- Equivalently: 8.2% decrease in odds per year of age
")
tidy_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
print(tidy_results)

cat("\nEXPLANATION:\n")
cat("Age 0.918: For each year older, survival odds become 91.8% of what they were",
    "(an 8.2% decrease).\n\n",
    
    "Example: A 30-year-old has slightly worse odds than a 29-year-old.\n\n",
    
    "Sex 18.3: Females have 18× better odds than males!\n",
    "If males have 10% survival, similar females would have ~65% survival.\n\n",
    
    "The big warning signs:\n\n",
    
    "The confidence intervals are huge (especially for sex: 0.948–3110)\n",
    "→ This means we might see anywhere from slightly worse to thousands of times better survival for females!\n\n",
    
    "Both intervals include 1 (no effect):\n",
    "Age: 0.725–1.06 → Could be slightly harmful or slightly helpful\n",
    "Sex: 0.948–3110 → Could be no difference or massive difference\n")

# ----------------------------
# 4. MODEL INTERPRETATION
# ----------------------------

# Prediction function
calculate_survival <- function(age, sex) {
  log_odds <- coef(model)[1] + coef(model)[2]*age + coef(model)[3]*sex
  probability <- plogis(log_odds)
  return(probability)
}

# Example predictions
cat("\n=== SAMPLE PREDICTIONS ===\n")
cat("* 25yo male:", round(calculate_survival(25, 0)*100, 1), "% survival\n")
cat("* 25yo female:", round(calculate_survival(25, 1)*100, 1), "% survival\n") 
cat("* 40yo male:", round(calculate_survival(40, 0)*100, 1), "% survival\n")

cat("\nEXPLANATION:\n")
cat("* Strong apparent gender effect: 91% vs 35.7% at age 25\n")
cat("* Strong apparent age effect: 35.7% at 25yo vs 13.3% at 40yo for males\n")
cat("* Note these effects are not statistically significant with this small sample\n")

# ----------------------------
# 5. MODEL EVALUATION
# ----------------------------

# Add predicted probabilities to our dataset
donner_data$Predicted_Prob <- predict(model, type = "response")

# Confusion matrix at 0.5 threshold
cat("\n=== CONFUSION MATRIX ===\n")
conf_matrix <- table(
  Actual = donner_data$Status,
  Predicted = ifelse(donner_data$Predicted_Prob > 0.5, 1, 0)
)
print(conf_matrix)

# Calculate evaluation metrics
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
sensitivity <- conf_matrix[2,2]/sum(conf_matrix[2,])
specificity <- conf_matrix[1,1]/sum(conf_matrix[1,])

cat("\nPERFORMANCE METRICS:\n")
cat("* Accuracy:", round(accuracy, 3), "\n")
cat("* Sensitivity:", round(sensitivity, 3), "\n")
cat("* Specificity:", round(specificity, 3), "\n")

cat("\nEXPLANATION:\n")
cat("* 75% overall accuracy\n")
cat("* Better at identifying deaths (85.7% specificity) than survivals (60% sensitivity)\n")
cat("* Trade-off between error types can be adjusted via threshold\n")

# ROC Curve analysis
cat("\nROC: a graphical representation that plots:\n")
cat("* True positive rate (sensitivity) against\n")
cat("* False positive rate (1 - specificity)\n")
cat("* At various thresholds for a binary classifier\n")

cat("\n=== ROC CURVE ANALYSIS ===\n")
roc_obj <- roc(donner_data$Status, donner_data$Predicted_Prob)
plot(roc_obj, main = "ROC Curve for Survival Prediction",
     print.auc = TRUE, auc.polygon = TRUE)

cat("\nEXPLANATION:\n")
cat("* AUC = 0.8125 (good discrimination despite non-significant coefficients)\n")
cat("* Curve shape shows moderate separation between classes\n")
cat("* Better than random guessing (AUC=0.5)\n")

cat("\nArea Under the ROC Curve (AUC):\n")
cat("* Quantifies the model's ability to distinguish outcomes\n")
cat("* Higher AUC indicates better performance\n")
cat("* AUC=1 represents a perfect classifier\n")
cat("* Summarizes performance across all classification thresholds\n")

# ----------------------------
# 6. VISUALIZATION
# ----------------------------

# Create prediction grid for visualization
pred_grid <- expand.grid(
  Age = seq(min(donner_data$Age), max(donner_data$Age), length.out = 100),
  Sex = c(0, 1)
)
pred_grid$Prob <- predict(model, newdata = pred_grid, type = "response")

# Plot survival probability curves
cat("\n=== FINAL VISUALIZATION ===\n")
ggplot(pred_grid, aes(x = Age, y = Prob, color = factor(Sex))) +
  geom_line(size = 1.5) +
  geom_point(data = donner_data, aes(y = Status), size = 2) +
  scale_color_manual(values = c("red", "blue"), 
                     labels = c("Male", "Female"),
                     name = "Sex") +
  labs(title = "Logistic Regression Model Predictions",
       subtitle = "Points show actual outcomes, lines show predicted probabilities",
       y = "Probability of Survival",
       x = "Age") +
  theme_minimal() +
  theme(legend.position = "bottom")

cat("\nEXPLANATION:\n")
cat("* Clear S-shaped logistic curves\n")
cat("* Females (blue) have higher probabilities at all ages\n")
cat("* Points align reasonably well with curves given small sample size\n")
cat("* Visual confirmation of age and sex effects despite non-significance\n")

# ----------------------------
# 7. CONCLUSION
# ----------------------------

cat("\n=== KEY FINDINGS ===\n")
cat("* 1. Apparent age effect: Older individuals had lower survival odds\n")
cat("    (not statistically significant)\n")
cat("* 2. Apparent sex effect: Females had higher survival odds\n")
cat("    (not statistically significant)\n")  
cat("* 3. Model's AUC of", round(auc(roc_obj), 2), "shows good predictive ability\n")
cat("    despite small sample\n")
cat("* 4. Visualizations suggest age and sex effects that may reach\n")
cat("    significance with larger sample\n")