# File:     02_Regression.R
# Project:  Inferential Statistics


# INSTALL AND LOAD PACKAGES ----------------------------------------------------
# Install packages only if needed (prevents duplicate installations)
if (!require("BSDA")) install.packages("BSDA")      # Basic Statistics and Data Analysis
if (!require("ggplot2")) install.packages("ggplot2") # For advanced graphing
if (!require("broom")) install.packages("broom")     # To tidy model outputs
if (!require("car")) install.packages("car")         # For regression diagnostics
if (!require("dplyr")) install.packages("dplyr")     # For data manipulation

# Load packages (plz mind order here. It matters to prevent function conflicts)
library(dplyr)    # Load first to avoid function masking
#Some functions with the same name exist in different packages
library(ggplot2)
library(broom)
library(car)
library(BSDA)

# Z-SCORE ANALYSIS ------------------------------------------------------------
# Many ways to calculate Z Score
# Here: two ways to calculate z-scores and p-values

# Sample data (IQ scores from a small class)
sample_iq <- c(102, 115, 98, 107, 110)
sample_iq
population_mean <- 100  # Let's assume this is our average (mean) IQ
population_mean

# METHOD 1: Manual calculation (good for understanding the math)
#This is what you are doing in you HW
z_manual <- (mean(sample_iq) - population_mean) / (sd(sample_iq)/sqrt(length(sample_iq)))
z_manual

p_manual <- 2*pnorm(-abs(z_manual))  # Two-tailed test
p_manual

print(paste("Manual z-score:", round(z_manual, 2), 
            "p-value:", format.pval(p_manual, digits = 3)))

#boxplot(sample_iq, ylab = "IQ"); abline(h = 100, col = "red")

# METHOD 2: Using BSDA package (easier for real analysis)
z_test <- z.test(sample_iq, mu = population_mean, sigma.x = sd(sample_iq))

print(z_test)


# BASIC STATISTICAL TESTS -----------------------------------------------------
# These examples show common tests we have been using

# One-sample t-test: Compare sample mean to known value
t.test(sample_iq, mu = 100)  # Is our class different from national average?

# Optional #################################################
qqnorm(sample_iq); qqline(sample_iq) #Check normality
# ===> Cohen's d quantifies the standardized effect size, showing how many standard deviations apart two means are
# (e.g., our sample IQ mean vs. population mean), with 0.2 = small, 0.5 = medium, 0.8+ = large effects.
cohens_d <- (mean(sample_iq) - 100)/sd(sample_iq) 
cohens_d

# Power analysis determines the sample size needed to reliably detect an effect of a given size 
# (like our d = 0.98) at a specified significance level (e.g., α = 0.05) and desired power (typically 80%).

power.t.test(delta = 6.4, sd = 6.54, sig.level = 0.05, power = 0.8) 
# ===>Together, they reveal whether non-significant results (like our p = 0.098) reflect trivial effects
# or insufficient sample size — in our case, the large Cohen’s d suggests the test was underpowered
# (n = 5 is likely too small to detect the real effect).

####################################################################

# Two-sample t-test: Compare two independent groups
group_a <- c(18, 22, 19, 25)  # Test scores from morning class
group_b <- c(25, 29, 26, 30)   # Test scores from afternoon class

t.test(group_a, group_b)         # Default Welch's test (unequal variances)

t.test(group_a, group_b, var.equal = TRUE)  # Student's t-test/classical

# Chi-square goodness-of-fit test
observed_counts <- c(20, 30, 50)  # Observed counts in 3 categories
expected_props <- c(0.3, 0.3, 0.4) # Expected proportions
chisq.test(observed_counts, p = expected_props)

# ===> p > 0.05 → Fail to reject the null (no significant deviation from expected proportions).
#But it’s marginally significant (close to 0.05).

#Chi-Square Test of Independence 

data(mtcars) # Load data (imbedded in R)

contingency_table <- table(mtcars$am, mtcars$cyl)  # # Create a contingency table
contingency_table

#Null Hypothesis: am & cyl are independent (no association).
#Alternative Hypothesis: am & cyl are dependent (associated).

chi_test <- chisq.test(contingency_table)
chi_test
# ===> Transmission type and cylinder count are associated (not independent).


# One-way ANOVA: Compare means across multiple groups
data(PlantGrowth)  # Built-in dataset of plant weights under 3 conditions
plant_model <- aov(weight ~ group, data = PlantGrowth)
summary(plant_model)  # Overall significance
TukeyHSD(plant_model)  # Which specific groups differ?

#Assumption Checks
shapiro.test(residuals(plant_model))
#Homogeneity of variance
car::leveneTest(weight ~ group, data = PlantGrowth)

#Visual Check
boxplot(weight ~ group, data = PlantGrowth, col = "lightblue")
qqnorm(residuals(plant_model))  
qqline(residuals(plant_model))  # Points should follow the line

# Correlation tests
cor.test(mtcars$mpg, mtcars$hp)  # Default Pearson correlation
cor.test(mtcars$mpg, mtcars$hp, method = "spearman")  # Non-parametric version

plot(mtcars$hp, mtcars$mpg, 
     xlab = "Horsepower", ylab = "MPG",
     main = "MPG vs. Horsepower")
abline(lm(mpg ~ hp, data = mtcars), col = "red")  # Regression line



# LINEAR REGRESSION ---------------------------------------------
# Goal: Show how outliers affect regression lines and how to diagnose problems

set.seed(123)  # For reproducibility

# 1. Create clean linear relationship (strong pattern)
clean_data <- data.frame(
  x = rnorm(50, mean = 10, sd = 2),
  y = 10 + 2 * rnorm(50, mean = 10, sd = 1)  # Stronger linear relationship
)

# 2. Add strategic outliers
outliers <- data.frame(
  x = c(5, 20, 18, 10, 15),  # Carefully chosen x-values
  y = c(3, 5, 35, 40, 10),    # Extreme y-values where needed
  type = c("High Leverage", "Horizontal", "Influential", "Vertical", "Regular")
)

# 3. Combine data
full_data <- bind_rows(
  clean_data %>% mutate(type = "Normal"),
  outliers
)

# 4. Enhanced visualization
ggplot(full_data, aes(x, y)) +
  # Clean data regression (thick blue line)
  geom_smooth(data = filter(full_data, type == "Normal"),
              method = "lm", se = FALSE, 
              color = "blue", linewidth = 1.5) +
  # Contaminated regression (thick dashed red line)
  geom_smooth(data = full_data, method = "lm", se = FALSE,
              color = "red", linetype = "dashed", linewidth = 1.5) +
  # Color-coded points with custom shapes
  geom_point(aes(color = type, shape = type), size = 4, alpha = 0.8) +
  # Emphasize influential points
  geom_point(data = filter(full_data, type %in% c("Influential", "High Leverage")),
             aes(x, y), color = "black", size = 5, shape = 1, stroke = 1.5) +
  # Labels and theme
  labs(title = "How Influential Points Distort Regression",
       subtitle = "Blue: True relationship (no outliers)\nRed: Contaminated by outliers",
       x = "Predictor Variable", y = "Outcome Variable",
       color = "Point Type", shape = "Point Type") +
  theme_minimal() +
  scale_color_manual(values = c("High Leverage" = "#E41A1C", 
                                "Influential" = "#FF7F00",
                                "Normal" = "#377EB8",
                                "Regular" = "#4DAF4A",
                                "Vertical" = "#984EA3",
                                "Horizontal" = "#A65628")) +
  theme(legend.position = "bottom")

# 5. Show numerical impact
cat("Clean model coefficients:\n")
lm(y ~ x, data = filter(full_data, type == "Normal")) %>% coefficients()
cat("\nContaminated model coefficients:\n")
lm(y ~ x, data = full_data) %>% coefficients()


# ===>	The clean model (only normal points) suggests no strong trend between x and y
#The contaminated model (with outliers) gives a misleading result —
# it suggests a strong negative trend that doesn’t actually exist in the normal data
# ===>This proves why it’s important to diagnose and handle outliers in regression analysis


# TWINS CASE STUDY: Nature vs. Nurture (Inspired by Cyril Burt's 1966 study)
# Goal: Simulate IQ scores of identical twins raised apart to explore genetic influence on intelligence
#Does a child’s biological IQ (nature) predict their IQ after being raised by a foster family (nurture)? 

# Set seed for reproducibility
set.seed(456)

# Simulate data: 27 monozygotic twin pairs (one raised by biological, one by foster parents)
# Assuming IQs are normally distributed and positively correlated due to shared genetics

# Step 1: Generate IQ for Twin A (biological home)
biological_IQ <- round(rnorm(27, mean = 95.3, sd = 15.73))
biological_IQ

# Step 2: Generate IQ for Twin B (foster home), correlated with Twin A
# Use Twin A's IQ + noise to reflect shared genetics + environmental differences
foster_IQ <- round(biological_IQ * 0.9 + rnorm(27, mean = 9.2, sd = 7.7))
foster_IQ

# Step 3: Create data frame
twins <- data.frame(
  BiologicalIQ = biological_IQ,
  FosterIQ = foster_IQ
)

twins

# Step 4: Fit linear regression model
twin_model <- lm(FosterIQ ~ BiologicalIQ, data = twins)
twin_model
#We get: FosterIQ = 2.46 + 0.97 × BiologicalIQ
#For every 1-point increase in Biological IQ, Foster IQ increases by 0.97 points, on average 
#This suggests a strong, nearly 1:1 relationship, indicating that biological (genetic) IQ strongly predicts foster IQ
#i.e., Twins raised apart still show a strong similarity in IQ

# Step 5: View model summary and tidy output
summary(twin_model)                          # Standard model summary
tidy(twin_model, conf.int = TRUE)            # Neater coefficient table with CI
glance(twin_model)                           # Model-level stats (R-squared, AIC, etc.)

#Visualizing it
ggplot(twins, aes(BiologicalIQ, FosterIQ)) +
  # Data points with slight transparency
  geom_point(size = 3, alpha = 0.7, color = "#4E79A7") +
  
  # Regression line with confidence interval
  geom_smooth(method = "lm", color = "#E15759", 
              fill = "#F28E2B", alpha = 0.2, 
              linewidth = 1.5) +
  
  # Perfect correlation reference line
  geom_abline(slope = 1, intercept = 0, 
              linetype = "dashed", color = "gray40",
              linewidth = 0.8) +
  
  # Key annotations
  annotate("text", x = 70, y = 135, 
           label = paste0("Foster IQ = ", round(coef(twin_model)[1],1), 
                          " + ", round(coef(twin_model)[2],2), " × Biological IQ"),
           hjust = 0, size = 4.5) +
  annotate("text", x = 70, y = 128,
           label = paste0("R² = ", round(glance(twin_model)$r.squared, 3), 
                          ", p < 0.001"),
           hjust = 0, size = 4.5) +
  
  # Formatting
  labs(title = "Genetic Influence on IQ in Separated Twins",
       subtitle = "Each point represents a twin pair raised apart",
       x = "Biological Twin IQ",
       y = "Foster Twin IQ",
       caption = "Dashed line shows perfect 1:1 correlation") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  coord_fixed(ratio = 1, xlim = c(60, 140), ylim = c(60, 140)) +
  scale_x_continuous(breaks = seq(60, 140, 20)) +
  scale_y_continuous(breaks = seq(60, 140, 20))


# REGRESSION DIAGNOSTICS -----------------------------------------------------
# Always check these before trusting your model results!

# 1. Residual plots (check linearity, constant variance)
plot(twin_model, which = 1)  # Residuals vs fitted values/General diagnostic

# 2. Q-Q plot (check normality of residuals)
plot(twin_model, which = 2)  

# 3. Scale-location plot (check homoscedasticity)
plot(twin_model, which = 3) #Purely variance (homoscedasticity)

# 4. Cook's distance (identify influential points)
plot(twin_model, which = 4)

# ===> Observations 6, 18, and 22 have relatively high Cook’s distances
#suggests they may have a disproportionate influence on the regression model 
#should be examined more closely


# Formal tests for assumptions
ncvTest(twin_model)  # Breusch-Pagan test for constant variance
# ===> The p-value is very high (0.83), so we fail to reject the null hypothesis.
#This means there is no evidence of heteroscedasticity
#Residuals appear to have constant variance

outlierTest(twin_model)  # Bonferroni outlier test

# ===>Although observation 18 had the largest studentized residual
#Bonferroni-adjusted p-value shows it’s not statistically significant
#confirms that the model has no influential outliers affecting its validity.


# TWIN STUDY ANALYSIS: Confidence vs Prediction Intervals

# 1. THE TWINS DATASET 
# -----------------------------------------------------------
set.seed(456)
biological_IQ <- round(rnorm(27, mean = 95.3, sd = 15.73))
foster_IQ <- round(biological_IQ * 0.9 + rnorm(27, mean = 9.2, sd = 7.7))

twins <- data.frame(
  BiologicalIQ = biological_IQ,
  FosterIQ = foster_IQ
)

# 2. FIT THE REGRESSION MODEL
# -----------------------------------------------------------
twin_model <- lm(FosterIQ ~ BiologicalIQ, data = twins)

# 3. CREATE PREDICTION DATA
# -----------------------------------------------------------
# Create sequence of BiologicalIQ values for prediction
prediction_data <- data.frame(
  BiologicalIQ = seq(
    from = min(twins$BiologicalIQ) - 5,  # Slightly below minimum observed
    to = max(twins$BiologicalIQ) + 5,     # Slightly above maximum observed
    length.out = 100                      # Smooth curve
  )
)

# 4. CALCULATE INTERVALS
# -----------------------------------------------------------
# Get confidence intervals (for the mean response)
conf_ints <- predict(twin_model, newdata = prediction_data, 
                     interval = "confidence") %>% 
  as.data.frame() %>%
  rename(CI_fit = fit, CI_lwr = lwr, CI_upr = upr)

# Get prediction intervals (for individual observations)
pred_ints <- predict(twin_model, newdata = prediction_data,
                     interval = "prediction") %>%
  as.data.frame() %>%
  rename(PI_fit = fit, PI_lwr = lwr, PI_upr = upr)

# Combine all predictions
plot_data <- cbind(prediction_data, conf_ints, pred_ints[,2:3])

# 5. CREATE THE PLOT
# -----------------------------------------------------------
ggplot(plot_data, aes(x = BiologicalIQ)) +
  
  # Original data points
  geom_point(data = twins, 
             aes(x = BiologicalIQ, y = FosterIQ),
             size = 3, alpha = 0.7, color = "#4E79A7") +
  
  # Regression line
  geom_line(aes(y = CI_fit), color = "#E15759", linewidth = 1.5) +
  
  # Confidence interval (mean response)
  geom_ribbon(aes(ymin = CI_lwr, ymax = CI_upr),
              fill = "#F28E2B", alpha = 0.3) +
  
  # Prediction interval (individual observations)
  geom_ribbon(aes(ymin = PI_lwr, ymax = PI_upr),
              fill = "#76B7B2", alpha = 0.2) +
  
  # Annotations
  annotate("text", 
           x = min(twins$BiologicalIQ) + 5, 
           y = max(twins$FosterIQ) - 5,
           label = "95% CI for mean response",
           color = "#F28E2B", hjust = 0, size = 4.5) +
  
  annotate("text", 
           x = min(twins$BiologicalIQ) + 5, 
           y = min(twins$FosterIQ) + 15,
           label = "95% PI for individuals",
           color = "#76B7B2", hjust = 0, size = 4.5) +
  
  # Formatting
  labs(title = "Genetic Influence on IQ in Separated Twins",
       subtitle = paste0("FosterIQ = ", round(coef(twin_model)[1], 1), 
                         " + ", round(coef(twin_model)[2], 2), 
                         " × BiologicalIQ (R² = ", 
                         round(summary(twin_model)$r.squared, 2), ")"),
       x = "Biological Twin IQ", 
       y = "Foster Twin IQ",
       caption = "Shaded regions show 95% confidence (orange) and prediction (teal) intervals") +
  
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray30"),
        panel.grid.minor = element_blank()) +
  
  coord_cartesian(ylim = c(min(twins$FosterIQ) - 10, max(twins$FosterIQ) + 10))


# Some INSIGHTS
# -----------------------------------------------------------
# - The NARROWER orange band (confidence interval) shows uncertainty about the MEAN Foster IQ
# - The WIDER teal band (prediction interval) shows expected range for INDIVIDUAL Foster IQs
# - The gap between bands illustrates the additional uncertainty when predicting individuals vs group means
# - All predictions become less precise (wider intervals) as we move further from the data center


# COMPARE MODELS WITH/WITHOUT OUTLIERS
# -----------------------------------------------------------
# Goal: Demonstrate how outlier removal affects regression results

# 1. IDENTIFY OUTLIERS
# Using standardized residuals > 2 SDs as cutoff
outliers <- which(abs(rstandard(twin_model)) > 2)
cat("Identified outliers at positions:", outliers, "\n")

# 2. PREPARE DATA WITH OUTLIER FLAG
twins_flagged <- twins %>%
  mutate(Outlier = ifelse(row_number() %in% outliers, "Outlier", "Normal"))

# 3. FIT COMPARISON MODELS
original_model <- twin_model  # Our existing model
clean_model <- lm(FosterIQ ~ BiologicalIQ, data = filter(twins_flagged, Outlier == "Normal"))

# 4. COMPARE MODEL STATISTICS
model_comparison <- bind_rows(
  glance(original_model) %>% mutate(Model = "With Outliers"),
  glance(clean_model) %>% mutate(Model = "Without Outliers")
) %>% 
  select(Model, r.squared, adj.r.squared, sigma, AIC, BIC) %>%
  mutate(across(where(is.numeric), round, 3))

print(model_comparison)

# 5. VISUAL COMPARISON
ggplot(twins_flagged, aes(x = BiologicalIQ, y = FosterIQ)) +
  # All data points with outlier coloring
  geom_point(aes(color = Outlier), size = 3, alpha = 0.8) +
  
  # Original regression line
  geom_smooth(method = "lm", se = FALSE, 
              color = "#377EB8", linewidth = 1.2) +
  
  # Clean regression line
  geom_smooth(data = filter(twins_flagged, Outlier == "Normal"), 
              method = "lm", se = FALSE,
              color = "#E41A1C", linetype = "dashed", linewidth = 1.2) +
  
  # Highlight outliers with rings
  geom_point(data = filter(twins_flagged, Outlier == "Outlier"),
             shape = 1, size = 4, stroke = 1.5, color = "black") +
  
  # Formatting
  labs(title = "Impact of Outliers on Regression Results",
       subtitle = paste("Blue: All data (n =", nrow(twins), 
                        ")\nRed: Outliers removed (n =", 
                        sum(twins_flagged$Outlier == "Normal"), ")"),
       x = "Biological IQ", 
       y = "Foster IQ",
       color = "Point Type") +
  scale_color_manual(values = c("Outlier" = "#FF7F00", 
                                "Normal" = "#4DAF4A")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

cat("\014")  # Clear console (like Ctrl+L)

#End of Lab 3
