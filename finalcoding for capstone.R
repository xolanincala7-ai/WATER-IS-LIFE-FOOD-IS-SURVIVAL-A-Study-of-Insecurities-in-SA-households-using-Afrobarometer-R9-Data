library(haven)
library(car)

# Here I load the full Afrobarometer South Africa Round 09 data 
SA.R9.data <- read_sav("SAF_R9.data.sav")



#  now i will be creating a data frame with the variables that i will be needing for this assignment.


library(dplyr)

SA.R9.var <- SA.R9.data %>%
  dplyr::select(RESPNO, URBRUR, Q6A, Q6B, Q1, Q100, Q93A, Q6E, Q91B, Q94, Q90A, Q90B, Q90C, Q90F)

summary(SA.R9.var)
head(SA.R9.var)

# The data processing and tranformations begins.
#Water insecurity 0 to 4 (low to highest)

# AGE RECODING NAS LEAVING It AS NUMERIC 
SA.R9.var$Q1[SA.R9.var$Q1 %in% c(-1, 998, 999)] <- NA

SA.R9.var$Q1 <- as.numeric(SA.R9.var$Q1)

# Employment rECODING NAs and the levels

SA.R9.var$Q93A <- with(SA.R9.var, ifelse(Q93A %in% c(-1, 8, 9), NA,  # Missing
                                         ifelse(Q93A %in% c(0, 1), 0,             # Unemployed
                                                ifelse(Q93A == 2, 1,                     # Yes, part time
                                                       ifelse(Q93A == 3, 2, NA)))))

# Income availability recoding NAs and making the measure a interval scale 

SA.R9.var$Q6E[SA.R9.var$Q6E %in% c(-1, 8, 9)] <- NA


## ASSET oWNERSHIP Binary Indicatoras Yes or no Ownership
SA.R9.var$Q90A<- ifelse(SA.R9.var$Q90A %in% c(1, 2), 1, 0)

SA.R9.var$Q90B <- ifelse(SA.R9.var$Q90B %in% c(1, 2), 1, 0)

SA.R9.var$Q90C <- ifelse(SA.R9.var$Q90C %in% c(1, 2), 1, 0)

SA.R9.var$Q90F <- ifelse(SA.R9.var$Q90F %in% c(1, 2), 1, 0)

# we handle the missing values 

SA.R9.var$Q90A[SA.R9.var$Q90A %in% c(-1, 8, 9)] <- NA

SA.R9.var$Q90B[SA.R9.var$Q90B %in% c(-1, 8, 9)] <- NA

SA.R9.var$Q90C[SA.R9.var$Q90C %in% c(-1, 8, 9)] <- NA

SA.R9.var$Q90F[SA.R9.var$Q90F %in% c(-1, 8, 9)] <- NA


SA.R9.var$asset_ownership <- rowSums(SA.R9.var[, c("Q90A", "Q90B", "Q90C", "Q90F")], na.rm = FALSE)



### LEVEL OF EDUCATION 

# Recode Q94 into a three-level ordinal scale
# Step 1: Create a new numeric variable for the recoded education levels
SA.R9.var$Q94<- with(SA.R9.var,
                     ifelse(Q94 %in% c(-1, 98, 99), NA,
                            ifelse(Q94 %in% c(0, 1), 0,  # No formal schooling
                                   ifelse(Q94 %in% c(2, 3, 4, 5), 1,  # School education
                                          ifelse(Q94 %in% c(6, 7, 8, 9), 2, NA)))))  # Higher education

# Step 2: Convert to an ordered factor with descriptive labels
SA.R9.var$Q94 <- factor(SA.R9.var$Q94,
                        levels = 0:2,
                        labels = c("No formal schooling",
                                   "School education",
                                   "Higher/Post-secondary education"))
# Now I am recoding the dependent variable and removing unwanted levels

# Recode non-response values to NA
# Step 1: Recode non-response values to NA
SA.R9.var$Q6A[SA.R9.var$Q6A %in% c(8, 9, -1)] <- NA

# Step 2: Use ifelse to assign labels
SA.R9.var$Q6A <- ifelse(SA.R9.var$Q6A == 0, "No food insecurity",
                        ifelse(SA.R9.var$Q6A %in% c(1, 2), "Low food insecurity",
                               ifelse(SA.R9.var$Q6A %in% c(3, 4), "High food insecurity", NA)))

# Step 3: Convert to ordered factor
SA.R9.var$Q6A <- factor(SA.R9.var$Q6A,
                        levels = c("No food insecurity",
                                   "Low food insecurity",
                                   "High food insecurity"))

#now I am recoding the the water source variable 

# Recode Q91B
SA.R9.var$Q91B <- with(SA.R9.var,
                       ifelse(Q91B %in% c(8, 9, -1), NA,
                              ifelse(Q91B %in% c(1, 2), 0,  # Inside compound
                                     ifelse(Q91B == 3, 1, NA))))  # Outside compound

# Optional: Add descriptive labels
SA.R9.var$Q91B <- factor(SA.R9.var$Q91B,
                         levels = c(0, 1),
                         labels = c("Internal", "External"))


# Now removing all NAs

SA.R9.var <- na.omit(SA.R9.var)


# coding the urban rural 

SA.R9.var$URBRUR <- factor(
  ifelse(SA.R9.var$URBRUR == 1, 0,
         ifelse(SA.R9.var$URBRUR == 2, 1, NA)),
  levels = c(0, 1),
  labels = c("Urban", "Rural"))
table(SA.R9.var$URBRUR)


# running regression 

SA.R9.var$Q6A <- factor(SA.R9.var$Q6A)

# Convert categorical predictors to factors
SA.R9.var$Q91B <- factor(SA.R9.var$Q91B)
SA.R9.var$Q94 <- factor(SA.R9.var$Q94)


# i am going to label water insecurity as factor for clarity
# Recode non-response values to NA
SA.R9.var$Q6B[SA.R9.var$Q6B %in% c(8, 9, -1)] <- NA

# Recode water insecurity levels
# Create a new variable to preserve original
# Step 1: Recode non-response values to NA
SA.R9.var$Q6B[SA.R9.var$Q6B %in% c(8, 9, -1)] <- NA

# Step 2: Use nested ifelse to assign labels
SA.R9.var$Q6B <- ifelse(SA.R9.var$Q6B == 0, "No water insecurity",
                        ifelse(SA.R9.var$Q6B %in% c(1, 2), "Low water insecurity",
                               ifelse(SA.R9.var$Q6B %in% c(3, 4), "High water insecurity", NA)))

# Step 3: Convert to ordered factor
SA.R9.var$Q6B <- factor(SA.R9.var$Q6B,
                        levels = c("No water insecurity",
                                   "Low water insecurity",
                                   "High water insecurity"))






# I am going to lable gender as factor 
SA.R9.var$Q100 <- factor(SA.R9.var$Q100,
                         levels = c(1, 2),
                         labels = c("Man", "Woman"))

# I am recoding the unemployment as factor 

SA.R9.var$Q93A <- factor(SA.R9.var$Q93A,
                         levels = c(0, 1, 2),
                         labels = c("Unemployed", "Employed part time", "Employed full time"))

# I am cash recoding income as factor 
# Recode non-response values to NA
SA.R9.var$Q6E[SA.R9.var$Q6E %in% c(8, 9, -1)] <- NA

# Recode income insecurity levels
# Create a new variable to preserve original
# Step 1: Recode non-response values to NA
SA.R9.var$Q6E[SA.R9.var$Q6E %in% c(8, 9, -1)] <- NA

# Step 2: Use nested ifelse to assign labels
SA.R9.var$Q6E <- ifelse(SA.R9.var$Q6E == 0, "No income insecurity",
                        ifelse(SA.R9.var$Q6E %in% c(1, 2), "Low income insecurity",
                               ifelse(SA.R9.var$Q6E %in% c(3, 4), "High income insecurity", NA)))

# Step 3: Convert to ordered factor
SA.R9.var$Q6E <- factor(SA.R9.var$Q6E,
                        levels = c("No income insecurity",
                                   "Low income insecurity",
                                   "High income insecurity"))





# Create a new data frame with selected and renamed columns
SA_clean <- SA.R9.var[, c("RESPNO", "URBRUR", "Q6A", "Q6B", "Q1", "Q100", "Q93A", "Q6E", "Q91B", "Q94", "asset_ownership")]

# Rename columns for clarity
names(SA_clean) <- c("Respondent_ID",
                     "Urban_Rural",
                     "Food_Insecurity",
                     "Water_Insecurity",
                     "Age",
                     "Gender",
                     "Employment_Status",
                     "Income_Insecurity",
                     "Primary_Water_Source",
                     "Education_Level",
                     "P.Asset_Score")


SA_clean <- na.omit(SA_clean)


#Now creating the descriptive 

library(dplyr)
library(knitr)
library(tidyr)
library(tinytex)
#Fixing numeric descriptive stats 
numeric_summary <- SA_clean %>%
  summarise(
    Age = paste0(round(mean(Age, na.rm = TRUE), 1), " ± ", round(sd(Age, na.rm = TRUE), 1)),
    P.Asset_Score = paste0(round(mean(P.Asset_Score, na.rm = TRUE), 2), " ± ", round(sd(P.Asset_Score, na.rm = TRUE), 2))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "N (%) or Mean ± SD") %>%
  mutate(Category = "")

# Fixing categorical Stats 
library(dplyr)

cat_vars <- c("Food_Insecurity", "Water_Insecurity", "Income_Insecurity",
              "Urban_Rural", "Education_Level", "Gender", "Employment_Status",
              "Primary_Water_Source")

summary_table <- lapply(cat_vars, function(var) {
  SA_clean %>%
    group_by(!!sym(var)) %>%
    summarise(N = n()) %>%
    mutate(Percent = round(100 * N / sum(N), 1),
           Variable = var) %>%
    rename(Category = !!sym(var))
}) %>%
  bind_rows() %>%
  mutate(`N (%) or Mean ± SD` = paste0(N, " (", Percent, "%)")) %>%
  dplyr::select(Variable, Category, `N (%) or Mean ± SD`) %>%
  group_by(Variable) %>%
  mutate(Variable = ifelse(row_number() == 1, Variable, "")) %>%
  ungroup()

final_table <- bind_rows(numeric_summary, summary_table)

# Total number of observations
total_n <- nrow(SA_clean)

# Create a row for total sample size
total_row <- data.frame(
  Variable = "Total sample size",
  Category = "",
  `N (%) or Mean ± SD` = total_n,
  stringsAsFactors = FALSE
)

# Bind it at the top of the table
final_table <- bind_rows(numeric_summary, summary_table)

#adding number of observations 

total_row_bottom <- tibble(
  Variable = "Total Sample Size",
  Category = "",
  `N (%) or Mean ± SD` = "1542"
)

final_table <- bind_rows(final_table, total_row_bottom)


library(knitr)
kable(final_table, caption = "Sample Characteristics of Respondents")

library(knitr)
library(kableExtra)

# Export table as LaTeX code
kable(final_table,
      format = "latex",
      booktabs = TRUE,
      caption = "Sample Characteristics of Respondents. Source: Afrobarometer Survey Round 9 - South Africa") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable("sample_characteristics.tex")

#Checking the bivariate associations and statistics 


library(vcd)
library(dplyr)
library(knitr)
library(kableExtra)



# Define your predictors
predictors <- c("Water_Insecurity", "Gender", "Urban_Rural", 
                "Education_Level", "Employment_Status", 
                "Income_Insecurity")

# Function to calculate Cramér’s V and p-value
get_assoc_stats <- function(var, outcome, data) {
  tab <- table(data[[var]], data[[outcome]])
  cramer <- assocstats(tab)$cramer
  p <- chisq.test(tab)$p.value
  strength <- case_when(
    cramer < 0.1 ~ "Negligible",
    cramer < 0.2 ~ "Weak",
    cramer < 0.4 ~ "Moderate",
    cramer < 0.6 ~ "Strong",
    TRUE ~ "Very Strong"
  )
  sig <- ifelse(p < 0.05, "Significant", "Not Significant")
  return(data.frame(
    Predictor = var,
    Cramers_V = round(cramer, 3),
    P_value = signif(p, 3),
    Strength = strength,
    Significance = sig
  ))
}

# Generate the summary table
assoc_summary <- do.call(rbind, lapply(predictors, get_assoc_stats, outcome = "Food_Insecurity", data = SA_clean))

# View the table in R
print(assoc_summary)



# Age
cor_age <- cor.test(SA_clean$Age, as.numeric(SA_clean$Food_Insecurity), method = "pearson")

# Asset Score
cor_asset <- cor.test(SA_clean$P.Asset_Score, as.numeric(SA_clean$Food_Insecurity), method = "pearson")

assoc_numeric <- bind_rows(
  data.frame(
    Predictor = "Age",
    Cramers_V = round(cor_age$estimate, 3),
    P_value = signif(cor_age$p.value, 3),
    Strength = case_when(
      abs(cor_age$estimate) < 0.1 ~ "Negligible",
      abs(cor_age$estimate) < 0.2 ~ "Weak",
      abs(cor_age$estimate) < 0.4 ~ "Moderate",
      abs(cor_age$estimate) < 0.6 ~ "Strong",
      TRUE ~ "Very Strong"
    ),
    Significance = ifelse(cor_age$p.value < 0.05, "Significant", "Not Significant")
  ),
  data.frame(
    Predictor = "P.Asset_Score",
    Cramers_V = round(cor_asset$estimate, 3),
    P_value = signif(cor_asset$p.value, 3),
    Strength = case_when(
      abs(cor_asset$estimate) < 0.1 ~ "Negligible",
      abs(cor_asset$estimate) < 0.2 ~ "Weak",
      abs(cor_asset$estimate) < 0.4 ~ "Moderate",
      abs(cor_asset$estimate) < 0.6 ~ "Strong",
      TRUE ~ "Very Strong"
    ),
    Significance = ifelse(cor_asset$p.value < 0.05, "Significant", "Not Significant")
  )
)

assoc_complete <- bind_rows(assoc_summary, assoc_numeric)

#Adding primary water source 
assoc_primary <- get_assoc_stats("Primary_Water_Source", "Food_Insecurity", SA_clean)

assoc_complete <- bind_rows(assoc_summary, assoc_primary, assoc_numeric)

print(assoc_complete)

# Export clean LaTeX table to .tex file
cat(
  kable(assoc_complete,
        format = "latex",
        booktabs = TRUE,
        caption = "Bivariate associations between food insecurity and socio-demographic factors in South Africa (n = 1, 542). Strength measured using Cramér’s V; significance assessed via Chi-square tests.",
        label = "tab:bivariate_associations",
        align = c("l", "c", "c", "c", "c")),
  file = "table2_bivariate_associations.tex"
)



#Now running the Multinomial Regressions 
# Load libraries
library(nnet)
library(broom)
library(dplyr)
library(tidyr)
library(gt)
library(pscl)
library(xtable)

# Fit multinomial model
model_multinom_full <- multinom(Food_Insecurity ~
                                  Water_Insecurity +
                                  Urban_Rural +
                                  Employment_Status +
                                  Income_Insecurity +
                                  Age +
                                  Primary_Water_Source +
                                  Education_Level +
                                  P.Asset_Score,
                                data = SA_clean)

# Tidy model output with exponentiated coefficients
tidy_model <- tidy(model_multinom_full, exponentiate = TRUE) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(statistic))),
         stars = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01 ~ "**",
           p.value < 0.05 ~ "*",
           TRUE ~ ""
         ),
         OR_p = paste0(round(estimate, 2), " (", signif(p.value, 3), ") ", stars))

# Reshape to wide format
table_data <- tidy_model %>%
  select(y.level, term, OR_p) %>%
  pivot_wider(names_from = y.level, values_from = OR_p) %>%
  rename(Predictor = term)

# Clean and relabel predictors
label_map <- c(
  "Water_InsecurityLow water insecurity" = "Water Insecurity: Low",
  "Water_InsecurityHigh water insecurity" = "Water Insecurity: High",
  "Urban_RuralRural" = "Residence: Rural",
  "Employment_StatusEmployed part time" = "Employment: Part-time",
  "Employment_StatusEmployed full time" = "Employment: Full-time",
  "Income_InsecurityLow income insecurity" = "Income Insecurity: Low",
  "Income_InsecurityHigh income insecurity" = "Income Insecurity: High",
  "Primary_Water_SourceExternal" = "Water Source: External",
  "Education_LevelSchool education" = "Education: School",
  "Education_LevelHigher/Post-secondary education" = "Education: Higher/Post-secondary",
  "P.Asset_Score" = "Asset Score (Continuous)",
  "Age" = "Age (Continuous)"
)

table_data <- table_data %>%
  mutate(Predictor = recode(Predictor, !!!label_map))

# Model statistics
logLik_val <- logLik(model_multinom_full)
AIC_val <- AIC(model_multinom_full)
BIC_val <- BIC(model_multinom_full)
null_model <- multinom(Food_Insecurity ~ 1, data = SA_clean)
McFadden_R2 <- 1 - (as.numeric(logLik_val) / as.numeric(logLik(null_model)))

# Create LaTeX table
latex_table <- xtable(table_data,
                      caption = "Adjusted Multinomial Logistic Regression Estimates",
                      label = "tab:multinom_summary")

print(latex_table,
      file = "multinomial.full.2.tex",
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      add.to.row = list(
        pos = list(nrow(table_data)),
        command = paste0("\\hline\n\\multicolumn{3}{l}{\\textit{Model Statistics:} ",
                         "LogLik = ", round(logLik_val, 2), ", ",
                         "AIC = ", round(AIC_val, 2), ", ",
                         "BIC = ", round(BIC_val, 2), ", ",
                         "McFadden's $R^2$ = ", round(McFadden_R2, 3), "} \\\\")
      )
)


unique(tidy_model$term)






model_multinom_reduced <- multinom(Food_Insecurity ~ 
                                     Water_Insecurity +
                                     Urban_Rural+
                                     Income_Insecurity +
                                     Education_Level +
                                     P.Asset_Score,
                                   data = SA_clean)

S(model_multinom_reduced)

# Tidy model output
tidy_model_reduced <- tidy(model_multinom_reduced, exponentiate = TRUE) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(statistic))),
         stars = case_when(
           p.value < 0.001 ~ "***",
           p.value < 0.01 ~ "**",
           p.value < 0.05 ~ "*",
           TRUE ~ ""
         ),
         OR_p = paste0(round(estimate, 2), " (", signif(p.value, 3), ") ", stars))

# Reshape to wide format
table_data_reduced <- tidy_model_reduced %>%
  select(y.level, term, OR_p) %>%
  pivot_wider(names_from = y.level, values_from = OR_p) %>%
  rename(Predictor = term)

# Clean and group predictor labels
label_map_reduced <- c(
  "Water_InsecurityLow water insecurity" = "Water Insecurity: Low",
  "Water_InsecurityHigh water insecurity" = "Water Insecurity: High",
  "Urban_RuralRural" = "Residence: Rural",
  "Income_InsecurityLow income insecurity" = "Income Insecurity: Low",
  "Income_InsecurityHigh income insecurity" = "Income Insecurity: High",
  "Education_LevelSchool education" = "Education: School",
  "Education_LevelHigher/Post-secondary education" = "Education: Higher/Post-secondary",
  "P.Asset_Score" = "Asset Score (Continuous)"
)

# Apply label mapping
table_data_reduced <- table_data_reduced %>%
  mutate(Predictor = recode(Predictor, !!!label_map_reduced))


logLik_val_reduced <- logLik(model_multinom_reduced)
AIC_val_reduced <- AIC(model_multinom_reduced)
BIC_val_reduced <- BIC(model_multinom_reduced)
null_model <- multinom(Food_Insecurity ~ 1, data = SA_clean)
McFadden_R2_reduced <- 1 - (as.numeric(logLik_val_reduced) / as.numeric(logLik(null_model)))


# Create LaTeX table
latex_table_reduced <- xtable(table_data_reduced,
                              caption = "Reduced Multinomial Logistic Regression Estimates",
                              label = "tab:multinom_reduced")

# Export with model stats as footnote
print(latex_table_reduced,
      file = "multinom_reduced.tex",
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity,
      add.to.row = list(
        pos = list(nrow(table_data_reduced)),
        command = paste0("\\hline\n\\multicolumn{3}{l}{\\textit{Model Statistics:} ",
                         "LogLik = ", round(logLik_val_reduced, 2), ", ",
                         "AIC = ", round(AIC_val_reduced, 2), ", ",
                         "BIC = ", round(BIC_val_reduced, 2), ", ",
                         "McFadden's $R^2$ = ", round(McFadden_R2_reduced, 3), "} \\\\")
      )
)


#Showing the plot for the effect of the variables 
library(ggplot2)
library(dplyr)
library(broom)

# Tidy model output with exponentiated coefficients (RRR)
tidy_reduced <- tidy(model_multinom_reduced, conf.int = TRUE, exponentiate = TRUE)

# Remove intercepts and annotate
tidy_reduced <- tidy_reduced %>% 
  filter(term != "(Intercept)") %>%
  mutate(
    effect_direction = ifelse(estimate > 1, "Positive Effect", "Negative Effect"),
    significant = ifelse(p.value < 0.05, "Significant", "Not Significant")
  )

# Simplify labels
label_map <- c(
  "Water_InsecurityLow water insecurity" = "Low Water Insecurity",
  "Water_InsecurityHigh water insecurity" = "High Water Insecurity",
  "Urban_RuralRural" = "Rural Residence",
  "Income_InsecurityLow income insecurity" = "Low Income Insecurity",
  "Income_InsecurityHigh income insecurity" = "High Income Insecurity",
  "Education_LevelSchool education" = "School Education",
  "Education_LevelHigher/Post-secondary education" = "Post-School Education",
  "P.Asset_Score" = "Asset Score"
)

# Apply mapping and preserve original order
tidy_reduced <- tidy_reduced %>%
  mutate(term = recode(term, !!!label_map)) %>%
  mutate(term = factor(term, levels = unique(term)))

# Create enhanced plot
plot_reduced <- ggplot(tidy_reduced, aes(x = estimate, y = term, color = effect_direction, shape = significant)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, linewidth = 0.8) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 1) +
  facet_wrap(~ y.level, scales = "free_x") +
  scale_color_manual(values = c("Positive Effect" = "red", "Negative Effect" = "green")) +
  scale_shape_manual(values = c("Significant" = 16, "Not Significant" = 1)) +
  labs(
    title = "Predictor Effects on Food Insecurity (RRR scale)",
    x = "Relative Risk Ratio",
    y = NULL,
    color = "Effect Direction",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.border = element_rect(color = "black", size = 1, fill = NA),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 11),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 30, b = 30, l = 30)  # Prevent clipping
  )

# Export to high-resolution PDF
ggsave("predictor_effects_reduced.pdf", plot = plot_reduced, width = 10, height = 6, units = "in", dpi = 300) 

tail(SA_clean)


library(dplyr)
library(stringr)

SA_clean_fmt <- SA_clean %>%
  mutate(across(where(is.factor), ~ str_replace_all(as.character(.), "insecurity", "insec."))) %>%
  mutate(across(where(is.character), ~ str_replace_all(., "Employed part time", "Part-time"))) %>%
  select(Respondent_ID, Urban_Rural, Food_Insecurity, Water_Insecurity, Age, Gender,
         Employment_Status, Income_Insecurity, Primary_Water_Source, Education_Level, P.Asset_Score)


library(knitr)
library(kableExtra)

kable_output <- SA_clean_fmt %>%
  tail(6) %>%
  kable(format = "latex", booktabs = TRUE, longtable = TRUE,
        caption = "Sample of Respondents with Food, Water, and Socio-Economic and Demographic Factors") %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size = 10)



kable_output <- kable_output %>%
  column_spec(3:4, width = "5cm") %>%
  column_spec(7, width = "4cm")

writeLines(kable_output, "SA_clean_table.tex")

# bargraph
data_long <- SA_clean %>%
  select(Water_Insecurity, Food_Insecurity) %>%
  pivot_longer(cols = everything(),
               names_to = "Type",
               values_to = "Level")

data_summary <- data_long %>%
  group_by(Type, Level) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Type) %>%
  mutate(Percent = Count / sum(Count) * 100)

# --- 2. Create the plot object
p <- ggplot(data_summary, aes(x = Level, y = Percent, fill = Type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = 1.5,
            color = "white",
            size = 3) +
  labs(
    title = "Water and Food insecurity among Respondents",
    x = "Insecurity Level",
    y = "Percentage of Respondents",
    fill = "Type"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 11, face = "bold")
  )

# --- 3. Save to PDF (half-page size)
ggsave(
  filename = "Insecurity_Percent_Barplot_HalfPage.pdf",
  plot = p,
  width = 5, height = 3.5, units = "in"  # size in inches
)
