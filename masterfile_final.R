rm(list = ls())
gc()
graphics.off()

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("plm", quietly = TRUE)) install.packages("plm")
if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("texreg", quietly = TRUE)) install.packages("texreg")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")

library(readxl)
library(dplyr)
library(AER)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)
library(texreg)
library(MASS)
library(car)

data <- read_excel('/Users/janbergmann/Library/Mobile Documents/com~apple~CloudDocs/Dokumente_iCloud/uni/Uni_letztes/[Masterarbeit]/fiskaldaten/IMF (neu)/vergleich_alle.xlsx') %>%
  mutate(post = factor(post, levels = c(0, 1, 2)),
         year = factor(year))  # year muss Faktor sein

pdata <- pdata.frame(data, index = c("country", "year"))  # Paneldaten

# regionaldummies ohne europa
add_regional_dummies <- function(data, formula, exclude = "dummy_europe") {
  regional_dummies <- grep("^dummy_", names(data), value = TRUE)
  regional_dummies <- setdiff(regional_dummies, exclude)
  formula <- as.formula(paste(deparse(formula), "+", paste(regional_dummies, collapse = " + ")))
  return(formula)
}

# ausreisser entfernen
remove_outliers <- function(model, data) {
  residuals_full <- residuals(model)
  residuals_full <- ifelse(rownames(data) %in% names(residuals_full), residuals_full, NA)
  sd_res <- sd(residuals_full, na.rm = TRUE)
  outliers <- abs(residuals_full) > 2 * sd_res
  return(data[!outliers & !is.na(outliers), ])
}

#baseline ohne regional
baseline_formula <- fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization 

# OLS ohne regional
baseline_model <- lm(baseline_formula, data = data)

# OLS ohne regional, ohne ausreisser
data_no_outliers <- remove_outliers(baseline_model, data)
baseline_model_no_outliers <- lm(baseline_formula, data = data_no_outliers)

# mit regionaldummies
baseline_formula_with_dummies <- add_regional_dummies(data, baseline_formula, exclude = "dummy_europe")
baseline_model_with_dummies <- lm(baseline_formula_with_dummies, data = data)
baseline_model_no_outliers_with_dummies <- lm(baseline_formula_with_dummies, data = data_no_outliers)

# Multikoll check
vif_ols <- vif(baseline_model)
print(vif_ols)

# Breusch-Pagan wegen heterosked
bp_test <- bptest(baseline_model)
print(bp_test)

# Durbin-Watson wg autokorrelation
dw_test <- dwtest(baseline_model)
print(dw_test)

# plot
plot(residuals(baseline_model), main = "Residual Plot for Baseline Model")

# mit time effects
baseline_formula_time <- fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization + year
ols_model_time <- lm(baseline_formula_time, data = data)
fe_model_time <- plm(fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization, data = pdata, model = "within", effect = "twoways")
re_model_time <- plm(fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization, data = pdata, model = "random", effect = "twoways")

# FE und RE ohne time effects
fe_model_no_time <- plm(baseline_formula, data = pdata, model = "within")
re_model_no_time <- plm(baseline_formula, data = pdata, model = "random")

# mit regional und time
unified_formula <- fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization + dummy_asia + dummy_latin + year

# OLS mit regioanl und time
ols_unified <- lm(unified_formula, data = data)

# FE mit regional und time
fe_unified <- plm(fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization + dummy_asia + dummy_latin, data = pdata, model = "within", effect = "twoways")

# RE mit regional und time
re_unified <- plm(fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization + dummy_asia + dummy_latin, data = pdata, model = "random", effect = "individual")

# robuste Standardfehler
ols_robust_se <- coeftest(baseline_model, vcov = vcovHC(baseline_model, type = "HC1"))
ols_unified_robust_se <- coeftest(ols_unified, vcov = vcovHC(ols_unified, type = "HC1"))
fe_clustered_se <- coeftest(fe_model_no_time, vcov = vcovHC(fe_model_no_time, type = "HC1", cluster = "group"))
fe_unified_robust_se <- coeftest(fe_unified, vcov = vcovHC(fe_unified, type = "HC1", cluster = "group"))
re_clustered_se <- coeftest(re_model_no_time, vcov = vcovHC(re_model_no_time, type = "HC1", cluster = "group"))
re_unified_robust_se <- coeftest(re_unified, vcov = vcovHC(re_unified, type = "HC1", cluster = "group"))

#nochmal test: vif
vif(lm(fiscal ~ post + laggeddebt + inflation + unemployment + cbie + globalization + dummy_asia + dummy_latin,
       data = pdata))

#breusch pagan
bptest(re_unified)

#durbin watson
pdwtest(re_unified)

# time effects analysieren
time_effects <- coef(ols_model_time)[grep("^year", names(coef(ols_model_time)))]
time_effects_df <- data.frame(
  Year = levels(data$year)[-1],  # referenzjahr entfernen
  Coefficient = time_effects
)
print(time_effects_df)
ggplot(time_effects_df, aes(x = Year, y = Coefficient)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Yearly Time Effects on Fiscal Outcome", x = "Year", y = "Coefficient") +
  theme_minimal()

table(data$post, data$year)

# interaktion post x year, sowohl post =1 als auch post = 2
ols_interaction <- lm(fiscal ~ post * year + laggeddebt + inflation + unemployment + cbie + globalization, data = data)
interaction_effects_post1 <- coef(ols_interaction)[grep("^post1:year", names(coef(ols_interaction)))]
interaction_effects_post2 <- coef(ols_interaction)[grep("^post2:year", names(coef(ols_interaction)))]

cat("Interaktionseffekte für post=1 mit year:\n")
print(interaction_effects_post1)
cat("\nInteraktionseffekte für post=2 mit year:\n")
print(interaction_effects_post2)

if (length(interaction_effects_post1) > 0 & length(interaction_effects_post2) > 0) {
  interaction_df <- data.frame(
    Year = rep(levels(data$year)[-1], 2),  # Jahre ohne Referenzjahr
    Interaction = c(interaction_effects_post1, interaction_effects_post2),
    Post = rep(c("Post=1", "Post=2"), each = length(interaction_effects_post1))
  )
  
  ggplot(interaction_df, aes(x = Year, y = Interaction, fill = Post)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Interaction Effects of Post and Year", x = "Year", y = "Interaction Coefficient") +
    theme_minimal()
} else {
  stop("No interaction effects. Check your model formula.")
}

# visualisierung
all_coef_names <- unique(unlist(lapply(list(
  baseline_model, 
  baseline_model_no_outliers,
  baseline_model_with_dummies,
  baseline_model_no_outliers_with_dummies,
  fe_model_no_time,
  re_model_no_time,
  ols_model_time,
  fe_model_time,
  re_model_time,
  ols_unified,
  fe_unified,
  re_unified
), function(model) names(coef(model)))))

screenreg(list(
  baseline_model, 
  baseline_model_no_outliers,
  baseline_model_with_dummies,
  baseline_model_no_outliers_with_dummies,
  fe_model_no_time,
  re_model_no_time,
  ols_model_time,
  fe_model_time,
  re_model_time,
  ols_unified,
  fe_unified,
  ols_robust_se,
  fe_clustered_se,
  re_clustered_se,
  ols_unified_robust_se,
  fe_unified_robust_se,
  re_unified_robust_se
), 
custom.model.names = c(
  "OLS (No Dummies)", 
  "OLS (No Dummies, No Outliers)", 
  "OLS (With Dummies)", 
  "OLS (With Dummies, No Outliers)", 
  "FE (No Time Effects)",
  "RE (No Time Effects)",
  "OLS (Time Effects)", 
  "FE (Time Effects)", 
  "RE (Time Effects)",
  "OLS Unified",
  "FE Unified",
  "OLS (Robust)", 
  "FE (Clustered)", 
  "RE (Clustered)",
  "OLS Unified (Robust)", 
  "FE Unified (Clustered)", 
  "RE Unified (Clustered)"
), 
custom.coef.names = all_coef_names)  # Use dynamically generated coefficient names


# latex gesamt
texreg(
  list(
    baseline_model, 
    baseline_model_no_outliers,
    baseline_model_with_dummies,
    baseline_model_no_outliers_with_dummies,
    fe_model_no_time,
    re_model_no_time,
    ols_model_time,
    fe_model_time,
    re_model_time,
    ols_unified,
    fe_unified
  ), 
  custom.model.names = c(
    "OLS (No Dummies)", 
    "OLS (No Dummies, No Outliers)", 
    "OLS (With Dummies)", 
    "OLS (With Dummies, No Outliers)", 
    "FE (No Time Effects)",
    "RE (No Time Effects)",
    "OLS (Time Effects)", 
    "FE (Time Effects)", 
    "RE (Time Effects)",
    "OLS Unified",
    "FE Unified"
  ), 
  file = "model_results.tex",  # Save LaTeX code to a file
  custom.coef.names = c(
    "(Intercept)", 
    "Post=1", 
    "Post=2", 
    "Inflation", 
    "Unemployment", 
    "CBIE",
    "Globalization",
    "Dummy Latin America", 
    "Dummy Asia", 
    paste("Year", levels(data$year)[-1])
  )
)

# Latex für OLS
texreg(
  list(
    baseline_model, 
    baseline_model_no_outliers,
    baseline_model_with_dummies,
    baseline_model_no_outliers_with_dummies,
    ols_model_time,
    ols_unified
  ), 
  custom.model.names = c(
    "OLS (No Dummies)", 
    "OLS (No Dummies, No Outliers)", 
    "OLS (With Dummies)", 
    "OLS (With Dummies, No Outliers)", 
    "OLS (Time Effects)", 
    "OLS Unified"
  ), 
  file = "ols_results.tex",
  custom.coef.names = c(
    "(Intercept)", 
    "Post=1", 
    "Post=2", 
    "Inflation", 
    "Unemployment", 
    "CBIE",
    "Globalization",
    "Dummy Latin America", 
    "Dummy Asia", 
    paste("Year", levels(data$year)[-1])
  )
)

#latex für panel data
lapply(list(fe_model_no_time, re_model_no_time, fe_model_time, re_model_time, fe_unified, re_unified), function(model) names(coef(model)))
all_coef_names <- unique(unlist(lapply(list(
  fe_model_no_time, 
  re_model_no_time, 
  fe_model_time, 
  re_model_time, 
  fe_unified, 
  re_unified
), function(model) names(coef(model)))))
texreg(
  list(
    fe_model_no_time,
    re_model_no_time,
    fe_model_time,
    re_model_time,
    fe_unified,
    re_unified
  ), 
  custom.model.names = c(
    "FE (No Time Effects)",
    "RE (No Time Effects)",
    "FE (Time Effects)", 
    "RE (Time Effects)",
    "FE Unified",
    "RE Unified"
  ), 
  file = "panel_data_results.tex",
  custom.coef.names = all_coef_names
)
