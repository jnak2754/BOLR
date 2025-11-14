# Install required packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("brms")) install.packages("brms")
if (!require("bayesplot")) install.packages("bayesplot")
if (!require("loo")) install.packages("loo")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("patchwork")) install.packages("patchwork")
if (!require("stringr")) install.packages("stringr")
if (!require("fastDummies")) install.packages("fastDummies")
if (!require("caret")) install.packages("caret")
if (!require("cowplot")) install.packages("cowplot")
if (!require("tidyr")) install.packages("tidyr")


# Load libraries
library(ggplot2)
library(dplyr)
library(brms)
library(bayesplot) 
library(loo)
library(gridExtra)
library(patchwork)
library(stringr)
library(fastDummies)
library(caret)
library(cowplot)
library(tidyr)
library(patchwork)

# Set working directory
setwd("C:/Users/jon0003/OneDrive - Auburn University/CAREER/Research Tasks/7. Theory Guided Data Science/2_Analysis/B-OLR")

# Set relative path to files
inputPath <- file.path(getwd(), 'Input Data')

################################################################################
# VARIABLES TO UPDATE FOR NEW ITERATIONS
# Load the dataset
WiSPD <- read.csv(file.path(inputPath, 'UpdatedFragilities_WiSPD_InferedHAZUSData_Weights_min_z0.csv'))
synthetic <- read.csv(file.path(inputPath, 'UpdatedFragilities_HAZUS_SyntheticData_Weights_min_z0.csv'))

outputPath <- file.path(getwd(), 'Output Data/HAZUS SimCenter Comparison Analysis/000 - Paper Figures Submission')

# Define your formula and family
formula <- damage_state_encoded ~ 1 + wind_speed_sample + terrain + roof_shape + number_of_stories + RWC + garage + roof_cover + RDA + storm_type
family <- cumulative("logit")

cat_cols <- c('number_of_stories', 'roof_shape', 'RWC', 'garage', 'terrain', 'roof_cover', 'RDA', 'storm_type', 'shutters', 'storm_type')
cont_cols <- c('wind_speed_sample') 

# Define the hierarchical model formula
# Define the hierarchical model formula
hierarchical_formula <- bf(
  damage_state_encoded ~ 1 + wind_speed_sample + terrain + roof_shape +
    number_of_stories + RWC + garage + roof_cover + RDA + storm_type +
    (1 | group)
)

# Define all possible class labels 
all_classes <- 0:4
# For M2 - increase the variability of the prior distributions used to decrease their influence when updating with empirical data
sd_multiplier <- 1  # Increase variance by a factor of 2

# Clean list of variables for posterior distribution comparison plots
variables <- list(
  list(variable = 'b_wind_speed_sample', param_string_title = 'Wind Speed'),
  list(variable = 'b_terrain', param_string_title = 'Terrain'),
  list(variable = 'b_roof_shapehip', param_string_title = 'Roof Shape - Hip'),
  list(variable = 'b_number_of_stories', param_string_title = 'Number of Stories'),
  list(variable = 'b_garagewkd', param_string_title = 'Garage - Weak'),
  list(variable = 'b_garagestd', param_string_title = 'Garage - Standard'),
  list(variable = 'b_roof_coverShingle', param_string_title = 'Roof Cover - Shingle'),
  list(variable = 'b_RWCtnail', param_string_title = 'Roof-to-Wall Connection - Toe-nail'),
  list(variable = 'b_RDA8s', param_string_title = 'RDA - 8s'),
  list(variable = 'b_RDA8d', param_string_title = 'RDA - 8d'),
  list(variable = 'b_storm_typeTornado', param_string_title = 'Storm Type - Tornado')
  )



# Cleaning the variable names for plotting
parameter_labels_interaction <- c(
  "b_Intercept[1]" = "Intercept: Category 1",
  "b_Intercept[2]" = "Intercept: Category 2",
  "b_Intercept[3]" = "Intercept: Category 3",
  "b_Intercept[4]" = "Intercept: Category 4",
  "b_wind_speed_sample" = "Effect of Wind Speed",
  "b_terrain" = "Effect of Terrain",
  "b_number_of_stories" = "Effect of Number of Stories",
  "b_roof_shapehip" = "Effect of Roof Shape",
  "b_RWCtnail" = "Effect of RWC Toe-nail",
  "b_garagewkd" = "Effect of Weak Garage",
  "b_garagestd" = "Effect of Standard Garage",
  "b_roof_coverstdShingle" = "Effect of roof Cover - Shingle",
  "b_RDA8s" = "Effect of RDA - 8s",
  "b_RDS8d" = "Effect of RDA - 8d",
  'b_storm_typeTornado' = 'Storm Type - Tornado',
  "b_number_of_stories:groupsynthetic" = "Number of Stories * Synthetic",
  "b_roof_shapehip:groupsynthetic" = "Roof Shape * Synthetic",
  "b_garagewkd:groupsynthetic" = "Weak Garage * Synthetic",
  "b_garagestd:groupsynthetic" = "Standard Garage * Synthetic",
  "b_wind_speed_sample:groupsynthetic" = "Wind Speed * Synthetic",
  "b_terrain:groupsynthetic" = "Terrain * Synthetic",
  "b_roof_coverstdShingle:groupsynthetic" = "Roof Cover (Shingle) * Synthetic",
  "b_RDA8s:groupsynthetic" = "Effect of RDA - 8s",
  "b_RDS8d:groupsynthetic" = "Effect of RDA - 8d",
  'b_storm_typeTornado:groupsynthetic' = 'Storm Type (Tornado) * Synthetic',
  "sd_group__Intercept:groupsynthetic" = "SD of Intercept by Group"
)


# Figure names
prefix <- 'noShutters_ALL'
CM_M0_figname <- paste0("/", prefix, "confusion_matrix_M0_plot.tif")
CM_M0WiSPD_figname <- paste0("/", prefix, "confusion_matrix_M0WiSPD_plot.tif")
CM_M1_figname <- paste0("/", prefix, "confusion_matrix_M1_plot.tif")
CM_M2_figname <- paste0("/", prefix, "confusion_matrix_M2_plot.tif")
CM_M2Syn_figname <- paste0("/", prefix, "confusion_matrix_M2Syn_plot.tif")
CM_M3_figname <- paste0("/", prefix, "confusion_matrix_M3_plot.tif")
CM_M3Syn_figname <- paste0("/", prefix, "confusion_matrix_M3Syn_plot.tif")
CM_H_figname <- paste0("/", prefix, "confusion_matrix_MH_plot.tif")
CM_HWiSPD_figname <- paste0("/", prefix, "confusion_matrix_MHWiSPD_plot.tif")

# Posterior distribution comparison plot figure name
posterior_dist_fig_name <- paste0(prefix, 'PosteriorDistributions.tif')
thresholds_fig_name <- paste0(prefix, "ThresholdDistributions.tif")

# Damage state figure names
DS_M0_Syn_figname <- file.path(outputPath, paste0(prefix, "DS_M0_Synthetic_Test.png"))
DS_M0_WiSPD_figname <- file.path(outputPath, paste0(prefix, "DS_M0_WiSPD_Test.png"))
DS_M1_WiSPD_figname <- file.path(outputPath, paste0(prefix, "DS_M1_WiSPD_Test.png"))
DS_M2_Syn_figname <- file.path(outputPath, paste0(prefix, "DS_M2_Synthetic_Test.png"))
DS_M2_WiSPD_figname <- file.path(outputPath, paste0(prefix, "DS_M2_WiSPD_Test.png"))
DS_M3_Syn_figname <- file.path(outputPath, paste0(prefix, "DS_M3_Synthetic_Test.png"))
DS_M3_WiSPD_figname <- file.path(outputPath, paste0(prefix, "DS_M3_WiSPD_Test.png"))
DS_MH_figname <- file.path(outputPath, paste0(prefix, "DS_MH_Synthetic_Test.png"))
DS_MH_Empirical_figname <- file.path(outputPath, paste0(prefix, "DS_MH_WiSPD_Test.png"))


# Excel file names
WiSPD_testing <- paste0(prefix, 'WiSPD_TestData.csv')
synthetic_testing <- paste0(prefix, 'Synthetic_TestData.csv')

################################################################################
# FUNCTIONS
# Plot a confusion matrix
plot_confusion_matrix <- function(confusion_matrix, plot_title, output_suffix) {
  output_file <- paste0(outputPath, output_suffix)
  
  # Convert the confusion matrix to a data frame with explicit Actual, Predicted, and Count columns
  cm_df <- as.data.frame.table(confusion_matrix, responseName = "Count")
  names(cm_df) <- c("Actual", "Predicted", "Count")
  
  # Get all possible damage states (0-4)
  all_classes <- as.character(0:4)
  
  # Ensure all combinations of Actual and Predicted are represented
  cm_df <- expand.grid(Actual = all_classes, Predicted = all_classes) %>%
    left_join(cm_df, by = c("Actual", "Predicted")) %>%
    mutate(Count = replace_na(Count, 0))
  
  # Create the plot
  p <- ggplot(data = cm_df, aes(x = Predicted, y = Actual, fill = Count)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma") +
    geom_text(aes(label = Count, color = Count), vjust = 0.5, hjust = 0.5) +
    scale_color_gradient(low = "white", high = "black") +
    labs(title = plot_title, x = "Predicted Class", y = "Actual Class") +
    theme_minimal() +
    guides(color = "none")
  
  # Save the plot to a file
  ggsave(output_file, plot = p, width = 4, height = 3, dpi = 300)
}


# Calculate the likelihood ratio index
calculate_LRI <- function(full_model, null_model_formula, data) {
  # Fit the null model (intercept only)
  null_model <- brm(
    formula = null_model_formula,
    data = data,
    family = cumulative("logit"),
    iter = 1000,
    warmup = 400,
    cores = 4,
    chains = 4,
    control = list(adapt_delta = 0.9, max_treedepth = 15)
  )
  
  # Extract the log-likelihoods of the full model and null model
  log_lik_full <- log_lik(full_model)
  log_lik_null <- log_lik(null_model)
  
  # Calculate the mean log-likelihoods across posterior samples for each observation
  mean_log_lik_full <- rowMeans(log_lik_full)
  mean_log_lik_null <- rowMeans(log_lik_null)
  
  # Sum the mean log-likelihoods across all observations
  sum_log_lik_full <- sum(mean_log_lik_full)
  sum_log_lik_null <- sum(mean_log_lik_null)
  
  # Compute the likelihood ratio index (LRI)
  LRI <- 1 - (sum_log_lik_full / sum_log_lik_null)
  
  return(LRI)
}

# Calculate the continuous ranked probability score
calculate_CRPS <- function(model, test_data, damage_col) {
  # Predictive samples from the posterior for the test dataset
  posterior_predictive_samples <- posterior_predict(model, newdata = test_data)
  posterior_predictive_samples <- posterior_predictive_samples - 1
  
  # Function to calculate CRPS for a single observation
  calculate_crps <- function(predictive_samples, observed_value) {
    unique_values <- sort(unique(as.vector(predictive_samples)))
    ecdf <- sapply(unique_values, function(value) {
      mean(predictive_samples <= value)
    })
    observed_cdf <- as.numeric(unique_values >= observed_value)
    sum((ecdf - observed_cdf)^2)
  }
  
  # Calculate CRPS for each observation
  crps_values <- sapply(1:nrow(test_data), function(i) {
    calculate_crps(posterior_predictive_samples[, i], as.integer(as.character(test_data[[damage_col]][i])))
  })
  
  # Average CRPS
  mean_crps <- mean(crps_values)
  
  return(mean_crps)
}

# Calculate the WAIC
calculate_WAIC <- function(model) {
  # Calculate WAIC using the brms function
  waic_result <- waic(model)
  
  # Extract the WAIC value
  waic_value <- waic_result$estimates['waic', 'Estimate']
  
  return(waic_value)
}


# Calculate ELPD/LOO-CV
calculate_LOO_CV <- function(model) {
  # Calculate LOO using the brms function with Pareto-smoothed importance sampling
  loo_result <- loo(model)
  
  # Extract the LOO-CV estimate
  elpd_loo <- loo_result$estimates['elpd_loo', 'Estimate']
  
  return(elpd_loo)
}

# Function to create a bar chart of predicted damage states and actual damage states
plot_damage_states <- function(predicted_values, actual_values, plot_title, save_path) {
  # Create a data frame with predicted and actual damage states
  data <- data.frame(Damage_State = factor(c(predicted_values, actual_values), 
                                            levels = 0:4),  # Set levels to always include 0, 1, 2, 3, 4
                     Type = rep(c("Predicted", "Actual"), each = length(predicted_values)))
  print(save_path)
  # Create the plot
  p <- ggplot(data, aes(x = Damage_State, fill = Type)) +
    geom_bar(position = "dodge") +
    labs(title = plot_title, x = "Damage State", y = "Count") +
    scale_fill_manual(values = c("Predicted" = "blue", "Actual" = "red"))
  
  # Save the plot
  ggsave(filename = save_path, plot = p, width = 8, height = 6, dpi = 300)
  
  return(p)
}

###############################################################################
# Empirical attribute edits for analysis
WiSPD <- WiSPD[WiSPD$year_built != 0, ]
WiSPD <- WiSPD[WiSPD$surge_damage_rating <= 3, ]

# Standardize the roof shape
standardize_roof_shape <- function(roof_shape) {
  # Return "other" if roof_shape is NA
  if (is.na(roof_shape)) {
    return("other")
  }
  
  # Convert to lowercase for case-insensitive matching
  roof_shape <- tolower(roof_shape)
  
  # Split the string by any non-alphanumeric characters
  parts <- str_split(roof_shape, "[^a-zA-Z0-9]+")[[1]]
  
  # Determine the standard roof shape based on the first occurrence of "hip" or "gable"
  if (length(parts) > 0) {
    for (part in parts) {
      if (str_detect(part, "hip")) {
        return("hip")
      } else if (str_detect(part, "gable")) {
        return("gab") 
      }
    }
  }
  return("other")
}

# Apply the function to create the new column
WiSPD <- WiSPD %>%
  mutate(roof_shape_std = sapply(roof_shape, standardize_roof_shape)) %>%
  filter(roof_shape_std %in% c("hip", "gab"))

# Standardize roof cover into Metal, Shingle, or Other categories
WiSPD <- WiSPD %>%
  mutate(roof_cover_std = case_when(
    # Metal roofs
    grepl("metal|steel|aluminum|tin|copper", roof_cover, ignore.case = TRUE) ~ "Metal",
    # Shingle roofs - including common variants and materials
    grepl("shingle|asphalt|3-tab|laminated|composition|architectural", 
          roof_cover, ignore.case = TRUE) ~ "Shingle",
    # Everything else categorized as Other
    TRUE ~ "Other"
  )) %>%
  # Clean up any NA values to Other
  mutate(roof_cover_std = ifelse(is.na(roof_cover_std), "Other", roof_cover_std))

# Drop Other - Roof Cover
WiSPD <- WiSPD %>%
  filter(roof_cover_std != "Other")


WiSPD$number_of_stories <- ifelse(WiSPD$number_of_stories >1, 2, 1)

WiSPD <- WiSPD %>%
  mutate(storm_type = case_when(
    grepl("Hurricane", X_project) ~ "Hurricane",
    grepl("Tornado", X_project) ~ "Tornado",
    TRUE ~ NA_character_ # If neither condition is met, assign NA
  ))


events <- subset(WiSPD, grepl("Hurricane", WiSPD$X_project))

################################################################################
# Dataframe for analysis

WiSPD_data <- WiSPD[c('damage_state', 'wind_speed_mph', 'first_floor_elevation_m',
                      'roof_shape_std', 'roof_cover_std', 'number_of_stories', 
                      'RWC', 'RDA', 'garage', 'shutters', 'terrain', 'storm_type')]

WiSPD_data <- na.omit(WiSPD_data)

################################################################################
# PREPROCESSING OF SYNTHETIC DATA
# Filter synthetic data to only include hurricane storm type
# synthetic <- synthetic %>%
#   filter(storm_type == "Hurricane")

# Convert the terrain values◘
synthetic$terrain <- synthetic$terrain / 100

# Encode the ordinal response variable
synthetic$damage_state_encoded <- factor(synthetic$damage_state, ordered = TRUE)

# Encode categorical variables
synthetic <- dummy_cols(synthetic, 
                       select_columns = cat_cols,
                       remove_selected_columns = FALSE)

# Standardize continuous variables
synthetic[cont_cols] <- scale(synthetic[cont_cols])

# Split into training and testing sets
set.seed(123) # For reproducibility
partition <- createDataPartition(synthetic$damage_state_encoded, p = 0.75, list = FALSE)
train_data_syn <- synthetic[partition, ]
test_data_syn <- synthetic[-partition, ]

################################################################################
# PREPROCESSING OF WiSPD DATA
WiSPD_data$WS_orig <- WiSPD_data$wind_speed_mph
names(WiSPD_data)[names(WiSPD_data) == "wind_speed_mph"] <- "wind_speed_sample"
names(WiSPD_data)[names(WiSPD_data) == "roof_shape_std"] <- "roof_shape"
names(WiSPD_data)[names(WiSPD_data) == "roof_cover_std"] <- "roof_cover"

# Encode the ordinal response variable
WiSPD_data$damage_state_encoded <- factor(WiSPD_data$damage_state, ordered = TRUE)

# Create dummy variables
WiSPD_data <- dummy_cols(WiSPD_data, select_columns = cat_cols)

# Standardize continuous variables
WiSPD_data[cont_cols] <- scale(WiSPD_data[cont_cols])

# Split into training and testing sets
set.seed(123) # For reproducibility
partition <- createDataPartition(WiSPD_data$damage_state_encoded, p = 0.75, list = FALSE)
train_data_WiSPD <- WiSPD_data[partition, ]
test_data_WiSPD <- WiSPD_data[-partition, ]


################################################################################
# M0 - TEST/TRAIN SPLIT OF SYNTHETIC DATA

# Train model on synthetic training data   
M0_synthetic <- brm(
  formula = formula,
  data = train_data_syn,
  family = cumulative("logit"),
  iter = 1000,
  warmup = 500,
  cores = 4, 
  chains = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Print model summary and save estimates to use as priors for another model
print(summary(M0_synthetic), digits = 5)
model_summary_M0 <- summary(M0_synthetic)

# Obtain posterior samples
summary_stats_M0 <- posterior_summary(M0_synthetic)
posterior_estimates_df_M0 <- as.data.frame(summary_stats_M0)

# Predict M0 with test synthetic dataset
predicted_values_M0 <- predict(M0_synthetic, newdata = test_data_syn)
head(predicted_values_M0)

# Assign class
predicted_class_M0 <- max.col(predicted_values_M0, "first")
test_data_syn$predicted_class_M0 <- predicted_class_M0 - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_data_syn$predicted_class_M0, test_data_syn$damage_state, "SDM - Synthetic Test", DS_M0_Syn_figname)

# Compare predicted values to actual values
actual <- factor(test_data_syn$damage_state, levels = all_classes)
predicted <- factor(test_data_syn$predicted_class_M0, levels = all_classes)
confusion_matrix_M0 <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M0)

# Calculate evaluation metrics using the confusion matrix
accuracy_M0 <- sum(diag(confusion_matrix_M0)) / sum(confusion_matrix_M0)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M0, "M0 Synthetic Model", CM_M0_figname)

# Calculate CRPS
mean_CRPS_M0 <- calculate_CRPS(M0_synthetic, test_data_syn, "damage_state")
cat("Mean CRPS for M0:", mean_CRPS_M0, "\n")

# Calculate WAIC
waic_value_M0 <- calculate_WAIC(M0_synthetic)
cat("WAIC for M0:", waic_value_M0, "\n")

# Calculate ELPD/LOO-CV
elpd_loo_value_M0 <- calculate_LOO_CV(M0_synthetic)
cat("ELPD (LOO-CV) for M0:", elpd_loo_value_M0, "\n")

# Calculate LRI
LRI_M0 <- calculate_LRI(M0_synthetic, damage_state_encoded ~ 1, train_data_syn)
cat("Likelihood Ratio Index (LRI) for M0:", LRI_M0, "\n")

################################################################################
# PREDICT WITH WiSPD ON MODEL 0
predicted_values_M0WiSPD <- predict(M0_synthetic, newdata = WiSPD_data)
head(predicted_values_M0WiSPD)

# Assign class
predicted_class_M0WiSPD <- max.col(predicted_values_M0WiSPD, "first")
WiSPD_data$predicted_class_M0WiSPD <- predicted_class_M0WiSPD - 1

# Plot predicted damage states and actual damage states
plot_damage_states(WiSPD_data$predicted_class_M0WiSPD, WiSPD_data$damage_state, "SDM - WiSPD Test", DS_M0_WiSPD_figname)

# Compare predicted values to actual values
actual <- factor(WiSPD_data$damage_state, levels = all_classes)
predicted <- factor(WiSPD_data$predicted_class_M0WiSPD, levels = all_classes)
confusion_matrix_M0WiSPD <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M0WiSPD)

# Calculate evaluation metrics using the confusion matrix
accuracy_M0WiSPD <- sum(diag(confusion_matrix_M0WiSPD)) / sum(confusion_matrix_M0WiSPD)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M0WiSPD, "M0 Synthetic Model Predict with WiSPD", CM_M0WiSPD_figname)

# Calculate CRPS
mean_CRPS_M0WiSPD <- calculate_CRPS(M0_synthetic, WiSPD_data, "damage_state")
cat("Mean CRPS for M0 using WiSPD test data:", mean_CRPS_M0WiSPD, "\n")

################################################################################
# M1 - FITTING WITH ALL OF SYNTHETIC DATA
M1_synthetic <- brm(
  formula = formula,
  data = synthetic,
  family = cumulative("logit"),
  iter = 1000,
  warmup = 500,
  cores = 4, 
  chains = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Print model summary and save estimates to use as priors for another model
print(summary(M1_synthetic), digits = 5)
model_summary_M1 <- summary(M1_synthetic)
estimates_M1 <- model_summary_M1$fixed[, "Estimate"]
std_errors_M1 <- model_summary_M1$fixed[, "Est.Error"]

# Obtain posterior samples
summary_stats_M1 <- posterior_summary(M1_synthetic)
posterior_estimates_df_M1 <- as.data.frame(summary_stats_M1)

# Calculate WAIC
waic_value_M1 <- calculate_WAIC(M1_synthetic)
cat("WAIC for M1:", waic_value_M1, "\n")

# Calculate ELPD/LOO-CV
elpd_loo_value_M1 <- calculate_LOO_CV(M1_synthetic)
cat("ELPD (LOO-CV) for M1:", elpd_loo_value_M1, "\n")

# Calculate LRI
LRI_M1 <- calculate_LRI(M1_synthetic, damage_state_encoded ~ 1, synthetic)
cat("Likelihood Ratio Index (LRI) for M0:", LRI_M1, "\n")

################################################################################
# PREDICT WITH WiSPD ON MODEL 1
predicted_values_M1 <- predict(M1_synthetic, newdata = WiSPD_data)
head(predicted_values_M1)

# Assign class
predicted_class_M1 <- max.col(predicted_values_M1, "first")
WiSPD_data$predicted_class_M1 <- predicted_class_M1 - 1

# Plot predicted damage states and actual damage states
plot_damage_states(WiSPD_data$predicted_class_M1, WiSPD_data$damage_state, "SDM - WiSPD Test", DS_M1_WiSPD_figname)

# Compare predicted values to actual values
actual <- factor(WiSPD_data$damage_state, levels = all_classes)
predicted <- factor(WiSPD_data$predicted_class_M1, levels = all_classes)
confusion_matrix_M1 <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M1)

# Calculate evaluation metrics using the confusion matrix
accuracy_M1 <- sum(diag(confusion_matrix_M1)) / sum(confusion_matrix_M1)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M1, "Model 1", CM_M1_figname)

# Calculate CRPS
mean_CRPS_M1WiSPD <- calculate_CRPS(M1_synthetic, WiSPD_data, "damage_state")
cat("Mean CRPS for M1WiSPD:", mean_CRPS_M1WiSPD, "\n")

################################################################################
# M2 - DETERMINE THE POSTERIOR DISTRIBUTIONS THROUGH BAYESIAN UPDATING: Create a new 
# B-OLR with the posterior distributions from model 1 as the prior 
# distributions and then fit model 2 with the WiSPD data 

# Obtain posterior samples from M1
posterior_samples_M1 <- as.data.frame(as_draws_df(M1_synthetic))
drop_list <- c("Intercept[1]", "Intercept[2]", "Intercept[3]", "Intercept[4]",
                  "disc", "lprior", "lp__", ".chain", ".iteration", ".draw")

# Derive the priors from posterior samples for each coefficient
specified_priors <- lapply(1:length(colnames(posterior_samples_M1)), function(i) {
  coef_name <- colnames(posterior_samples_M1)[i]
  
  # Remove redundant "b_" prefix if it exists
  if (startsWith(coef_name, "b_")) {
    coef_name <- sub("^b_", "", coef_name)
  }
  
  # Explicitly skip Intercepts and other excluded parameters
  if (!coef_name %in% drop_list) {
    coef_posterior <- posterior_samples_M1[[colnames(posterior_samples_M1)[i]]]
    mean_estimate <- mean(coef_posterior, na.rm = TRUE)
    sd_estimate <- sd(coef_posterior, na.rm = TRUE)
    
    # Increase the standard deviation to make priors less informative
    adjusted_sd <- sd_estimate * sd_multiplier
    
    if (!is.na(mean_estimate) && !is.na(adjusted_sd)) {
      set_prior(sprintf("normal(%f, %f)", mean_estimate, adjusted_sd), class = "b", coef = coef_name)
    } else {
      NULL  # Exclude if any NA values found
    }
  } else {
    NULL  # Exclude this prior explicitly
  }
})

# Flatten list of priors into a single vector
specified_priors <- do.call(c, specified_priors[!sapply(specified_priors, is.null)])

# Prior predictive check
M2_prior_predictive <- brm(
  formula = formula,
  data = train_data_WiSPD,          
  family = cumulative("logit"),
  prior = specified_priors,         
  sample_prior = "only",           
  iter = 1000,
  warmup = 500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Visualize prior predictive damage state distribution
pp_check(M2_prior_predictive, type = "bars", nsamples = 100) +
  ggtitle("Prior Predictive Check: M2 (Informed Priors from M1)")

# Extract simulated draws
y_prior_sim <- posterior_predict(M2_prior_predictive)

# Compare proportions of each damage state
prior_sim_probs <- prop.table(table(factor(y_prior_sim, levels = 0:4)))
observed_probs <- prop.table(table(factor(train_data_WiSPD$damage_state, levels = 0:4)))

comparison <- data.frame(
  DamageState = 0:4,
  PriorPredicted = as.numeric(prior_sim_probs),
  ObservedWiSPD = as.numeric(observed_probs)
)
print(comparison)

# Model 2
M2_updated <- brm(
  formula = formula,  
  data = train_data_WiSPD,    
  family = cumulative("logit"),
  prior = specified_priors, 
  iter = 1000,
  warmup = 500,
  cores = 4, 
  chains = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Extract posterior parameters from the new model
posterior_params_M2 <- as_draws(M2_updated)

# Print summary of the new model
print(summary(M2_updated), digits = 5)

# Calculate WAIC
waic_value_M2 <- calculate_WAIC(M2_updated)
cat("WAIC for M2:", waic_value_M2, "\n")

# Calculate ELPD/LOO-CV
elpd_loo_value_M2 <- calculate_LOO_CV(M2_updated)
cat("ELPD (LOO-CV) for M2:", elpd_loo_value_M2, "\n")

# Calculate LRI
LRI_M2<- calculate_LRI(M2_updated, damage_state_encoded ~ 1, train_data_WiSPD)
cat("Likelihood Ratio Index (LRI) for M2:", LRI_M2, "\n")
################################################################################
# PREDICT WITH TESTING WiSPD 
predicted_values_M2 <- predict(M2_updated, newdata = test_data_WiSPD)
head(predicted_values_M2)

# Assign class
predicted_class_M2 <- max.col(predicted_values_M2, "first")
test_data_WiSPD$predicted_class_M2 <- predicted_class_M2 - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_data_WiSPD$predicted_class_M2, test_data_WiSPD$damage_state, "SeqLM - WiSPD Test", DS_M2_WiSPD_figname)

# Compare predicted values to actual values
actual <- factor(test_data_WiSPD$damage_state, levels = all_classes)
predicted <- factor(test_data_WiSPD$predicted_class_M2, levels = all_classes)
confusion_matrix_M2 <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M2)

# Calculate evaluation metrics using the confusion matrix
accuracy_M2 <- sum(diag(confusion_matrix_M2)) / sum(confusion_matrix_M2)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M2, "Model 2 - WiSPD", CM_M2_figname)

# Calculate CRPS
mean_CRPS_M2WiSPD <- calculate_CRPS(M2_updated, test_data_WiSPD, "damage_state")
cat("Mean CRPS for M2WiSPD:", mean_CRPS_M2WiSPD, "\n")

################################################################################
# PREDICT WITH TESTING SYNTHETIC 
predicted_values_M2Syn <- predict(M2_updated, newdata = test_data_syn)
head(predicted_values_M2Syn)

# Assign class
predicted_class_M2Syn <- max.col(predicted_values_M2Syn, "first")
test_data_syn$predicted_class_M2Syn <- predicted_class_M2Syn - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_data_syn$predicted_class_M2Syn, test_data_syn$damage_state, "SeqLM - Synthetic Test", DS_M2_Syn_figname)

# Compare predicted values to actual values
actual <- factor(test_data_syn$damage_state, levels = all_classes)
predicted <- factor(test_data_syn$predicted_class_M2Syn, levels = all_classes)
confusion_matrix_M2Syn <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M2Syn)

# Calculate evaluation metrics using the confusion matrix
accuracy_M2Syn <- sum(diag(confusion_matrix_M2Syn)) / sum(confusion_matrix_M2Syn)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M2Syn, "Model 2 - Synthetic Test", CM_M2Syn_figname)

# Calculate CRPS
mean_CRPS_M2Syn <- calculate_CRPS(M2_updated, test_data_syn, "damage_state")
cat("Mean CRPS for M2Syn:", mean_CRPS_M2Syn, "\n")

################################################################################
# M3 - FITTING THE BAYESIAN ORIDINAL LOGISTIC REGRESSION W WiSPD DATA TEST/TRAIN

# Train model on WiSPD training data
M3_WiSPD <- brm(
  formula = formula,
  data = train_data_WiSPD,
  family = cumulative("logit"),
  iter = 1000,
  warmup = 500,
  cores = 4, 
  chains = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Print model summary and save estimates to use as priors for another model
print(summary(M3_WiSPD), digits = 5)
model_summary_M3 <- summary(M3_WiSPD)

# Obtain posterior samples
summary_stats_M3 <- posterior_summary(M3_WiSPD)
posterior_estimates_df_M3 <- as.data.frame(summary_stats_M3)

# Calculate WAIC
waic_value_M3 <- calculate_WAIC(M3_WiSPD)
cat("WAIC for M3:", waic_value_M3, "\n")

# Calculate ELPD/LOO-CV
elpd_loo_value_M3 <- calculate_LOO_CV(M3_WiSPD)
cat("ELPD (LOO-CV) for M3:", elpd_loo_value_M3, "\n")

# Calculate LRI
LRI_M3<- calculate_LRI(M3_WiSPD, damage_state_encoded ~ 1, train_data_WiSPD)
cat("Likelihood Ratio Index (LRI) for M3:", LRI_M3, "\n")

# Predict with test WiSPD
predicted_values_M3 <- predict(M3_WiSPD, newdata = test_data_WiSPD)
head(predicted_values_M3)

# Assign class
predicted_class_M3 <- max.col(predicted_values_M3, "first")
test_data_WiSPD$predicted_class_M3 <- predicted_class_M3 - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_data_WiSPD$predicted_class_M3, test_data_WiSPD$damage_state, "EDM - WiSPD Test", DS_M3_WiSPD_figname)

# Compare predicted values to actual values
actual <- factor(test_data_WiSPD$damage_state, levels = all_classes)
predicted <- factor(test_data_WiSPD$predicted_class_M3, levels = all_classes)
confusion_matrix_M3 <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M3)

# Calculate evaluation metrics using the confusion matrix
accuracy_M3 <- sum(diag(confusion_matrix_M3)) / sum(confusion_matrix_M3)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M3, "Model 3 - WiSPD Test", CM_M3_figname)

# Calculate CRPS
mean_CRPS_M3WiSPD <- calculate_CRPS(M3_WiSPD, test_data_WiSPD, "damage_state")
cat("Mean CRPS for M3WiSPD:", mean_CRPS_M3WiSPD, "\n")

################################################################################
# PREDICT WITH TESTING SYNTHETIC 
predicted_values_M3Syn <- predict(M3_WiSPD, newdata = test_data_syn)
head(predicted_values_M3Syn)

# Assign class
predicted_class_M3Syn <- max.col(predicted_values_M3Syn, "first")
test_data_syn$predicted_class_M3Syn <- predicted_class_M3Syn - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_data_syn$predicted_class_M3Syn, test_data_syn$damage_state, "EDM - Synthetic Test", DS_M3_Syn_figname)

# Compare predicted values to actual values
actual <- factor(test_data_syn$damage_state, levels = all_classes)
predicted <- factor(test_data_syn$predicted_class_M3Syn, levels = all_classes)
confusion_matrix_M3Syn <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M3Syn)

# Calculate evaluation metrics using the confusion matrix
accuracy_M3Syn <- sum(diag(confusion_matrix_M3Syn)) / sum(confusion_matrix_M3Syn)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M3Syn, "Model 3 - Synthetic Test", CM_M3Syn_figname)

# Calculate CRPS
mean_CRPS_M3Syn <- calculate_CRPS(M2_updated, test_data_syn, "damage_state")
cat("Mean CRPS for M3Syn:", mean_CRPS_M3Syn, "\n")

################################################################################
# HIERARCHICAL MODEL OF M2
# Add a group indicator variable to both datasets
train_data_syn$group <- "synthetic"
train_data_WiSPD$group <- "empirical"

# Combine the datasets
combined_data <- bind_rows(train_data_syn, train_data_WiSPD)

# Fit the hierarchical Bayesian model
hierarchical_model <- brm(
  formula = hierarchical_formula,
  data = combined_data,
  family = cumulative("logit"),
  iter = 1000,     
  warmup = 500,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 20)
)

# Summary of the model
summary(hierarchical_model)

# Calculate the metrics as before
LRI_hierarchical <- calculate_LRI(hierarchical_model, damage_state_encoded ~ 1, combined_data)
CRPS_hierarchical <- calculate_CRPS(hierarchical_model, combined_data, "damage_state")
WAIC_hierarchical <- calculate_WAIC(hierarchical_model)
ELPD_LOO_CV_hierarchical <- calculate_LOO_CV(hierarchical_model)

# Print the results
cat("LRI for Hierarchical Model:", LRI_hierarchical, "\n")
cat("CRPS for Hierarchical Model:", CRPS_hierarchical, "\n")
cat("WAIC for Hierarchical Model:", WAIC_hierarchical, "\n")
cat("ELPD (LOO-CV) for Hierarchical Model:", ELPD_LOO_CV_hierarchical, "\n")

################################################################################
## PREDICT WITH TESTING WiSPD ##
# Add a group indicator variable to both datasets
test_data_syn$group <- "synthetic"
test_data_WiSPD$group <- "empirical"

# Combine the datasets
combined_test_data <- bind_rows(test_data_syn, test_data_WiSPD)

# Predict with the test data
predicted_values_MH <- predict(hierarchical_model, newdata = combined_test_data)
head(predicted_values_MH)

# Assign class
predicted_class_MH <- max.col(predicted_values_MH, "first")
combined_test_data$predicted_class_MH <- predicted_class_MH - 1

# Plot predicted damage states and actual damage states
plot_damage_states(combined_test_data$predicted_class_MH, combined_test_data$damage_state, "HierM - Combined Data", DS_MH_figname)

# Compare predicted values to actual values
actual <- factor(combined_test_data$damage_state, levels = all_classes)
predicted <- factor(combined_test_data$predicted_class_MH, levels = all_classes)
confusion_matrix_MH <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_MH)

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_MH, "Hierarchical Model - Combined Data", CM_H_figname)

# Calculate evaluation metrics using the confusion matrix
accuracy_MH <- sum(diag(confusion_matrix_MH)) / sum(confusion_matrix_MH)

################################################################################
## PREDICTING WITH EMPIRICAL DATA ONLY ##
# Subset the empirical data from the combined dataset
test_empirical_data <- combined_test_data %>% filter(group == "empirical")

# Make predictions using the hierarchical model on the empirical data
predicted_values_empirical <- predict(hierarchical_model, newdata = test_empirical_data)
head(predicted_values_empirical)

# Assign class
predicted_class_empirical <- max.col(predicted_values_empirical, "first")
test_empirical_data$predicted_class_empirical <- predicted_class_empirical - 1

# Plot predicted damage states and actual damage states
plot_damage_states(test_empirical_data$predicted_class_empirical, test_empirical_data$damage_state, "HierM - Empirical Data", DS_MH_Empirical_figname)

# Compare predicted values to actual values
actual <- factor(test_empirical_data$damage_state, levels = all_classes)
predicted <- factor(test_empirical_data$predicted_class_empirical, levels = all_classes)
confusion_matrix_empirical <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_empirical)

# Accuracy
accuracy_empirical <- sum(diag(confusion_matrix_empirical)) / sum(confusion_matrix_empirical)
cat("Accuracy on Empirical Data:", accuracy_empirical, "\n")

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_empirical, "Hierarchical Model - Empirical Data", CM_HWiSPD_figname)

# CRPS Empirical Predictions
CRPS_hierarchical_empirical <- calculate_CRPS(hierarchical_model, test_empirical_data, "damage_state")
cat("CRPS for Hierarchical Model - EmpiricaL Data only:", CRPS_hierarchical_empirical, "\n")

################################################################################
# SAVE TESTING DATASETS
write.csv(test_data_syn, file = file.path(outputPath, synthetic_testing), row.names = FALSE)
write.csv(test_data_WiSPD, file = file.path(outputPath, WiSPD_testing), row.names = FALSE)

################################################################################
# PLOTS TO COMPARE POSTERIOR DISTRIBUTIONS
create_posterior_plot <- function(variable, param_string_title) {
  # Extract posterior samples for the parameter of interest
  posterior_samples_M1 <- as_draws_df(M1_synthetic, variable = variable)
  posterior_samples_M2 <- as_draws_df(M2_updated, variable = variable)
  posterior_samples_M3 <- as_draws_df(M3_WiSPD, variable = variable)
  posterior_samples_MH <- as_draws_df(hierarchical_model, variable = variable)
  
  # Convert to data frames and add a model identifier
  posterior_samples_M1 <- as.data.frame(posterior_samples_M1) %>%
    mutate(Model = "SDM - Synthetic Data Model")
  posterior_samples_M2 <- as.data.frame(posterior_samples_M2) %>%
    mutate(Model = "SeqLM - Sequential Learning Model")
  posterior_samples_M3 <- as.data.frame(posterior_samples_M3) %>%
    mutate(Model = "EDM - Empirical Data Model")
  posterior_samples_MH <- as.data.frame(posterior_samples_MH) %>%
    mutate(Model = "HierM - Hierarchical Model (Fixed Effects)")
  
  combined_posterior_samples <- bind_rows(posterior_samples_M1, 
                                          posterior_samples_M2,
                                          posterior_samples_M3,
                                          posterior_samples_MH)
  
  names(combined_posterior_samples)[1] <- param_string_title
  
  # Calculate common x-axis limits across all variables
  x_min <- min(combined_posterior_samples[[param_string_title]])
  x_max <- max(combined_posterior_samples[[param_string_title]])
  
  # Plot
  plot <- ggplot(combined_posterior_samples, aes(x = .data[[param_string_title]], fill = Model)) +
    geom_density(alpha = 0.5) +
    labs(title = paste(param_string_title),
         x = "Coefficient Value", y = "Density") +
    theme_minimal(base_size = 16) +
    scale_fill_manual(values = c(
      "SDM - Synthetic Data Model" = "#98DF8A", 
      "SeqLM - Sequential Learning Model" = "#FF7F0E",
      "EDM - Empirical Data Model" = "#1F77B4",
      "HierM - Hierarchical Model (Fixed Effects)" = "#9467BD"
    )) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      legend.position = "right",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16)
    )
  
  return(list(plot = plot, x_min = x_min, x_max = x_max))
}

# Create individual plots and get x-axis limits
plot_info <- lapply(variables, function(v) {
  create_posterior_plot(v$variable, v$param_string_title)
})

# Find global x-axis limits
global_x_min <- min(sapply(plot_info, function(x) x$x_min))
global_x_max <- max(sapply(plot_info, function(x) x$x_max))

# Update plots with common x-axis limits
plots <- lapply(plot_info, function(p) {
  p$plot + coord_cartesian(xlim = c(global_x_min, global_x_max))
})

# Create a dummy plot to use its legend
legend_plot <- plots[[1]] +
  theme(legend.position = "right")

# Extract the legend using cowplot's get_legend function
legend <- get_legend(legend_plot)

# Combine the plots into a single figure using patchwork, adding the legend to the right
combined_plot <- wrap_plots(plots, ncol = 3) + plot_layout(guides = "collect") & theme(legend.position = "none")
# 
# # Desired grid
# n_cols <- 3
# n_rows <- 4
# n_cells <- n_cols * n_rows
# 
# # How many real plots you have
# n_plots <- length(plots)
# 
# # Number of empty spacers needed so the legend lands in the LAST cell
# n_spacers <- max(n_cells - n_plots - 1L, 0L)
# 
# # Build the grid: all plots, then spacers, then the guide area as last tile
# grid_plots <- c(
#   plots,
#   rep(list(plot_spacer()), n_spacers),
#   list(guide_area())        # <- legend will be placed here
# )
# 
# # Collect guides into the guide_area() tile and remove legends from individual panels
# final_plot <- wrap_plots(
#   grid_plots,
#   ncol = n_cols,
#   guides = "collect"
# ) & theme(
#   legend.position = "right",     # orientation inside the legend tile
#   legend.text = element_text(size = 14),
#   legend.title = element_text(size = 16)
# )

# Grid layout
n_cols  <- 3
n_rows  <- 4
n_cells <- n_cols * n_rows            # 12

# Put the legend in the second-to-last cell (11th)
legend_pos <- n_cells - 1L            # 11

# How many plots you actually have
n_plots <- length(plots)

# Spacers BEFORE the legend so it lands exactly in cell 11
# (= number of cells before legend) - (plots already occupying those cells)
n_spacers_before <- max((legend_pos - 1L) - n_plots, 0L)

# Spacers AFTER the legend to fill the grid
n_spacers_after  <- max(n_cells - (n_plots + n_spacers_before + 1L), 0L)

grid_plots <- c(
  plots,
  rep(list(plot_spacer()), n_spacers_before),
  list(guide_area()),                       # legend goes here (cell 11)
  rep(list(plot_spacer()), n_spacers_after) # leaves cell 12 empty
)

final_plot <- wrap_plots(
  grid_plots,
  ncol   = n_cols,
  guides = "collect"                        # collect all legends into guide_area()
) & theme(
  legend.position = "right",                # orientation *inside* the legend tile
  legend.text  = element_text(size = 14),
  legend.title = element_text(size = 16)
)

# Save the combined plot
output_path <- file.path(outputPath, posterior_dist_fig_name)
ggsave(filename = output_path, plot = final_plot, width = 20, height = 12, dpi = 300, bg = "white")

################################################################################
# WEIGHTED SCORING SYSTEM GIVEN NEW METRICS
metrics <- data.frame(
  Model = c("M0", "M1", "M2", "M3", "MH"),
  LRI = c(LRI_M0, LRI_M1, LRI_M2, LRI_M3, LRI_hierarchical),
  CRPS = c(mean_CRPS_M0, mean_CRPS_M1WiSPD, mean_CRPS_M2WiSPD, mean_CRPS_M3WiSPD, CRPS_hierarchical),
  WAIC = c(waic_value_M0, waic_value_M1, waic_value_M2, waic_value_M3, WAIC_hierarchical),
  ELPD_LOO_CV = c(elpd_loo_value_M0, elpd_loo_value_M1, elpd_loo_value_M2, elpd_loo_value_M3, ELPD_LOO_CV_hierarchical),
  Accuracy = c(accuracy_M0, accuracy_M1, accuracy_M2, accuracy_M3, accuracy_MH)
)

# Normalize the metrics
metrics$LRI_Norm <- metrics$LRI / max(metrics$LRI)
metrics$CRPS_Norm <- 1 / metrics$CRPS / max(1 / metrics$CRPS)
metrics$WAIC_Norm <- 1 / metrics$WAIC / max(1 / metrics$WAIC)
metrics$ELPD_LOO_CV_Norm <- metrics$ELPD_LOO_CV / max(metrics$ELPD_LOO_CV)
metrics$Accuracy_Norm <- metrics$Accuracy / max(metrics$Accuracy)

# Assign weights
weights <- c(0.2, 0.3, 0.2, 0.2, 0.1)

# Calculate the composite score
metrics$Composite_Score <- rowSums(
  metrics[, c("LRI_Norm", "CRPS_Norm", "WAIC_Norm", "ELPD_LOO_CV_Norm", "Accuracy_Norm")] * weights
)

# Rank models based on the composite score
metrics <- metrics %>% arrange(desc(Composite_Score))

# Output the results
print(metrics)

################################################################################
# POSTERIOR PREDICTIVE CHECK
# Function to run and save pp_check plots
run_pp_check <- function(model, newdata, model_name, output_dir) {
  plot <- pp_check(model, newdata = newdata, type = "bars", nsamples = 100) +
    ggtitle(paste("Posterior Predictive Check:", model_name)) +
    theme_minimal(base_size = 14)
  
  # Save plot
  ggsave(filename = file.path(output_dir, paste0("pp_check_", model_name, ".png")),
         plot = plot, width = 8, height = 6, dpi = 300)
  
  return(plot)
}

# Run pp_check for all models
pp_plot_M0 <- run_pp_check(M0_synthetic, test_data_syn, "M0_Synthetic", outputPath)
pp_plot_M1 <- run_pp_check(M1_synthetic, WiSPD_data, "M1_WiSPD", outputPath)
pp_plot_M2 <- run_pp_check(M2_updated, test_data_WiSPD, "M2_WiSPD", outputPath)
pp_plot_M3 <- run_pp_check(M3_WiSPD, test_data_WiSPD, "M3_WiSPD", outputPath)
pp_plot_MH <- run_pp_check(hierarchical_model, combined_test_data, "MH_Combined", outputPath)


################################################################################
get_thresholds <- function(model, model_name) {
  draws <- posterior::as_draws_df(model) %>%
    as.data.frame()
  
  # Filter for just Intercept[n] (the thresholds)
  intercept_cols <- grep("^Intercept\\[[1-4]\\]$", names(draws), value = TRUE)
  
  draws %>%
    dplyr::select(all_of(intercept_cols)) %>%
    dplyr::mutate(draw = row_number()) %>%
    tidyr::pivot_longer(
      cols = -draw,
      names_to = "Threshold",
      values_to = "Value"
    ) %>%
    dplyr::mutate(Model = model_name)
}


thresholds_all <- bind_rows(
  get_thresholds(M0_synthetic, "SDM - Synthetic Data Model"),
  get_thresholds(M2_updated, "SeqLM - Sequential Learning Model"),
  get_thresholds(M3_WiSPD, "EDM - Empirical Data Model"),
  get_thresholds(hierarchical_model, "HierM - Hierarchical Model (Fixed Effects)")
)

thresholds_all <- thresholds_all %>%
  mutate(Threshold = gsub("\\[(\\d+)\\]", " \\1", Threshold))

# Create new color palette for thresholds
threshold_colors <- c(
  "Intercept 1" = "#2ca02c",  # green
  "Intercept 2" = "#ffbf00",  # yellow
  "Intercept 3" = "#ff7f0e",  # orange
  "Intercept 4" = "#d62728"   # red
)

# Plot thresholds overlaid per model
thresholds_by_model_plot <- ggplot(thresholds_all, aes(x = Value, fill = Threshold)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Model, scales = "free", nrow = 2, ncol = 2) +   # ← 2×2 layout
  scale_fill_manual(
    values = threshold_colors,
    guide = guide_legend(nrow = 1)
  ) +
  labs(
    x = "Latent Threshold Value",
    y = "Density",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 13),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# Save the new plot
output_path_model_facet <- file.path(outputPath, paste0(prefix, "Thresholds_ByModel.tif"))
ggsave(
  filename = output_path_model_facet,
  plot = thresholds_by_model_plot,
  width = 8,   # Adjusted width for 2x2 grid
  height = 5,   # Adjusted height for 2x2 grid
  dpi = 300,
  units = "in",
  device = "tiff"
)




