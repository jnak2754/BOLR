library(ggplot2)
library(dplyr)
library(caret)
library(fastDummies)
library(brms)
library(bayesplot)
library(loo)
library(ggmcmc)
library(mcmcplots)
library(MASS)
library(reshape2)
library(purrr)
library(rstudioapi)
library(stringr)
library(ggplot2)
library(patchwork)

setwd("C:/Users/jon0003/OneDrive - Auburn University/CAREER/Research Tasks/7. Theory Guided Data Science/2_Analysis/B-OLR")

# Set relative path to files
inputPath <- file.path(getwd(), 'Input Data')

################################################################################
# Load the dataset
WiSPD <- read.csv(file.path(inputPath, 'UpdatedFragilities_WiSPD_InferedHAZUSData_Weights_min_z0.csv'))
synthetic <- read.csv(file.path(inputPath, 'UpdatedFragilities_HAZUS_SyntheticData_Weights_min_z0.csv'))

################################################################################
# GLOBAL VARIABLES TO UPDATE FOR NEW ITERATIONS
outputPath <- file.path(getwd(), 'Output Data/HAZUS SimCenter Comparison Analysis/000 - Paper Figures Submission')

# Define your formula and family
formula <- damage_state_encoded ~ 1 + wind_speed_sample #+ terrain + roof_shape + number_of_stories + RWC + garage + roof_cover + RDA + storm_type
family <- cumulative("logit")

cat_cols <- c('number_of_stories', 'roof_shape', 'RWC', 'garage', 'terrain', 'roof_cover', 'RDA', 'storm_type', 'shutters', 'storm_type')
cont_cols <- c('wind_speed_sample')
DS <- c('damage_state')

# List of variables and their corresponding titles (for M3 only)
variables <- list(
  list(variable = 'b_wind_speed_sample', param_string_title = 'Wind Speed')
  # list(variable = 'b_terrain', param_string_title = 'Terrain'),
  # list(variable = 'b_roof_shapehip', param_string_title = 'Roof Shape - Hip'),
  # list(variable = 'b_number_of_stories', param_string_title = 'Number of Stories'),
  # list(variable = 'b_RWCtnail', param_string_title = 'RWC - Toe-nail'),
  # list(variable = 'b_garagewkd', param_string_title = 'Garage - Weak'),
  # list(variable = 'b_garagestd', param_string_title = 'Garage - Standard'),
  # list(variable = 'b_roof_coverShingle', param_string_title = 'Roof Cover - Shingle'),
  # list(variable = 'b_storm_typeTornado', param_string_title = 'Storm Type - Tornado'),
  # list(variable = 'b_RDA8s', param_string_title = 'RDA - 8s'),
  # list(variable = 'b_RDA8d', param_string_title = 'RDA - 8d')
  )

# Output paths - increment number for each new model
model_number <- "min_z0_1"  # Update for each new model 
syn_model_number <- paste0(model_number, "_SYN")

outputPath <- file.path(getwd(), 'Output Data/HAZUS SimCenter Comparison Analysis/000 - Paper Figures Submission/Empirical Model/')
posterior_dist_fig_name <- paste0("/", model_number, "_posterior_distributions.tif")
confusion_matrix_fig_name <- paste0("/", model_number, "_confusion_matrix.tif")
testing_data_name <- paste0("/TestingData_", model_number, ".csv")
training_data_name <- paste0("/TrainingData_", model_number, ".csv")
syn_posterior_dist_fig_name <- paste0("/", syn_model_number, "_posterior_distributions.tif")
syn_confusion_matrix_fig_name <- paste0("/", syn_model_number, "_confusion_matrix.tif")
syn_testing_data_name <- paste0("/TestingData_", syn_model_number, ".csv")
syn_training_data_name <- paste0("/TrainingData_", syn_model_number, ".csv")
posterior_dist_fig_name_combined <- paste0("/", model_number, "_posterior_distributionsCOMBINED.tif")
# Current year for age calculation
current_year <- 2024

# All Classes for Confusion matrix
all_classes <- 0:4

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
# M3 - FITTING THE BAYESIAN ORIDINAL LOGISTIC REGRESSION W WiSPD DATA TEST/TRAIN
print(get_prior(formula, train_data_WiSPD))

# Train model on WiSPD training data
M3_WiSPD <- brm(
  formula = formula,
  data = train_data_WiSPD,
  family = cumulative("logit"),
  iter = 2000,
  warmup = 1000,
  cores = 4, 
  chains = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# Print model summary and save estimates to use as priors for another model
print(summary(M3_WiSPD), digits = 5)
model_summary_M3 <- summary(M3_WiSPD)

# Obtain posterior samples
posterior_samples <- as_draws_df(M3_WiSPD)
posterior_long <- posterior_samples %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw, lp__, lprior, disc,
                         `b_Intercept[1]`, `b_Intercept[2]`, `b_Intercept[3]`
                         ), names_to = "parameter", values_to = "value")

create_m3_posterior_plot <- function(variable, param_string_title) {
  # Extract posterior samples for M3 only
  posterior_samples_M3 <- as_draws_df(M3_WiSPD, variable = variable)
  
  # Convert to data frame and add a model identifier
  posterior_samples_M3 <- as.data.frame(posterior_samples_M3) %>%
    mutate(Model = "M3 - WiSPD")
  
  # Rename the first column to match the param_string_title
  names(posterior_samples_M3)[1] <- param_string_title
  
  # Plot for M3 only
  plot <- ggplot(posterior_samples_M3, aes(x = .data[[param_string_title]], fill = Model)) +
    geom_density(alpha = 0.5) +
    labs(title = paste(param_string_title),
         x = "Coefficient Value", y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("M3 - WiSPD" = "#1F77B4")) +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold")) +
    coord_cartesian(xlim = c(-2, 2))  # Set consistent x-axis limits for all plots
  
  return(plot)
}


# Create individual plots for M3 only
m3_plots <- lapply(variables, function(v) {
  create_m3_posterior_plot(v$variable, v$param_string_title)
})

# Combine the M3 subplots into a single figure
combined_m3_plot <- wrap_plots(m3_plots, ncol = 2)
output_path <- file.path(outputPath, posterior_dist_fig_name)
ggsave(filename = output_path, plot = combined_m3_plot, width = 10, height = 12, dpi = 300, bg = "white")

# Predict with trained model
predicted_values_M3 <- predict(M3_WiSPD, newdata = test_data_WiSPD)
head(predicted_values_M3)

# Assign class
predicted_class_M3 <- max.col(predicted_values_M3, "first")
test_data_WiSPD$predicted_class <- predicted_class_M3 - 1

# Compare predicted values to actual values
actual <- factor(test_data_WiSPD[[DS]], levels = all_classes)
predicted <- factor(test_data_WiSPD$predicted_class, levels = all_classes)
confusion_matrix_M3 <- table(Actual = actual, Predicted = predicted)
print(confusion_matrix_M3)


# Define the function
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

# Plot confusion matrix
plot_confusion_matrix(confusion_matrix_M3, "Confusion Matrix", confusion_matrix_fig_name)

# Calculate evaluation metrics using the confusion matrix
accuracy_M3 <- sum(diag(confusion_matrix_M3)) / sum(confusion_matrix_M3)
print(accuracy_M3)

# Save testing dataset
write.csv(test_data_WiSPD, file = file.path(outputPath, testing_data_name), row.names = FALSE)
write.csv(train_data_WiSPD, file = file.path(outputPath, training_data_name), row.names = FALSE)

################################################################################
calculate_LRI <- function(full_model, null_model_formula, data) {
  # Fit the null model (intercept only)
  null_model <- brm(
    formula = null_model_formula,
    data = data,
    family = cumulative("logit"),
    iter = 2000,
    warmup = 1000,
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

LRI <- calculate_LRI(M3_WiSPD, damage_state_encoded ~ 1, train_data_WiSPD)
cat("Likelihood Ratio Index (LRI):", LRI, "\n")

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

# Calculate CRPS
mean_CRPS <- calculate_CRPS(M3_WiSPD, test_data_WiSPD, DS)
cat("Mean CRPS:", mean_CRPS, "\n")

calculate_WAIC <- function(model) {
  # Calculate WAIC using the brms function
  waic_result <- waic(model)
  
  # Extract the WAIC value
  waic_value <- waic_result$estimates['waic', 'Estimate']
  
  return(waic_value)
}

waic_value <- calculate_WAIC(M3_WiSPD)
cat("WAIC:", waic_value, "\n")

calculate_LOO_CV <- function(model) {
  # Calculate LOO using the brms function with Pareto-smoothed importance sampling
  loo_result <- loo(model)
  
  # Extract the LOO-CV estimate
  elpd_loo <- loo_result$estimates['elpd_loo', 'Estimate']
  return(elpd_loo)
}

elpd_loo_value <- calculate_LOO_CV(M3_WiSPD)
cat("ELPD (LOO-CV):", elpd_loo_value, "\n")

################################################################################
# Train model on Synthetic training data
M3_SYN <- brm(
  formula = formula,
  data = train_data_syn,
  family = cumulative("logit"),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

print(summary(M3_SYN), digits = 5)

# ---------- Posterior density plots (Synthetic) ----------
create_m3syn_posterior_plot <- function(variable, param_string_title) {
  posterior_samples_syn <- as_draws_df(M3_SYN, variable = variable) |>
    as.data.frame() |>
    dplyr::mutate(Model = "M3 - Synthetic")
  
  names(posterior_samples_syn)[1] <- param_string_title
  
  ggplot(posterior_samples_syn, aes(x = .data[[param_string_title]], fill = Model)) +
    geom_density(alpha = 0.5) +
    labs(title = param_string_title, x = "Coefficient Value", y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("M3 - Synthetic" = "#FF7F0E")) +
    theme(legend.position = "none", plot.title = element_text(size = 16, face = "bold")) +
    coord_cartesian(xlim = c(-2, 2))
}

m3syn_plots <- lapply(variables, function(v) {
  create_m3syn_posterior_plot(v$variable, v$param_string_title)
})

combined_m3syn_plot <- patchwork::wrap_plots(m3syn_plots, ncol = 2)
ggsave(
  filename = file.path(outputPath, syn_posterior_dist_fig_name),
  plot = combined_m3syn_plot,
  width = 10, height = 12, dpi = 300, bg = "white"
)

# ---------- Predictions, confusion matrix, accuracy ----------
predicted_values_SYN <- predict(M3_SYN, newdata = test_data_syn)
predicted_class_SYN <- max.col(predicted_values_SYN, "first") - 1L
test_data_syn$predicted_class <- predicted_class_SYN

actual_syn <- factor(test_data_syn[[DS]], levels = all_classes)
predicted_syn <- factor(test_data_syn$predicted_class, levels = all_classes)
confusion_matrix_SYN <- table(Actual = actual_syn, Predicted = predicted_syn)
print(confusion_matrix_SYN)


# ---------- Metrics: LRI, CRPS, WAIC, LOO ----------
LRI_SYN <- calculate_LRI(M3_SYN, damage_state_encoded ~ 1, train_data_syn)
cat("Likelihood Ratio Index (Synthetic):", LRI_SYN, "\n")

mean_CRPS_SYN <- calculate_CRPS(M3_SYN, test_data_syn, DS)
cat("Mean CRPS (Synthetic):", mean_CRPS_SYN, "\n")

waic_value_SYN <- calculate_WAIC(M3_SYN)
cat("WAIC (Synthetic):", waic_value_SYN, "\n")

elpd_loo_value_SYN <- calculate_LOO_CV(M3_SYN)
cat("ELPD (LOO-CV) (Synthetic):", elpd_loo_value_SYN, "\n")

# ---------- Save synthetic testing dataset (same outputPath)
write.csv(
  test_data_syn,
  file = file.path(outputPath, syn_testing_data_name),
  row.names = FALSE
)

write.csv(train_data_syn, file = file.path(outputPath, syn_training_data_name), row.names = FALSE)

################################################################################
# PLOTS TO COMPARE POSTERIOR DISTRIBUTIONS
create_posterior_plot <- function(variable, param_string_title) {
  # Extract posterior samples for the parameter of interest
  posterior_samples_M3 <- as_draws_df(M3_WiSPD, variable = variable)
  posterior_samples_ME3Syn <- as_draws_df(M3_SYN, variable = variable)
  
  # Convert to data frames and add a model identifier
  posterior_samples_M3 <- as.data.frame(posterior_samples_M3) %>%
    mutate(Model = "EDM - Empirical Data Model")
  posterior_samples_ME3Syn <- as.data.frame(posterior_samples_ME3Syn) %>%
    mutate(Model = "SDM - Synthetic Data Model")
  
  combined_posterior_samples <- bind_rows(posterior_samples_M3,
                                          posterior_samples_ME3Syn)

  
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
      "EDM - Empirical Data Model" = "#1F77B4"
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

# Desired grid
n_cols <- 3
n_rows <- 4
n_cells <- n_cols * n_rows

# How many real plots you have
n_plots <- length(plots)

# Number of empty spacers needed so the legend lands in the LAST cell
n_spacers <- max(n_cells - n_plots - 1L, 0L)

# Build the grid: all plots, then spacers, then the guide area as last tile
grid_plots <- c(
  plots,
  rep(list(plot_spacer()), n_spacers),
  list(guide_area())        # <- legend will be placed here
)

# Collect guides into the guide_area() tile and remove legends from individual panels
final_plot <- wrap_plots(
  grid_plots,
  ncol = n_cols,
  guides = "collect"
) & theme(
  legend.position = "right",     # orientation inside the legend tile
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 16)
)

# Save the combined plot
output_path <- file.path(outputPath, posterior_dist_fig_name_combined)
ggsave(filename = output_path, plot = final_plot, width = 20, height = 12, dpi = 300, bg = "white")

################################################################################
get_thresholds <- function(model, model_name) {
  draws <- posterior::as_draws_df(model) %>%
    as.data.frame()
  
  intercept_cols <- grep("^b_Intercept\\[[1-4]\\]$", names(draws), value = TRUE)
  
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
  get_thresholds(M3_SYN, "SDM - Synthetic Data Model"),
  get_thresholds(M3_WiSPD, "EDM - Empirical Data Model")
)

thresholds_all <- thresholds_all %>%
  mutate(
    Threshold = gsub("b_Intercept\\[(\\d+)\\]", "DS \\1", Threshold),
    Threshold = factor(Threshold, levels = paste0("DS ", 1:4))
  )

thresholds_all$Model <- recode(
  thresholds_all$Model,
  "SDM - Synthetic Data Model" = "SDM\nSynthetic Data Model",
  "EDM - Empirical Data Model" = "EDM\nEmpirical Data Model"
)

ggplot(thresholds_all, aes(x = Value, fill = Threshold)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Model, nrow = 1) +
  scale_fill_manual(
    values = c("DS 1" = "#98DF8A", "DS 2" = "#FFD700", "DS 3" = "#FF7F0E", "DS 4" = "#D62728")
  ) +
  labs(
    title = "Posterior Distributions of Thresholds by Model",
    x = "Latent Threshold Value",
    y = "Density",
    fill = "Damage State"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

ggsave(
  filename = file.path(outputPath, paste0(model_number, "_thresholds_by_DS.tif")),
  width = 10, height = 4, dpi = 300, units = "in"
)
