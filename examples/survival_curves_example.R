# Example usage of the new model score prediction and survival curve functionality

# Load required libraries
library(bregr)
library(survival)
library(ggplot2)

# Prepare example data using survival::lung dataset
data(lung, package = "survival")
lung <- lung |> 
  dplyr::filter(ph.ecog != 3, !is.na(ph.ecog)) |>
  dplyr::mutate(ph.ecog = factor(ph.ecog))

# Example 1: Basic usage with Cox regression
mds_cox <- br_pipeline(
  lung,
  y = c("time", "status"),
  x = c("age", "ph.ecog"),
  x2 = "sex",
  method = "coxph"
)

# Generate model predictions (risk scores)
risk_scores <- br_predict(mds_cox, model_idx = "age")
print(paste("Generated", length(risk_scores), "risk score predictions"))

# Create survival curves based on model scores
p1 <- br_show_survival_curves(
  mds_cox, 
  model_idx = "age",
  n_groups = 3,
  title = "Survival Curves by Age Model Risk Score"
)

# Example 2: Using different number of groups
p2 <- br_show_survival_curves(
  mds_cox,
  model_idx = "ph.ecog", 
  n_groups = 2,
  group_labels = c("Low Risk", "High Risk"),
  title = "Survival Curves by ECOG Performance Status Model"
)

# Example 3: Multiple focal variables
mds_multi <- br_pipeline(
  lung,
  y = c("time", "status"),
  x = c("age", "ph.ecog", "ph.karno"),
  x2 = "sex",
  method = "coxph"
)

# Create survival curves for different models
p3 <- br_show_survival_curves(
  mds_multi,
  model_idx = "ph.karno",
  n_groups = 3,
  title = "Survival Curves by Karnofsky Performance Score Model"
)

print("All examples completed successfully!")
print("Plots can be displayed with: print(p1), print(p2), print(p3)")

# Example output information
cat("\nModel Summary:\n")
cat("- Models fitted:", length(br_get_models(mds_cox)), "\n")
cat("- Model names:", paste(br_get_model_names(mds_cox), collapse = ", "), "\n")
cat("- Sample size:", nrow(br_get_data(mds_cox)), "\n")