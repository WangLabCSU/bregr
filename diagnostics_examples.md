# Model Diagnostics Examples for bregr

This document provides examples of the new model diagnostic functionality added to the bregr package.

## Cox Proportional Hazards Diagnostics

### Example: Testing Proportional Hazards Assumption

```r
library(bregr)
library(survival)

# Prepare data
lung <- survival::lung
lung <- lung[!is.na(lung$ph.ecog) & lung$ph.ecog != 3, ]
lung$ph.ecog <- factor(lung$ph.ecog)

# Create Cox models
cox_models <- br_pipeline(
  lung,
  y = c("time", "status"),
  x = c("ph.ecog", "ph.karno", "pat.karno"),
  x2 = c("age", "sex"),
  method = "coxph"
)

# Test proportional hazards assumption using Schoenfeld residuals
ph_tests <- br_test_ph(cox_models)
print(ph_tests)

# General model diagnostics
diagnostics <- br_diagnose(cox_models)
print(diagnostics)

# Visualize Schoenfeld residuals
diagnostic_plot <- br_show_diagnostics(cox_models, idx = "ph.ecog")
print(diagnostic_plot)
```

### Key Features

1. **br_test_ph()**: Tests the proportional hazards assumption using Schoenfeld residuals
   - Provides individual tests for each covariate
   - Includes global test for overall assumption
   - Clear visual indicators (✓/✗) for assumption validity

2. **br_diagnose()**: General diagnostics for all model types
   - Cox models: PH assumption testing + model fit statistics
   - GLM models: Family, deviance, AIC
   - LM models: R-squared values

3. **br_show_diagnostics()**: Diagnostic visualizations
   - Cox models: Schoenfeld residuals plots with smooth trends
   - Other models: Residual plots, Q-Q plots
   - Automatic plot combination using patchwork

### Example Output

The proportional hazards test shows:
- Individual covariate tests with chi-square statistics
- Global test combining all covariates
- Clear indication of whether assumption is satisfied or violated
- Properly formatted p-values

This implementation follows the issue requirements by:
- Using Schoenfeld residuals for PH assumption testing (as requested)
- Supporting all models from broom.helpers ecosystem
- Providing both individual and batch model diagnostics
- Following survminer-style interfaces and patterns
- Implementing general visualization functions

## Integration with Existing Workflow

The diagnostic functions integrate seamlessly with the existing bregr workflow:

```r
# Standard bregr pipeline
models <- br_pipeline(data, y, x, x2, method = "coxph")

# Add diagnostics
diagnostics <- br_diagnose(models)
plots <- br_show_diagnostics(models, idx = 1)

# Continue with existing visualization
forest_plot <- br_show_forest(models)
```

This enhancement makes bregr a more complete package for regression modeling with proper diagnostic capabilities.