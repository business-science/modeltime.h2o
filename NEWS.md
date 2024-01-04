# modeltime.h2o 0.1.2

- Get back on CRAN (timetk archival)

# modeltime.h2o 0.1.1

## New Functions

- `automl_leaderboard()`: Returns the AutoML Leaderboard from the AutoML run when the model was created. 
- `automl_update_model()`: Makes it easy to swap out models from the AutoML Leaderboard.

## Fix CRAN issue

- Moved `h2o.init()` to skip on CRAN per H2O recommendation. The original issue was related to `libxgboost4j_gpu*.so` being left on the user system when the user's temp library. This is being corrected by H2O. 

# modeltime.h2o 0.1.0

This is the initial release with key functions:

- `automl_reg()`: Uses H2O AutoML as a backend for forecasting.  
- `save_h2o_model()` and `load_h2o_model()`: Saving and loading H2O models and Workflow/Parsnip objects
