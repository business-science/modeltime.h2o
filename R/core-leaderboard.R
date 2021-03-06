# AUTOML LEADERBOARD DOCS ----

#' H2O AutoML Leaderboard Utilities
#' 
#' @description
#' 
#' The H2O AutoML Leaderboard lists any models that have been created during the `automl_reg()`
#' training process. 
#' 
#' - The training process automatically uses the top model. 
#' - The available models can be shown with `automl_leaderboard()`
#' - The model change the model used using `automl_update_model()`.
#'
#' @param object An object created by [automl_reg()] and trained (fitted). 
#' @param model_id An H2O Model ID (shown in the AutoML Leaderboard). 
#'  Alternatively, the user can provide an H2O model. 
#'
#' @return 
#' - `automl_leaderboard()`: A `tibble` containing the H2O AutoML Leaderboard
#' - `automl_update_model()`: An updated `parnsip` or `workflow` with the H2O Model updated
#' 
#' @examples 
#' \dontrun{
#' library(tidymodels)
#' library(modeltime.h2o)
#' library(h2o)
#' library(tidyverse)
#' library(timetk)
#' 
#' h2o.init(
#'     nthreads = -1,
#'     ip       = 'localhost',
#'     port     = 54321
#' )
#' 
#' # Model Spec
#' model_spec <- automl_reg(mode = 'regression') %>%
#'     set_engine(
#'         engine                     = 'h2o',
#'         max_runtime_secs           = 5, 
#'         max_runtime_secs_per_model = 4,
#'         nfolds                     = 5,
#'         max_models                 = 3,
#'         exclude_algos              = c("DeepLearning"),
#'         seed                       = 786
#'     ) 
#' 
#' 
#' # Fit AutoML
#' model_fit <- model_spec %>%
#'     fit(value ~ ., data = training(m750_splits))
#' 
#' # Inspect the Leaderboard
#' leaderboard_tbl <- automl_leaderboard(model_fit)
#' leaderboard_tbl
#' 
#' # Swap an H2O Model Out (Using the 2nd model from the leaderboard)
#' model_id_2  <- leaderboard_tbl$model_id[[2]]
#' model_fit_2 <- automl_update_model(model_fit, model_id_2)
#' model_fit_2
#' 
#' # Shutdown H2O when Finished. 
#' # Make sure to save any work before. 
#' h2o.shutdown(prompt = FALSE)
#'     
#'     
#' }
#'   
#' @name automl_leaderboard
#' @export

# AUTOML LEADERBOARD ----

#' @rdname automl_leaderboard
#' @export
automl_leaderboard <- function(object) {
    UseMethod("automl_leaderboard")
}

#' @export
automl_leaderboard.workflow <- function(object) {
    get_leaderboard(object)
}

#' @export
automl_leaderboard.model_fit <- function(object) {
    get_leaderboard(object)
}

#' @export
automl_leaderboard.model_spec <- function(object) {
    msg <- "No leaderboard found. Make sure you have fitted (trained) your parsnip or workflow model with the `fit()` function."
    rlang::abort(msg)
}

#' @export
automl_leaderboard.default <- function(object) {
    rlang::abort(stringr::str_glue("This function is designed for `automl_reg()` models. The `object` provided has class: {class(object)[1]}"))
}




# CHANGE MODEL -----

#' @rdname automl_leaderboard
#' @export
automl_update_model <- function(object, model_id) {
    
    if (rlang::is_missing(object)) rlang::abort("`object` is missing. Please provide a valid `automl_reg()` object.")
    
    if (rlang::is_missing(model_id)) rlang::abort("`model_id` is missing. Please provide a valid H2O Model ID.")
    
    UseMethod("automl_update_model")
}

#' @export
automl_update_model.workflow <- function(object, model_id) {
    change_h2o_model(object, model_id)
}

#' @export
automl_update_model.model_fit <- function(object, model_id) {
    change_h2o_model(object, model_id)
}

#' @export
automl_update_model.model_spec <- function(object, model_id) {
    msg <- "No leaderboard found. Make sure you have fitted (trained) your parsnip or workflow model with the `fit()` function."
    rlang::abort(msg)
}

#' @export
automl_update_model.default <- function(object, model_id) {
    rlang::abort(stringr::str_glue("This function is designed for `automl_reg()` models. The `object` provided has class: {class(object)[1]}"))
}

