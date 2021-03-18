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
#' - The model change the model used using `automl_change_model()`.
#'
#' @param object An object created by [automl_reg()] and trained (fitted). 
#' @param model_id An H2O Model ID (shown in the AutoML Leaderboard).
#'
#' @return A tibble containing the H2O AutoML Leaderboard
#' 
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

get_leaderboard <- function(object) {
    
    # Check object is trained
    msg1 <- "Model or workflow object is not trained."
    if (!is_trained(object)) stop(msg1)
    
    # Extract leaderboard
    if (inherits(object, "model_fit")) {
        leaderboard <- object$fit$extras$leaderboard
    } else if (inherits(object, "workflow")) {
        leaderboard <- object$fit$fit$fit$extras$leaderboard
    } 
    
    # Was there a leaderboard?
    msg2 <- "No leaderboard found. Make sure you have an `automl_reg()` model."
    if (is.null(leaderboard)) rlang::abort(msg2)
    
    return(leaderboard)
}

is_trained <- function(x) {
    
    trained <- FALSE
    
    if (inherits(x, "model_fit")) {
        trained <- TRUE
    }
    
    if (inherits(x, "workflow")) {
        trained <- x$trained
    }
    
    if (inherits(x, "mdl_time_ensemble")) {
        trained <- TRUE
    }
    
    return(trained)
}


# CHANGE MODEL -----

#' @rdname automl_leaderboard
#' @export
automl_change_model <- function(object, model_id) {
    
    if (rlang::is_missing(object)) rlang::abort("`object` is missing. Please provide a valid `automl_reg()` object.")
    
    if (rlang::is_missing(model_id)) rlang::abort("`model_id` is missing. Please provide a valid H2O Model ID.")
    
    UseMethod("automl_change_model")
}

#' @export
automl_change_model.workflow <- function(object, model_id) {
    change_automl_model(object, model_id)
}

#' @export
automl_change_model.model_fit <- function(object, model_id) {
    change_automl_model(object, model_id)
}

#' @export
automl_change_model.model_spec <- function(object, model_id) {
    msg <- "No leaderboard found. Make sure you have fitted (trained) your parsnip or workflow model with the `fit()` function."
    rlang::abort(msg)
}

#' @export
automl_change_model.default <- function(object, model_id) {
    rlang::abort(stringr::str_glue("This function is designed for `automl_reg()` models. The `object` provided has class: {class(object)[1]}"))
}

# CHANGE MODEL UTILITIES ----

change_automl_model <- function(object, model_id) {
    
    # Check object is trained
    msg1 <- "Model or workflow object is not trained."
    if (!is_trained(object)) stop(msg1)
    
    # Get h2o model
    h2o_model <- h2o.getModel(model_id)
    
    # Change the object's h2o model
    if (inherits(object, "model_fit")) {
        object$fit <- swap_h2o_model(object$fit, h2o_model)
    } else if (inherits(object, "workflow")) {
        object$fit$fit$fit <- swap_h2o_model(object$fit$fit$fit, h2o_model)
    } 
    
    return(object)
}

# These are made in the H2O 
swap_h2o_model <- function(object, h2o_model) {
    UseMethod("swap_h2o_model", object)
}





