
#' Get the H2O AutoML Leaderboard
#'
#' Find out which models have been created. Used to get the Model ID.
#'
#' @param object An object created by [automl_reg()]
#'
#' @return A tibble containing the H2O AutoML Leaderboard
#' 
#' @description 
#' The H2O AutoML Leaderboard lists any models that have been created during the `automl_reg()`
#' training process. The training process automatically uses the top model. However, users
#' can change the model used using `automl_change_model()`.
#' 
#' 
#'
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
