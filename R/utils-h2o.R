
# LEADERBOARD UTILITIES ----

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

# CHANGE MODEL UTILITIES ----

change_h2o_model <- function(object, model) {
    
    # Check object is trained
    msg1 <- "Model or workflow object is not trained."
    if (!is_trained(object)) stop(msg1)
    
    # Get h2o model
    if (is.character(model)) {
        h2o_model <- h2o.getModel(model)
    } else if (attr(model_id, "package") == "h2o") {
        h2o_model <- model
    } else {
        rlang::abort("`model_id` is not valid.")
    }
    
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





