# SAVE/LOAD ----

#' Saving and Loading Modeltime H2O Models
#'
#' H2O AutoML models require a special storage process that saves / loads the
#' recipe used to recreate a model to / from a directory that the user
#' defines.
#'
#' @param object A fitted model object
#' @param path A directory to store the H2O AutoML model files
#' @param overwrite Whether or not to allow overwriting a H2O AutoML model's directory. Default: FALSE.
#' 
#' @return 
#' - `save_h2o_model()`: No return value, called for side effects (composes a directory of model files)
#' - `load_h2o_model()`: No return value, called for side effects (reads a directory of model files)
#' 
#' @examples
#' \dontrun{
#' library(tidymodels)
#' library(tidyverse)
#' library(timetk)
#' library(modeltime.h2o)
#' 
#' h2o.init()
#'
#' model_fit <- automl_reg(mode = 'regression') %>%
#'     set_engine(
#'         engine                     = 'h2o',
#'         max_runtime_secs           = 30, 
#'         max_runtime_secs_per_model = 30,
#'         project_name               = 'project_01',
#'         nfolds                     = 5,
#'         max_models                 = 1000,
#'         exclude_algos              = c("DeepLearning"),
#'         seed                       =  786
#'     ) %>%
#'     fit(value ~ date + id, m750)
#'
#' # Saves the related files needed to recreate the model
#' model_fit %>% save_h2o_model(path = "/dir_h2o_automl_model/")
#' 
#' # Loads the model
#' load_h2o_model(path = "/dir_h2o_automl_model/")
#'
#' }
#'
#' @export
save_h2o_model <- function(object, path, overwrite = FALSE) {
    
    # Check Class
    is_acceptable_class <- c("workflow", "model_fit") %>%
        purrr::map_lgl(.f = function(cl) inherits(object, cl)) %>%
        any()
    if (!is_acceptable_class) {
        rlang::abort("'object' must be class 'workflow' or 'model_fit'.")
    }
    
    # Check Path
    path_extension <- fs::path_ext(path)
    if (path_extension != "") {
        msg <- glue::glue("'path' should be a directory only. Found extension: {path_extension}. No file extensions are permitted.")
        rlang::abort(msg)
    }
    
    # If directory exists, check if OK to overwrite
    if (!overwrite) {
        if (fs::dir_exists(path)) {
            msg <- glue::glue("A directory exists at path: {path}. Use 'overwrite = TRUE' to overwrite.")
            rlang::abort(msg)
        }
    }
    
    # If No Directory, create it
    if (!fs::dir_exists(path)) {
        fs::dir_create(path)
    }
    
    # SAVE PROCEDURE
    
    # 1. Save the modeltime model
    
    rds_path <- fs::path(path, "modeltime_model.rds")
    saveRDS(object, file = rds_path)
    
    # 2. Save (Serialize) the H2O model
    
    if (inherits(object, "workflow")) {
        # Is workflow
        h2o_model <- object$fit$fit$fit$models$model_1
    } else {
        # Is parsnip model_fit
        h2o_model <- object$fit$models$model_1
    }
    
    fileout <- h2o::h2o.saveModel(
        object = h2o_model,
        path   = path,
        force  = overwrite
    )
    
    file.rename(fileout, fs::path(path, 'h2o_model'))
    
    msg <- glue::glue("\n\nModel saved at path: {path}")
    message(msg)
}

#' @rdname save_h2o_model 
#' @export
load_h2o_model <- function(path) {
    
    # Check Path
    path_extension <- fs::path_ext(path)
    if (path_extension != "") {
        msg <- glue::glue("'path' should be a directory only. Found extension: {path_extension}. No file extensions are permitted.")
        rlang::abort(msg)
    }
    
    # 1. Load the modeltime model
    rds_path <- fs::path(path, "modeltime_model.rds")
    object   <- readRDS(file = rds_path)
    
    # 2. Load the H2O model
    path  <- fs::path(path, "h2o_model")
    model <- h2o::h2o.loadModel(path)
    
    # 2. Recombine the modeltime model and the gluon model
    if (inherits(object, "workflow")) {
        # Is workflow
        object$fit$fit$fit$models$model_1 <- model
    } else {
        # Is parsnip model_fit class
        object$fit$models$model_1 <- model
    }
    
    return(object)
}