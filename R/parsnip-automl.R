# H20 AUTOML ----

#' General Interface for H2O AutoML Time Series Models
#'
#' `automl_reg()` is a way to generate a _specification_ of a AutoML model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `h2o`.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#'
#' @details
#'
#'  Other options and arguments can be set using `set_engine()`. 
#'
#' The model can be created using the fit() function using the following engines:
#'
#' - __H2O__ "h2o" (the default)
#'
#' @section Engine: h2o
#'
#' The engine uses `h2o.automl()`.
#' 
#' @section Fit Details:
#'
#' The following features are REQUIRED to be available in the incoming data for the
#' fitting process.
#'
#' - __Fit:__ `fit(y ~ ., data)`: Includes a target feature that is a
#' function of a "date" feature. 
#' 
#' - __Predict:__ `predict(model, new_data)` where `new_data` contains
#'  a column named "date".
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#' 
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' \dontrun{
#' library(tidymodels)
#' library(tidyverse)
#' library(timetk)
#' library(modeltime.h2o)
#' library(h2o)
#' 
#' data_tbl <- walmart_sales_weekly %>%
#'             select(id, Date, Weekly_Sales)
#'             
#' splits <- timetk::time_series_split(data_tbl, assess = "3 month", cumulative = TRUE)
#' 
#' recipe_spec <- recipe(Weekly_Sales ~ ., data = training(splits)) %>%
#'                step_timeseries_signature(Date)
#' 
#' train_tbl <- rsample::training(splits) %>% bake(prep(recipe_spec), .)
#' test_tbl  <- rsample::testing(splits) %>% bake(prep(recipe_spec), .)
#' 
#' h2o.init(nthreads = -1,
#'          ip = 'localhost',
#'          port = 54321)
#'          
#'  
#' 
#' # ---- MODEL SPEC ----
#' model_spec <- automl_reg(mode = 'regression') %>%
#'   parsnip::set_engine('h2o',
#'                      max_runtime_secs = 30, 
#'                      max_runtime_secs_per_model = 30,
#'                      project_name = 'project_01',
#'                      nfolds        = 5,
#'                      max_models    = 1000,
#'                      exclude_algos = c("DeepLearning"),
#'                      seed          =  786) 
#'
#' model_spec
#'
#' # ---- TRAINING ----
#' # Important: Make sure the date is included as regressor.
#' model_fitted <- model_spec %>%
#'     fit(Weekly_Sales ~ ., data = train_tbl)
#'
#' model_fitted
#'
#' # ---- PREDICT ----
#' # - IMPORTANT: New Data must have date feature
#'
#' stats::predict(model_fitted, test_tbl)
#' }
#'
#' @export
automl_reg <- function(mode = "regression") {

  args <- list()

  parsnip::new_model_spec(
    "automl_reg",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = "h2o"
  )

}

#' @export
print.automl_reg <- function(x, ...) {
  cat("H2O AutoML Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }

  invisible(x)
}



#' @export
#' @importFrom parsnip translate
translate.automl_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'h2o'` for translation.")
    engine <- "h2o"
  }
  x <- parsnip::translate.default(x, engine, ...)

  x
}

# FIT -----

#' H2O AutoML Modeling Function (Bridge)
#'
#' @param formula formula specified for training
#' @param data A dataframe containing the training data
#' @param ... Additional arguments.
#' 
#' @export
automl_fit_impl <- function(formula, data, ...) {

  others <- list(...)

  y <- all.vars(formula)[1]

  x <- attr(stats::terms(formula, data = data), "term.labels")
  
  if (!inherits(data, "H2OFrame")){
    
    training_frame <- data %>%
                      dplyr::mutate_if(is.ordered, ~{factor(.x, ordered = F)}) %>%
                      h2o::as.h2o()
    
  } else { 
    
    training_frame <- data;
    data <- dplyr::as_tibble(data)
    
  }

  
  # INDEX & PERIOD
  # Determine Period, Index Col, and Index
  index_tbl <- modeltime::parse_index_from_data(data %>% dplyr::select(dplyr::all_of(x)))
  period    <- modeltime::parse_period_from_index(index_tbl, 'auto')
  idx_col   <- names(index_tbl)
  idx       <- timetk::tk_index(index_tbl)

  args <- list(
    x = x,
    y = y,
    training_frame = training_frame
  )

  res <- make_h2o_call("h2o.automl", args, others)

  model <- tidyr::as_tibble(res@leaderboard) %>%
           dplyr::slice(1) %>%
           dplyr::pull(model_id) %>%
           h2o::h2o.getModel()
  
  .f <- purrr::compose(purrr::partial(h2o::h2o.predict, newdata = training_frame),
                       tidyr::as_tibble,
                       purrr::as_vector,
                       base::as.numeric,
                       .dir = 'forward')
  
  # RETURN
  modeltime::new_modeltime_bridge(
    class = "automl_fit_impl",

    # Models
    models = list(
      model_1 = model
    ),

    # Data - .actual, .fitted, and .residuals columns
    data = tibble::tibble(
      !!idx_col    :=  idx,
      .actual       =  base::as.numeric(data[[y]]),
      .fitted       =  .f(model),
      .residuals    =  base::as.numeric(data[[y]]) - .f(model)
    ),

    # Description - Convert arima model parameters to short description
    desc = stringr::str_glue('H2O AutoML - {stringr::str_to_title(model@algorithm)}') 
  )

}

make_h2o_call <- function(.fn, args, others) {

  # remove args with NULLs
  args <- args[lengths(args) != 0]

  # create unevaluated model call
  model_call <- rlang::call2(.fn = .fn, !!!args, .ns = "h2o")

  # add others if not NULL
  if (length(others) > 0) {
    model_call <- rlang::call_standardise(model_call)
    model_call <- rlang::call_modify(model_call, !!!others)
  }

  rlang::eval_tidy(model_call)
}

#' @export
print.automl_fit_impl <- function(x, ...) {
  cat("\n")
  cat(x$desc)
  cat("\n")
  cat("--------")
  cat("\nModel: ")
  print(x$models$model_1)
  invisible(x)
}

# PREDICT ----

#' @export
predict.automl_fit_impl <- function(object, new_data, ...) {
  automl_predict_impl(object, new_data, ...)
}


#' Bridge prediction Function for H2O AutoML Models
#'
#' @inheritParams parsnip::predict.model_fit
#' @param ... Additional arguments passed to `h2o::h2o.predict()`
#'
#' @export
automl_predict_impl <- function(object, new_data, ...) {

  # PREPARE INPUTS
  model  <- object$models$model_1
  
  if (!inherits(new_data, "H2OFrame")) {

    new_data <- new_data %>%
      dplyr::mutate_if(is.ordered, ~{factor(.x, ordered = F)}) %>%
      h2o::as.h2o()
    
  }

  preds <- h2o::h2o.predict(model, new_data, ...) %>% 
           tidyr::as_tibble() %>%
           dplyr::pull(predict)
  
  return(preds)

}
