# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

# AutoML ----

make_automl <- function() {

  # SETUP
  model <- "automl_reg"
  mode  <- "regression"
  eng   <- "h2o"

  parsnip::set_new_model(model)
  parsnip::set_model_mode(model, mode)

  # automl_reg: regression ----

  # * Model ----
  parsnip::set_model_engine(model, mode = mode, eng = eng)
  parsnip::set_dependency(model, eng = eng, pkg = "modeltime.h2o")
  parsnip::set_dependency(model, eng = eng, pkg = "h2o")

  # * Encoding ----
  parsnip::set_encoding(
    model   = model,
    eng     = eng,
    mode    = mode,
    options = list(
      predictor_indicators = "none",
      compute_intercept    = FALSE,
      remove_intercept     = FALSE,
      allow_sparse_x       = FALSE
    )
  )

  # * Fit ----
  parsnip::set_fit(
    model         = model,
    eng           = eng,
    mode          = mode,
    value         = list(
      interface = "formula",
      protect   = c("formula", "data"),
      func      = c(fun = "automl_fit_impl"),
      defaults  = list()
    )
  )

  # * Predict ----
  parsnip::set_pred(
    model         = model,
    eng           = eng,
    mode          = mode,
    type          = "numeric",
    value         = list(
      pre       = NULL,
      post      = NULL,
      func      = c(fun = "predict"),
      args      =
        list(
          object   = rlang::expr(object$fit),
          new_data = rlang::expr(new_data)
        )
    )
  )

}

# nocov end
