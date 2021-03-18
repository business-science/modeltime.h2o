testthat::context("H2O AUTOML TEST")

# SETUP ----

h2o.init(
    nthreads = -1,
    ip       = 'localhost',
    port     = 54321
)

# Model Spec
model_spec <- automl_reg(mode = 'regression') %>%
    set_engine(
        engine                     = 'h2o',
        max_runtime_secs           = 5, 
        max_runtime_secs_per_model = 4,
        nfolds                     = 5,
        max_models                 = 3,
        exclude_algos              = c("DeepLearning"),
        seed                       =  786
    ) 

# PARSNIP ----

test_that("automl_reg: Parsnip Test", {
    
    testthat::skip_on_cran()
    
    model_fit <<- model_spec %>%
        fit(value ~ ., data = training(m750_splits))
    
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(m750_splits)) %>%
        modeltime_forecast(new_data = testing(m750_splits))
    
    # $fit
    testthat::expect_s3_class(model_fit$fit, "automl_fit_impl")
    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")
    testthat::expect_equal(names(model_fit$fit$data)[1], "date")
    
    # $preproc
    testthat::expect_equal(model_fit$preproc$y_var, "value")
    

    # Structure
    testthat::expect_identical(nrow(testing(m750_splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(m750_splits)$date, predictions_tbl$.index)
    
    # Out-of-Sample Accuracy Tests
    
    resid <- testing(m750_splits)$value - predictions_tbl$.value
    
    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 5000)
    
    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 1000)
    
})


# ---- WORKFLOWS ----

test_that("automl_reg: Workflow Test", {
    
    testthat::skip_on_cran()
    
    # Recipe spec
    recipe_spec <- recipe(value ~ date, data = training(m750_splits)) %>%
        step_log(value, skip = FALSE) %>%
        step_date(date, features = "month") %>%
        step_mutate(date_num = as.numeric(date))
    
    # Workflow
    wflw <- workflow() %>%
        add_recipe(recipe_spec) %>%
        add_model(model_spec)
    
    wflw_fit <<- wflw %>%
        fit(training(m750_splits))
    
    # Forecast
    predictions_tbl <- wflw_fit %>%
        modeltime_calibrate(testing(m750_splits)) %>%
        modeltime_forecast(
            new_data = testing(m750_splits), 
            actual_data = training(m750_splits)
        ) %>%
        mutate_at(vars(.value), exp)
    
    # Tests
    
    testthat::expect_s3_class(wflw_fit$fit$fit$fit, "automl_fit_impl")
    
    # Structure
    
    testthat::expect_s3_class(wflw_fit$fit$fit$fit$data, "tbl_df")
    testthat::expect_equal(names(wflw_fit$fit$fit$fit$data)[1], "date")
      
    # $preproc
    mld <- wflw_fit %>% workflows::pull_workflow_mold()
    testthat::expect_equal(names(mld$outcomes), "value")
    
    full_data <- bind_rows(training(m750_splits), testing(m750_splits))
    
    # Structure
    testthat::expect_identical(nrow(full_data), nrow(predictions_tbl))
    testthat::expect_identical(full_data$date, predictions_tbl$.index)
    
    # Out-of-Sample Accuracy Tests
    predictions_tbl <- predictions_tbl %>% filter(.key == "prediction")
    resid <- testing(m750_splits)$value - predictions_tbl$.value
    
    # - Max Error less than 1500
    testthat::expect_lte(max(abs(resid)), 5000)
    
    # - MAE less than 700
    testthat::expect_lte(mean(abs(resid)), 1000)
    
})


# AUTOML LEADERBOARD ----

test_that("automl_leaderboard() works.", {
  
  testthat::skip_on_cran()
  
  # PASS 
  expect_s3_class(automl_leaderboard(model_fit), "tbl_df")
  
  expect_s3_class(automl_leaderboard(wflw_fit), "tbl_df")
  
  # ERRORS 
  
  # Workflow is not trained
  expect_error(
    automl_leaderboard(workflow())
  )
  
  # Workflow iw not trained
  expect_error(
    workflow() %>% 
      add_model(automl_reg() %>% set_engine("h2o")) %>%
      automl_leaderboard()
  )
  
  # Model spec not trained
  expect_error(
    automl_leaderboard(automl_reg())
  )
  
  # Incorrect object
  expect_error(
    automl_leaderboard("a")
  )
  
  
})


# CHANGE AUTOML MODEL ----
test_that("automl_change_model() works.", {
  
  testthat::skip_on_cran()
  
  # Parsnip 
  model_ids <- automl_leaderboard(model_fit) %>% pull(model_id)
  
  model_id_1 <- model_ids[1]
  model_id_2 <- model_ids[2]
  
  model_fit_swapped <- automl_change_model(model_fit, model_id_2)
  
  model_2 <- h2o.getModel(model_id_2)
  
  expect_equal(model_fit_swapped$fit$models$model_1, model_2)
  
  expect_equal(
    model_fit_swapped$fit$desc, 
    stringr::str_glue('H2O AutoML - {stringr::str_to_title(model_2@algorithm)}')
  )
  
  # Workflow 
  model_ids <- automl_leaderboard(wflw_fit) %>% pull(model_id)
  
  model_id_1 <- model_ids[1]
  model_id_2 <- model_ids[2]
  
  model_fit_swapped <- automl_change_model(wflw_fit, model_id_2)
  
  model_2 <- h2o.getModel(model_id_2)
  
  expect_equal(model_fit_swapped$fit$fit$fit$models$model_1, model_2)
  
  expect_equal(
    model_fit_swapped$fit$fit$fit$desc, 
    stringr::str_glue('H2O AutoML - {stringr::str_to_title(model_2@algorithm)}')
  )
  
  
  # Errors
  expect_error(
    automl_change_model("a")
  )
  
  
})

