# PACKAGE IMPORTS ----

#' @import h2o 
#' @import modeltime
#' @importFrom stats predict

# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
    # This defines the model database
    
    #H2O AutoML
    make_automl()
}

.onDetach <- function(libpath) {
    
    detach("package:h2o")

}
