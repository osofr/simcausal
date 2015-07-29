## ---------------------------------------------------------------------
#' @title Class for storing a node object (the data generating distribution specified by the SEM)
#' @docType class
#' @format An R6 class object.
#' @name Node_Class
#' @details Following fields are created during initialization
#' \itemize{
#' \item{nodes} ...
#' \item{subset_regs} ...
#' \item{sA_nms} ...
#' \item{sW_nms} ...
#' \item{Kmax} ...
#' }
#' Description...
#' Description...
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
# @export
Node_Class <- R6Class("Node_Class",
  class = TRUE,
  portable = TRUE,
  public = list(
    name = character(),
    t = NULL,
    distr = character(),
    dist_params = list(),
    Kmax = NULL,
    order = NULL,
    EFU = NULL,

    # capture the user environment; user.env is used when eval'ing sVar exprs (enclos = user.env)
    initialize = function(user.env, netind_cl) {
      self$user.env <- user.env
      self$netind_cl <- netind_cl
      self$Kmax <- self$netind_cl$Kmax
      invisible(self)
    },

    newfun = function(data.df) { # list of variable names from data.df with special var name (ANCHOR_ALLVARNMS_VECTOR_0)
      return(list(ANCHOR_ALLVARNMS_VECTOR_0 = colnames(data.df)))
    }
  ),

  active = list(
    placeholder = function() {}
  ),

  private = list(
    privplaceholder = function() {}
  )
)

## ---------------------------------------------------------------------
#' @title Class for storing a DAG object (the data generating distribution specified by the SEM)
#' @docType class
#' @format An R6 class object.
#' @name DAG_Class
#' @details Following fields are created during initialization
#' \itemize{
#' \item{nodes} ...
#' \item{subset_regs} ...
#' \item{sA_nms} ...
#' \item{sW_nms} ...
#' \item{Kmax} ...
#' }
#' Description...
#' Description...
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
# @export
DAG_Class <- R6Class("DAG_Class",
  class = TRUE,
  portable = TRUE,
  public = list(
    user.env = emptyenv(),        # user environment to be used as enclos arg to eval(sVar)
    # Kmax = NULL,
    Nsamp = NULL,				  # sample size (nrows) of the simulation dataset
    # capture the user environment; user.env is used when eval'ing sVar exprs (enclos = user.env)
    initialize = function(user.env, netind_cl) {
      self$user.env <- user.env
      self$netind_cl <- netind_cl
      self$Kmax <- self$netind_cl$Kmax
      invisible(self)
    },


    newfun = function(data.df) { # list of variable names from data.df with special var name (ANCHOR_ALLVARNMS_VECTOR_0)
      return(list(ANCHOR_ALLVARNMS_VECTOR_0 = colnames(data.df)))
    }
  ),

  active = list(
    placeholder = function() {}
  ),

  private = list(
    privplaceholder = function() {}
  )
)

