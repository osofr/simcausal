
# Parse MSM formula
# Create new term object
# Replace all summary measure S() calls in glm formula with "XMSMterms.i" vars that will be eval'ed and defined during MSM evaluation
# Define a DAG w/ time-varying nodes, each node is a summary measure from S(), with mapped node names, e.g., XMSMterms.i <-> S(...)[i]
# Returns modified term.formula object and the new DAG
parse.MSMform <- function(msm.form, t_vec, old.DAG, term_map_tab_old = NULL) {
  # *************
  # Convert the formula into a term object, marking the I() calls as "specials":
  term_form <- terms.formula(as.formula(msm.form), keep.order=TRUE, specials=c("S", "I")) # create term.formula object
  call <- attr(term_form, "variables")    # get the call with all variables from the formula
  # *************

  # Parse glm call tree and find all calls wrapped in S() expressions:
  findS_exprs <- function(x) {
    if (is.atomic(x) || is.name(x)) {
      character()
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(S))) {
        S_exprs <- deparse(x[[2]])
        return(S_exprs)
      } else {
        S_exprs <- character()
      }
      c(S_exprs, unlist(lapply(x, findS_exprs)))
    } else if (is.pairlist(x)) {
        unlist(lapply(x, findS_exprs))
    } else {
        stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }
  # Parse the glm call tree and replace all S() expressions with mapped vars names to be evaluated:
  modS_exprs <- function (x, where = parent.frame()) {
    if (is.atomic(x) || is.name(x)) {
        x   # Leave unchanged
    } else if (is.call(x)) {
      # Replace with glm term S() with its mapped variable name (lookup):
      if (identical(x[[1]], quote(S))) {
        S_expr <- deparse(x[[2]])
        k_indx <- which(S_exprs_vec%in%S_expr)[1]
        parse(text=XMSMterms[k_indx])[[1]]
      } else {
        as.call(lapply(x, modS_exprs, where = where))
      }
    } else if (is.pairlist(x)) {
      as.pairlist(lapply(x, modS_exprs, where = where))
    } else {
      stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }

  S_exprs_vec <- unique(findS_exprs(call))  # vector of unique S expressions that need to be evaluated by parser

  # There are S() terms (summary measures) that need to be evaluated:
  if (length(S_exprs_vec)>0) {
    # No previously eval'ed S_exprs_vec => need to define a new mapping and evaluate (by defining a DAG of these summary measures):
    if (is.null(term_map_tab_old)) {
      XMSMterms <- "XMSMterm." %+% seq(S_exprs_vec)  # vector of corresponding term names (column names) that replace S exprs
    # Summary measures have been evaluated before => get the old mapping for these summary measure variable names:
    } else {
      S_exprs_vec_old <- as.character(term_map_tab_old[, "S_exprs_vec"])
      XMSMterms_old <- as.character(term_map_tab_old[, "XMSMterms"])
      map_idx <- NULL
      for (S_exprs in S_exprs_vec) {
        map_idx <- c(map_idx, which(S_exprs_vec_old %in% S_exprs))
      }
      XMSMterms <- XMSMterms_old[map_idx]
      if (length(XMSMterms)!=length(S_exprs_vec)) stop("unable to map some of S() expressions in MSM formula, check that all of the summary measure expressions have been previously defined")
    }

    term_maptab <- data.frame(S_exprs_vec = S_exprs_vec, XMSMterms = XMSMterms, stringsAsFactors = FALSE)
    # Modify the original MSM glm call with new names for S_exprs_vec (summary measures):
    mod_S_call <- modS_exprs(call)
    # Add modified MSM glm call to term formula object:
    attr(term_form, "variables") <-  mod_S_call

    dprint("original call"); dprint(call)
    dprint("modified S() call"); dprint(mod_S_call)
    dprint("mapping of S(.) MSM formula terms to data.frame variables"); dprint(term_maptab)

    # Create a DAG with nodes given by above MSM terms (summary measure DAG):
    MSMtermsD <- DAG.empty()
    for (i in seq(XMSMterms)) {
      MSMtermsD <- MSMtermsD + node(XMSMterms[i], t = t_vec,  distr = "rconst", const = .(parse(text = S_exprs_vec[i])[[1]]))
    }
    # Add user.env to all DAG nodes:
    user.env <- attributes(old.DAG)$user.env
    assert_that(!is.null(user.env))
    MSMtermsD <- lapply(MSMtermsD, function(Dnode) {Dnode[["node.env"]] <- user.env; Dnode})
    class(MSMtermsD) <- "DAG"
    attributes(MSMtermsD)$user.env <- user.env
  # no S() terms detected in the MSM glm call, nothing to change:
  } else {
    MSMtermsD <- NULL
    term_maptab <- NULL
  }
  return(list(term_form = term_form, MSMtermsDAG = MSMtermsD, term_maptab = term_maptab))
}

#************
# MOVED TO INDIVIDUAL EVALUATION OF PARAMETERS, CALLED RIGHT BEFORE PARAMETER EVAL
#************
subset_dat <- function(df, outcome, t_sel) { # subset full data (in wide format) by time-points that are involved in the outcome
  if (!is.longfmt(df)) {
    dfDAG <- attr(df, "DAG")
    df_vars_sel <- unlist(Nattr(Ntvec(dfDAG, t_sel), "name"))
    # print("df_vars_sel"); print(df_vars_sel)
    var_sel_idx <- c(1,match(df_vars_sel, names(df)))
    if (length(var_sel_idx) < ncol(df)) {
      savedattrs <- CopyAttributes(attrslist=attributes(df), tvals=t_sel) 	# save attributes for original data
      df <- df[, var_sel_idx] # * Subset all actions by t, attributes, covariate values(?)
      attributes(df) <- c(attributes(df), savedattrs)
      attr(df, "node_nms") <- df_vars_sel
    }
    dprint("subsetting by t result"); dprint(head(df, 20)); # print(attributes(df))
    df
  } else {
    message("current target evaluation can't subset data in long format, parameter will be evaluated over the entire t range available in supplied data. Provide input data in wide format to evaluate outcomes over specific t ranges")
    df
  }
}
# subset full data (in long format) by time-points:
subset_dat_long <- function(dt, t_sel) {
  if (is.longfmt(dt)) {
    dtDAG <- attr(dt, "DAG")
    savedattrs <- CopyAttributes(attrslist=attributes(dt), tvals=t_sel) 	# save attributes for original data
    data.table::setkeyv(dt,"t")
    dt_subs <- dt[SJ(t_sel)] # * Subset the data.table by t values

    attributes(dt_subs) <- c(attributes(dt_subs), savedattrs)	# put old attributes back
    # attr(dt_subs, "node_nms") <- dt_subs_vars_sel
    data.table::setkeyv(dt_subs, c("ID", "t"))

    dprint("result of subsetting by t"); dprint(dt_subs); # print(attributes(dt_subs))
    dt_subs
  } else {
    message("this function can only subset data in long format")
    dt
  }
}
#************

#' Define Non-Parametric Causal Parameters
#'
#' Set up the causal target parameter as a vector of expectations, ratio of expectations or contrast of expectations (average treatment effect) over the nodes of specified actions. 
#'These settings are then used to evaluate the true value of the causal target parameter by calling \code{\link{eval.target}} function.
#'
#' @param DAG Object specifying the directed acyclic graph (DAG) for the observed data 
#' @param outcome Name of the outcome node
#' @param t Integer vector of time points to use for expectations, has to be omitted or NULL for non-time-varying DAGs.
#' @param param A character vector \code{"ActionName1"}, specifying the action name for the expectation target parameter; 
#'\code{"ActionName1 / ActionName2"}, for the ratio of expectations of \code{outcome} nodes for actions \code{"ActionName1"} 
#'and \code{"ActionName2"}; \code{"ActionName1 - ActionName2"} for the contrast of expectations of \code{outcome} for actions \code{"ActionName1"} and \code{"ActionName2"}
#' @param ... Additional attributes (to be used in future versions)
#' @param attr Additional attributes (to be used in future versions)
#' @return A modified DAG object with the target parameter saved as part of the DAG,
#'this DAG can now be passed as an argument to \code{\link{eval.target}} function for actual Monte-Carlo evaluation of the target parameter. See Examples.
#' @examples
#'#---------------------------------------------------------------------------------------
#'# EXAMPLE 1: DAG with single point treatment
#'#---------------------------------------------------------------------------------------
#'# Define a DAG with single-point treatment ("Anode")
#'D <- DAG.empty()
#'D <- D + node("W1", distr="rbern", prob=plogis(-0.5))
#'D <- D + node("W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- D + node("Anode", distr="rbern", prob=plogis(-0.5 - 0.3*W1 - 0.3*W2))
#'D <- D + node("Y", distr="rbern", prob=plogis(-0.1 + 1.2*Anode + 0.3*W1 + 0.3*W2), 
#'EFU=TRUE)
#'D_WAY <- set.DAG(D)
#'
#'# Defining interventions (actions)
#'# define action "A1" that sets the treatment node to constant 1
#'D_WAY <- D_WAY + action("A1", nodes=node("Anode",distr="rbern", prob=1))
#'# define another action "A0" that sets the treatment node to constant 0
#'D_WAY <- D_WAY + action("A0", nodes=node("Anode",distr="rbern", prob=0))
#'#---------------------------------------------------------------------------------------
#'# Defining and calculating causal parameters:
#'#---------------------------------------------------------------------------------------
#'# Counterfactual mean of node "Y" under action "A1"
#'D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1")
#'eval.target(D_WAY, n=10000)
#'
#'# Contrasts of means of "Y" under action "A1" minus action "A0"
#'D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1-A0")
#'eval.target(D_WAY, n=10000)
#'
#'# Ratios of "Y" under action "A1" over action "A0"
#'D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1/A0")
#'eval.target(D_WAY, n=10000)
#'
#'# Alternative parameter evaluation by passing already simulated full data to 
#'# \code{eval.target}
#'X_dat1 <- simfull(A(D_WAY), n=10000)
#'D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1/A0")
#'eval.target(D_WAY, data=X_dat1)
#'
#'#---------------------------------------------------------------------------------------
#'# EXAMPLE 2: DAG with time-varying outcomes (survival outcome) 
#'#---------------------------------------------------------------------------------------
#'# Define longitudinal data structure over 6 time-points t=(0:5)
#'t_end <- 5
#'D <- DAG.empty()
#'D <- D + node("L2", t=0, distr="rbern", prob=0.05)
#'D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
#'D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, 
#'ifelse(L1[0]==0 & L2[0]==0, 0.1,
#'ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
#'D <- D + node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE)
#'D <- D + node("Y",  t=0, distr="rbern",
#'prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)),
#'EFU=TRUE)
#'D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1,
#'ifelse(L2[t-1]==1, 0.9,
#'min(1,0.1 + t/16))))
#'D <- D + node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1,
#'ifelse(L1[0]==1 & L2[0]==0, 0.3,
#'ifelse(L1[0]==0 & L2[0]==0, 0.1,
#'ifelse(L1[0]==1 & L2[0]==1, 0.7,
#'0.5)))))
#'D <- D + node("A2", t=1:t_end, distr="rbern", prob=0, EFU=TRUE)
#'D <- D + node("Y",  t=1:t_end, distr="rbern",
#'prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))),
#'EFU=TRUE)
#'D <- set.DAG(D)
#'
#'# Add two dynamic actions (indexed by values of the parameter theta={0,1})
#'# Define intervention nodes
#'act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
#'act_tp_theta <- node("A1",t=1:t_end, distr="rbern", 
#'prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
#'# Add two actions to current DAG object
#'D <- D + action("A1_th0", nodes=c(act_t0_theta, act_tp_theta), theta=0)
#'D <- D + action("A1_th1", nodes=c(act_t0_theta, act_tp_theta), theta=1)
#'#---------------------------------------------------------------------------------------
#'# Defining and calculating the target parameter
#'#---------------------------------------------------------------------------------------
#'# Counterfactual mean of node "Y" at time-point t=4 under action "A1_th0"
#'D <- set.targetE(D, outcome="Y", t=4, param="A1_th0")
#'eval.target(D, n=5000)
#'
#'# Vector of counterfactual means of"Y" over all time points under action "A1_th1"
#'D <- set.targetE(D, outcome="Y", t=0:5, param="A1_th1")
#'eval.target(D, n=5000)
#'
#'# Vector of counterfactual contrasts of "Y" over all time points 
#'# for action "A1_th1" minus action "A1_th0"
#'D <- set.targetE(D, outcome="Y", t=0:5, param="A1_th1 - A1_th0")
#'eval.target(D, n=5000)
#'
#'# Vector of counterfactual ratios of "Y" over all time points 
#'# for action "A1_th0" over action "A1_th1"
#'D <- set.targetE(D, outcome="Y", t=0:5, param="A1_th0 / A1_th1")
#'eval.target(D, n=5000)
#'
#' @export
# function to set the target as expectation of several nodes indexed by time, saves target obj in DAG until evaluation
set.targetE <- function(DAG, outcome, t, param, ...,  attr=list()) {
  # collect all attributes (must be named)
  attrs <- list(...)
  attrs <- append(attrs, attr)
  attnames <- names(attrs)
  if (length(attrs) != 0 && (is.null(attnames) || any(attnames==""))) {
  	stop("please specify name for each attribute")
  }

  attr(DAG, "target")$param_name <- "E" 	# set target param name
  attr(DAG, "target")$params.MSM <- NULL 	# set MSM params to null
  if (missing(t)) t <- NULL
  attr(DAG, "target")$outnodes <- list(gen_name=as.character(outcome), t=t)

  if ((class(param)=="character")&&length(param)==1) {
  	res_ratio <- length(gsub("[[:space:]]*","",unlist(strsplit(param, "/", fixed = TRUE)))) > 1
  	res_diff <- length(gsub("[[:space:]]*","",unlist(strsplit(param, "-", fixed = TRUE)))) > 1
  	interv_nms <- gsub("[[:space:]]*","",unlist(strsplit(param, "[-/]")))	# names of interventions
  	if (length(interv_nms)>2) stop("cannot specify more than two intervention names in param argument")

  	attr(DAG, "target")$params.E <- list(res_ratio=res_ratio, res_diff=res_diff, interv_nms=interv_nms) # record the parameters for expectation target in a list
  		attr(DAG, "target")$attrs <- attrs # record attributes in separate list
  } else {
  	stop("param must be a character string")
  }

  call <- match.call(expand.dots = TRUE) # save the call that was used for calling set.targetE
  attr(DAG, "target")$call <- call # record the initial target param fun call
  DAG
}

#' Define Causal Parameters with a Working Marginal Structural Model (MSM)
#'
#' Set up the MSM causal target parameter for the current DAG object. These settings can be later used to evaluate the true value of the MSM parameter on the full (counterfactual) data by calling \code{eval.target} function.
#'
#' Enclosing an MSM formula term inside S(), e.g., S(mean(A[0:t])), forces this term to be evaluated as a summary measure of time-indexed nodes in the full data environment. All such MSM terms are parsed and then evaluated inside the previously simulated full data environment, each S() term is then replaced with a vector name 'XMSMterms.i' that is a result of this evaluation.
#'
#' @param DAG Object specifying the directed acyclic graph (DAG) for the observed data 
#' @param outcome Name of the outcome node
#' @param t Vector of time points which are used for pooling the \code{outcome}
#' @param formula MSM formula for modeling pooled outcome on the full data with glm regression. Left hand side should be equal to the \code{outcome}, right hand side can include baseline covariates, action-specific attribute names and time-dependent treatment summary measures. See Details.
#' @param family Model family to use in the \code{glm} regression
#' @param hazard When TRUE MSM fits the discrete hazard function for survival \code{outcome} (if outcome node had \code{EOF=TRUE} attribute)
#' @param ... Additional attributes (to be used in future versions)
#' @param attr Additional attributes (to be used in future versions)
#' @return A modified DAG object with well-defined target parameter saved as part of the DAG, this DAG can now be passed as an argument to \code{eval.target} function for actual Monte-Carlo evaluation of the target parameter. See Examples.
#' @examples
#'
#'#---------------------------------------------------------------------------------------
#'# DAG with time-varying outcomes (survival outcome) 
#'#---------------------------------------------------------------------------------------
#'# Define longitudinal data structure over 6 time-points t=(0:5)
#'t_end <- 5
#'D <- DAG.empty()
#'D <- D + node("L2", t=0, distr="rbern", prob=0.05)
#'D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
#'D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5,
#'ifelse(L1[0]==0 & L2[0]==0, 0.1,
#'ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
#'D <- D + node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE)
#'D <- D + node("Y",  t=0, distr="rbern",
#'prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)),
#'EFU=TRUE)
#'D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1,
#'ifelse(L2[t-1]==1, 0.9,
#'min(1,0.1 + t/16))))
#'D <- D + node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1,
#'ifelse(L1[0]==1 & L2[0]==0, 0.3,
#'ifelse(L1[0]==0 & L2[0]==0, 0.1,
#'ifelse(L1[0]==1 & L2[0]==1, 0.7,
#'0.5)))))
#'D <- D + node("A2", t=1:t_end, distr="rbern", prob=0, EFU=TRUE)
#'D <- D + node( "Y",  t=1:t_end, distr="rbern",
#'prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))),
#'EFU=TRUE)
#'D <- set.DAG(D)
#'
#'# Add two dynamic actions (indexed by values of the parameter theta={0,1})
#'# Define intervention nodes
#'act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
#'act_tp_theta <- node("A1",t=1:t_end, distr="rbern",
#'prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
#'# Add two actions to current DAG object
#'D <- D + action("A1_th0", nodes=c(act_t0_theta, act_tp_theta), theta=0)
#'D <- D + action("A1_th1", nodes=c(act_t0_theta, act_tp_theta), theta=1)
#'
#'#---------------------------------------------------------------------------------------
#'# MSM EXAMPLE 1: Modeling survival over time
#'#---------------------------------------------------------------------------------------
#'# Modeling pooled survival Y_t over time as a projection on the following working 
#'# logistic model:
#'msm.form <- "Y ~ theta + t + I(theta*t)"
#'D <- set.targetMSM(D, outcome="Y", t=0:5, formula=msm.form, family="binomial", 
#'hazard=FALSE)
#'MSMres <- eval.target(D, n=1000)
#'MSMres$coef
#'
#'#---------------------------------------------------------------------------------------
#'# MSM EXAMPLE 2: Modeling survival over time with exposure-based summary measures
#'#---------------------------------------------------------------------------------------
#'# Now we want to model Y_t by adding a summary measure covariate defined as mean 
#'# exposure A1 from time 0 to t;
#'# Enclosing any term inside S() forces its evaluation in the environment 
#'# of the full (counterfactual) data.
#'msm.form_sum <- "Y ~ theta + t + I(theta*t) + S(mean(A1[0:t]))"
#'D <- set.targetMSM(D, outcome="Y", t=0:5, formula=msm.form_sum, family="binomial",
#'hazard=FALSE)
#'MSMres <- eval.target(D, n=1000)
#'MSMres$coef
#'
#' @export
# function to set MSM as the target, saves target obj in DAG until evaluation
set.targetMSM <- function(DAG, outcome, t, formula, family="quasibinomial", hazard, ...,  attr=list()) {
  # collect all attributes (must be named)
  attrs <- list(...)
  attrs <- append(attrs, attr)
  attnames <- names(attrs)
  if (length(attrs) != 0 && (is.null(attnames) || any(attnames==""))) {
  	stop("please specify name for each attribute")
  }
  if (missing(t)) t <- NULL

  attr(DAG, "target")$param_name <- "MSM"		# set the target param name
  attr(DAG, "target")$params.E <- NULL 		# set expectation params to null
  attr(DAG, "target")$outnodes <- list(gen_name=as.character(outcome), t=t)
  attr(DAG, "target")$params.MSM <- list(form=formula, family=family) # record the MSM parameters in a list
  if (!missing(hazard)) {
  	attr(DAG, "target")$params.MSM <- c(attr(DAG, "target")$params.MSM, hazard=hazard)
  } else {
  	attr(DAG, "target")$params.MSM <- c(attr(DAG, "target")$params.MSM, hazard=NULL)
  }

  attr(DAG, "target")$attrs <- attrs # record attributes to MSM in separate list

  call <- match.call(expand.dots = TRUE) # save the call that was used for calling set.targetMSM
  attr(DAG, "target")$call <- call # record the initial target param fun call
  DAG
}


#' Evaluate the True Value of the Causal Target Parameter
#'
#' This function estimates the true value of the previously set target parameter (\code{set.targetE} or \code{set.targetMSM}) using the DAG object and either 1) \code{data}: list of action-specific simulated \code{data.frames}; or 2) \code{actions}; or 3) when \code{data} and \code{actions} are missing, using all distinct actions previously defined on the \code{DAG} object.
#'
#' For examples and additional details see documentation for \code{\link{set.targetE}} or \code{\link{set.targetMSM}}
#'
#' @param DAG DAG object with target parameter set via \code{set.targetE} or \code{set.targetMSM} functions
#' @param n Number of observations to simulate (if simulating full data), this is overwritten by the number of observations in each data
#' @param data List of action-specific \code{data.frames} generated with \code{sim} or \code{simfull}
#' @param actions Character vector of action names which play the role of the data generating mechanism for simulated data when argument \code{data} is missing. Alternatively, \code{actions} can be a list of action DAGs  pre-selected with \code{A(DAG)} function. When this argument is missing, full data is automatically sampled from all available actions in the \code{DAG} argument.
#' @param rndseed Seed for the random number generator.
#' @param verbose Set to \code{TRUE} to print messages on status and information to the console. 
#'  Turn this off by default using options(simcausal.verbose=FALSE).
#' @return For targetE returns a vector of counterfactual means, ATE or ATR; for targetMSM returns a named list with the MSM model fit (\code{"msm"}), 
#'MSM model coefficients (\code{"coef"}), the mapping of the MSM summary terms \code{S()} to the actual variable names used in the data, (\code{"S.msm.map"}), 
#'and the long format full data that was used for fitting this MSM \code{"df_long"}.
#' 
#' @importFrom assertthat assert_that is.count
#' @export
eval.target <- function(DAG, n, data, actions, rndseed = NULL, verbose = getOption("simcausal.verbose")) {
  gen_full_dat <- function(actions, wide = TRUE, LTCF = FALSE) { # generate full data when its not provided as an argument
    if (verbose) message("evaluating the target on "%+%n%+%" simulated samples per action")
    if (is.character(actions)) { # grab appropriate actions from the DAG by name
      actions <- getactions(DAG, actions)
    } else if (is.list(actions)) {
      checkDAGs <- sapply(actions, is.DAG)
      if (!all(checkDAGs)) stop("actions must be either a list of DAGs or a vector of action names")
    } else {
      stop("argument actions must be a character vector of action names or a list of action DAGs")
    }
    if (!LTCF) outcome <- NULL
    simfull(actions = actions, n = n, wide = wide, LTCF = outcome, rndseed=rndseed)
  }

  if (missing(DAG)) stop("must specify DAG argument")
  if (!is.DAG(DAG)) stop("DAG must be an object of class DAG")
  if (!is.DAGlocked(DAG)) stop("call set.DAG() and +action before attempting to evaluate target parameters on DAG")
  if (missing(n) & missing(data)) stop("sample size n or data must be supplied")
  if (!missing(data)) {
    if (!is.list(data)) stop("data must be a list of full data (action) data.frames")
    if (!all(sapply(data, is.data.frame))) stop("data must be a list of full data (action) data.frames")
    n_all <- unique(sapply(data, nrow))
    # if (length(n_all)>1) warning("number of rows for different data.frames in data is not unique")
    if (!missing(n)) message("argument n will not be used, the action-specific simulated data is alrady provided by argument data")
    n <- n_all[1]
  }
  if (!missing(n)) {
    assertthat::assert_that(is.count(n))
  }
  if (is.null(attr(DAG, "target"))) stop("need to specify the target parameter by running set.targetMSM() or set.targetE() for DAG object")

  param_name <- attr(DAG, "target")$param_name
  params.E <- attr(DAG, "target")$params.E
  params.MSM <- attr(DAG, "target")$params.MSM
  outnodes <- attr(DAG, "target")$outnodes
  outcome <- attr(DAG, "target")$outnodes$gen_name
  t_act <- attr(DAG, "target")$outnodes$t
  node_nms <- unlist(Nattr(DAG, "name"))

  if (!is.null(t_act)) {
    outnode_nms <- paste0(outcome, "_", t_act) # expand outnodes to a vector of existing outnode names (gen_name x time combo)
  } else {
    outnode_nms <- outcome
  }
  attrs <- attr(DAG, "target")$attrs 	# collect attributes (additional named target specific parameters)

  if (is.null(params.E)&is.null(params.MSM)) stop("at least one target parameter must be specified (set.targetE,set.targetMSM) ")
  if (length(outnodes)!=2) stop("outnodes must be a list of length two")
  if (class(outnodes)!="list") stop("outnodes must be a list")
  if (!all(c("gen_name", "t")%in%names(outnodes))) stop("outnodes must contain named items gen_name and t")
  if (length(outnodes$gen_name)>1) stop("can only specify one generic name in outnodes$gen_name")

  if (!is.null(params.E)) {
    if (missing(data)) {# if no full data, simulate full data from actions argument
      if (verbose) message("data not specified, simulating full data")
      if (missing(actions)) {
        if (verbose) message("no actions specified, sampling full data for ALL actions from the DAG")
        actions <- A(DAG)
      }
      data <- gen_full_dat(actions=actions)
    }
    if(!all(params.E$interv_nms%in%names(data))) stop("some of the actions in param argument could not be found in the simulated full data")
    if (is.longfmt(data[[1]])) stop("full data must be in wide format for target set.targetE, run sim(actions, n)")
    vec_EFUP <- sapply(N(DAG)[outnode_nms], is.EFUP)
    if (any(vec_EFUP)&(!is.LTCF(data[[1]], outcome))) {
      if (verbose) message("some outcome nodes have EFU=TRUE, applying Last Time Point Carry Forward function: doLTCF()")
      data <- lapply(data, doLTCF, LTCF=outcome)
    }
    res <- eval.E(DAG = DAG, df_full = data, outnodes = outnodes, outnode_nms = outnode_nms, params.E = params.E, attrs = attrs)
  } else if (!is.null(params.MSM)) {
    if (is.null(params.MSM$form)) stop("MSM formula must be specified")
    if (is.null(params.MSM$family)) {
      message("using the default glm family for MSM: quasibinomial")
      params.MSM$family <- "quasibinomial"
    }

    if (missing(data)) {# if no full data, simulate full data from actions argument			
      if (verbose) message("data not specified, simulating full data")
      if (missing(actions)) {	
      	if (verbose) message("no actions specified, sampling full data for ALL actions from the DAG")
      	actions <- A(DAG)
      }
      if (is.null(params.MSM$hazard) || (params.MSM$hazard)) { # if hazard is missing or wanted, do not impute forward the outcome
      	LTCF <- FALSE
      } else if (!params.MSM$hazard) { # for survival outcome, impute forward
      	LTCF <- TRUE
      }
      data <- gen_full_dat(actions=actions, wide=TRUE, LTCF=LTCF)
    }
    res <- eval.MSM(DAG=DAG, df_full=data, outnodes=outnodes, outnode_nms=outnode_nms, params.MSM=params.MSM, attrs=attrs, verbose=verbose)
  } else {
    stop("target parameter can be either E (expectation) or working MSM, other parameters are not implemented")
  }
  attr(DAG, "target")$res <- res 	# save each node`s parent set as an attribute of output data.frame
  res
}

# calculate expectations and contrasts over a node or a time-varying vector of a node
eval.E <- function(DAG, df_full, outnodes, outnode_nms, params.E, attrs) {
  interv_nms <- params.E$interv_nms
  res_ratio <- params.E$res_ratio
  res_diff <- params.E$res_diff
  # Calculates the counterfactual mean for EACH intervention (specified in actions) and each outnode	
  # CHECKS THAT NO OUTCOME NODES ARE MISSING (NA)
  outmean_l <- lapply(df_full[interv_nms], function(df) {
                      out <- df[,outnode_nms, drop=FALSE]
                      for (i in (1:ncol(out))) {
                        ###########################################################
                        # CHECK ALL OUTCOMES ARE NOT NA
                        # IF NA -> THROW AN EXCEPTION (EFU EXCEPTION)
                        if (any(is.na(out[,i]))) stop("Unable to evaluate the expectation for outcome "%+%outnode_nms[i]%+% ", some observations are censored before the outcome, the actions were defined incorrectly")
                        ###########################################################
                      }
                      sapply(out, mean)
                    })
  # *) If param is a contrast, contrast those means, otherwise return a vector of means
  if (length(interv_nms)==2) {
    if(res_ratio) {
      out_vec <- outmean_l[[1]]/outmean_l[[2]]
      names(out_vec) <- paste0("Ratio_",outnode_nms)
    } else if (res_diff) {
      out_vec <- outmean_l[[1]]-outmean_l[[2]]
      names(out_vec) <- paste0("Diff_",outnode_nms)
    } else {
      stop("unsupported arithmetic operation with respect to the true mean node values")
    }
  } else {
    if (length(outmean_l)>1) stop("non unique name matching in param and df_full")
    out_vec <- unlist(outmean_l)
    names(out_vec) <- paste0("Mean_",outnode_nms)
  }
  return(list(res=out_vec, call=attr(DAG, "target")$call))
}

# calculate parameters based on MSMs
#	*) combine action-specific datasets into one
#	*) run a working regression model on that full data to calculate the true value of the target param
eval.MSM <- function(DAG, df_full, outnodes, outnode_nms, params.MSM, attrs, verbose = getOption("simcausal.verbose")) {
  outcome <- outnodes$gen_name
  t_vec <- outnodes$t
  form <- params.MSM$form
  family <- params.MSM$family
  hazard <- params.MSM$hazard

  # check hazard argument is specified when outcomes are EFU type
  vec_EFUP <- sapply(DAG[outnode_nms], is.EFUP) # a) check all(is.EFUP(outcomes))
  if (any(vec_EFUP)&&(is.null(hazard))) stop("for failure-type outcomes (EFU=TRUE) the 'hazard' argument must be provided when calling set.targetMSM()")

  LTCF_param <- NULL
  if (!is.null(hazard)) {
    if (!hazard) LTCF_param <- outcome	# for survival outcome, carry last known observation forward after failure event
    sim_paramstr <- "simfull(actions = actions, n = n, LTCF = " %+% LTCF_param %+% ")"
    if (hazard & is.LTCF(df_full[[1]], outcome)) stop("For modeling hazard in target param.MSM full data must be simulated with LTCF=NULL (outcome NOT carried forward), run "%+%sim_paramstr%+%"")
    if (!hazard & !is.LTCF(df_full[[1]], outcome)) stop("For modeling survival in target param.MSM full data must be imputed with LTCF='outcome' (last time point (outcome) carried forward), where 'outcome' is the name of the EOF=TRUE node, run "%+%sim_paramstr%+%"")
    if (hazard) {
      if (!all(vec_EFUP)) stop("for hazard=TRUE all outcome nodes must be set to EFU=TRUE")
      # vec_Bern <- sapply(DAG[outnode_nms], is.Bern) # b) check all(is.binary(outcomes))
      # if (!all(vec_Bern)) stop("for hazard=TRUE all outcome nodes must have Bernoulli distribution")
    }
  }

  #*******************************
  # evaluating the summary measures for wide format df_full and then converting both to long format
  #*******************************
  if (!is.longfmt(df_full[[1]])) {
    parse_form <- parse.MSMform(msm.form = form, t_vec = t_vec, old.DAG = DAG)
    MSMtermsDAG <- parse_form$MSMtermsDAG	# DAG that defines the exposure summaries that need to be eval`ed
    term_maptab <- parse_form$term_maptab

    act_names <- names(df_full)
    if (!is.null(term_maptab)) { # evaluate the summary measures and merge with main data only when S() wrapped terms were found

      XMSM_terms <- as.character(term_maptab[,"XMSMterms"])
      dprint("MSM term names"); dprint(XMSM_terms)
      if (verbose) message("evaluating MSM summary measures and converting full data to long format for MSM target parameter")
      # evaluate summary measures for all observations in df_full, inside df_full environment
      SMSM_Xdat <- lapply(df_full, function(prevdat) simFromDAG(DAG = MSMtermsDAG, Nsamp = nrow(prevdat), LTCF = LTCF_param, prev.data = prevdat))
      # ***** cbind df_full and SMSM_Xdat, saving appropriate attributes
      df_full <- lapply(seq(length(df_full)), function(i_act) {
                        old_attr <- CopyAttributes(attributes(df_full[[i_act]]))
                        MSM_attr <- CopyAttributes(attributes(SMSM_Xdat[[i_act]]))
                        old_attr$DAG <- c(MSM_attr$DAG, old_attr$DAG)	# merge two DAGs (the action and MSM terms)
                        class(old_attr$DAG) <- "DAG"
                        MSM_sum_nodes <- attr(SMSM_Xdat[[i_act]], "node_nms") # get new node names from MSM summary data
                        old_attr$node_nms <- c(old_attr$node_nms, MSM_attr$node_nms)
                        old_attr$attnames <- c(old_attr$attnames, XMSM_terms)
                        sel_cols <- !(names(SMSM_Xdat[[i_act]])%in%("ID"))
                        dat <- cbind(df_full[[i_act]], SMSM_Xdat[[i_act]][,sel_cols])
                        attributes(dat) <- c(attributes(dat), old_attr)
                        attr(dat, "target")$params.MSM.parse <- parse_form
                        dat
                        })
    }
    # SuppressGivenWarnings(df_full <- lapply(df_full, DF.to.longDT), GetWarningsToSuppress())
    df_full <- lapply(df_full, DF.to.longDT, return_DF = FALSE)
    names(df_full) <- act_names
    # * Subset all data.frame variables by t once its in long format
    if(!is.null(t_vec)) df_full <- lapply(df_full, subset_dat_long, t_vec) #this should include the indicators

    #*******************************
    # evaluating the target parameter when the df_full is already in long format (with warnings)
    # ****TO DO**** 
    # ADD SUBSETTING BY t FOR DATA in LONG FORMAT
    #*******************************
  } else {
    MSM.parse_old <- attr(df_full[[1]], "target")$params.MSM.parse
    term_maptab_old <- MSM.parse_old$term_maptab

    dprint("old map of MSM formula terms to vars:"); dprint(term_maptab_old)
    parse_form <- parse.MSMform(msm.form = form, t_vec = t_vec, old.DAG = DAG, term_map_tab_old = term_maptab_old)

    if (!is.null(parse_form$term_maptab) && verbose) {
      message("for df_full in long format new summary measures cannot be calculated, using whatever summary measures already exist in df_full")
      message("for df_full in long format outcome is pooled over the same t vector as defined in the first MSM that generated the long format data, changing pooling t requires re-generating the full data")
      message("assuming the data is based on the following map of MSM terms to variable names")
      print(parse_form$term_maptab)
    }
  }

  naout_byaction <- sapply(df_full, function(df) any(is.na(df[,outcome]))) # CHECK THAT NO OUTCOME NODES ARE MISSING (NA)
  if (any(naout_byaction)) stop("Unable to evaluate MSMs outcome "%+%outcome%+% ", for action(s) ", paste(names(df_full)[naout_byaction], collapse=","), ", some observations are censored before the outcome, check DAG(s) defining the action(s)")

  # * Combine all actions into one dataframe and convert to long format
  df_combine <- data.table::rbindlist(df_full, fill=TRUE)
  dprint("df_combine"); dprint(df_combine)

  # * Run the glm regression for form from params.MSM
  if (verbose) message("MSM: fitting glm to full data")
  m <- glm(parse_form$term_form, data = df_combine, family = family, na.action = na.exclude)
  coef <- coef(m)
  return(list(msm = m, coef = coef, S.msm.map = parse_form$term_maptab, hazard = hazard, call = attr(DAG, "target")$call, df_long = df_full))
}