# evaluate expressions enclosed by .(expression) right away in the parent (calling) environment
bquote2 <- function (x, where = parent.frame()) { 
  if (is.atomic(x) || is.name(x)) { # Leave unchanged
    x
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(.))) { # Call to .(), so evaluate
    eval(x[[2]], where)
    } else { # Otherwise apply recursively, turning result back into call
      as.call(lapply(x, bquote2, where = where))
    }
  } else if (is.pairlist(x)) {
    as.pairlist(lapply(x, bquote2, where = where))
  } else { # User supplied incorrect input
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

#' Create Node Object(s)
#'
#' This function provides a convenient way to define a node and its distribution in a time-varying format without unnecessary code repetition. 
#' The node distribution is allowed to vary as a function of time (\code{t}), with subsetting of the past nodes accomplished via \code{NodeName[t]}.
#' Intended for use in conjunction with functions \code{\link{set.DAG}}, a DAG object constructor, and \code{\link{add.action}}, an action (intervention) constructor.
#'
#' The combination of a generic name \code{name} and time point \code{t} must be unique in the sense that no other user-specified input node can result in the same 
#' combination of \code{name} and time point \code{t}. 
#' In other words, the combination of \code{name} and \code{t} must uniquely identify each node in the DAG. 
#' The user should use the same \code{name} to identify measurements of the same attribute (e.g. 'A1c') at various time points.
#'
#' All nodes indexed by the same time point \code{t} value must have consecutive \code{order} values. 
#' The \code{order} values of all nodes indexed by the same \code{t} value must have their \code{order} values: 
#' 1) strictly greater than the \code{order} values of all nodes indexed by a strictly lower \code{t} value and 
#' 2) strictly lower than the \code{order} values of all nodes indexed by a strictly higher \code{t} value. 
#' All nodes of a DAG must have consecutive \code{order} values starting at one. 
#' The collection of unique \code{t} values of all nodes of a DAG must be consecutive values starting at 0.
#'
#' All node calls that share the same generic name \code{name} must also share the same \code{EFU} value (if any is specified in at least one of them). 
#' A value of \code{TRUE} for the \code{EFU} indicates that if a simulated value for a measurement of the attribute represented by node is 1 
#' then all the following nodes with that measurement (in terms of higher \code{t} values) in the DAG will be unobserved (i.e., their simulated value will be set to NA).
#'
#' Each formula of an input node is an evaluable R expression. All formulas are delayed in the evaluation until the simulation time.
#' Formulas can refer to standard or user-specified R functions that must only apply to the values of parent nodes, 
#' i.e. a subset of the node(s) with an \code{order} value strictly lower than that of the node characterized by the formula. 
#' Formulas must reference the parent nodes with unique \code{name} identifiers, employing the square bracket vector subsetting \code{name[t]} for referencing a 
#' parent node at a particular time point \code{t} (if any time-points were specified). 
#' The square bracket notation is used to index a generic name with the relevant time point as illustrated in the examples. 
#' When an input node is used to define several nodes (i.e., several measurement of the same attribute, \code{t=0:5}), the formula(s) specified in that node can apply 
#' to each node indexed by a given time point denoted by \code{t}. This generic expression \code{t} can then be referenced within a formula to simultaneously identify a 
#' different set of parent nodes for each time point as illustrated below. Note that the parents of each node represented by a given \code{node} object are implicitly defined 
#' by the nodes referenced in formulas of that \code{node} call.
#'
#' (ADVANCED USE ONLY) All distribution parameters (e.g., \code{mean}, \code{probs}, \code{sd}, \code{unifmin} and \code{unifmax}) are always interpreted with delayed evaluation, 
#' to force immediate evaluation of a variable \code{Var} wrap the variable inside \code{.()} function, as in \code{.(Var)}. See Example 2 for a working example that evaluates \code{.(t_end)} variable.
#'
#' @param name Character node name, for time-dependent nodes the names will be automatically expanded to a scheme "name_t" for each t provided specified
#' @param t Node time-point(s). Allows specification of several time-points when t is a vector of positive integers, in which case the output will consist of a named list of length(t) nodes, corresponding to each value in t.
#' @param distr Character name of the node distribution, can be a standard distribution R function, s.a. rnorm, rbinom, runif or user defined. The function must accept a named argument "n" to specify the total sample size. Distributional parameters (arguments) must be passed as either named arguments to node or as a named list of parameters "params".
#' @param EFU End-of-Follow Up flag for designating a survival/censoring type node, only applies to Bernoulli nodes. When \code{EFU=TRUE} this node becomes an indicator for the end of follow-up event (censoring, end of study, death, etc). 
#' When simulated variable with this node distribution evaluates to value 1 subsequent nodes with higher temporal \code{order} values will be set to NA by default (or imputed with carry forward imputation, depending on the settings of the \code{sim} function). 
#' This can only be set to \code{TRUE} and should be omitted otherwise.
#' @param order An optional integer parameter specifying the order in which these nodes will be sampled. The value of order has to start at 1 and be unique for each new node, can be specified as a range / vector and has to be of the same length as the argument \code{t} above. When order is left unspecified it will be automatically inferred based on the order in which the node(s) were added in relation to other nodes. See Examples and Details below.
#' @param ... Named arguments specifying distribution parameters that are accepted by the \code{distr} function. The parameters can be R expressions that are themselves formulas of the past node names.
#' @param params A list of additional named parameters to be passed on to the \code{distr} function. The parameters have to be either constants or character strings of R expressions of the past node names.
#' @param asis.params (ADVANCED USE ONLY) A list of additional named distributional parameters that will be evaluated "as is", inside the currently simulated data.frame + the calling environment, without any modifications to the R expression strings inside the \code{asis.params} list. 
#' There is no error-checking for existing node names and no parent node name extraction (the arrows from parents will not appear in \code{plotDAG}). Time varying nodes should be refernced by their names as they appear in the simulated data, as in \code{TVar_t}.
#' @return A list containing node object(s) (expanded to several nodes if t is an integer vector of length > 1)
#' @example tests/RUnit/set.DAG.R
#' @export
# Constructor for node objects, uses standard R distribution functions, 
# added optional attributes that are saved with the node (such as custom distribution functions, etc)
node <- function(name, t, distr, EFU, order, ..., params = list(), asis.params = list()) {
  env <- parent.frame()

  if (grepl("_", name, fixed = TRUE)) stop("node names with underscore character '_' are not allowed")

  # collect all distribution parameters with delayed evaluation (must be named):
  dist_params <- eval(substitute(alist(...)))
  if (length(dist_params)>0) {
    dist_params <- lapply(dist_params, function(x) deparse(bquote2(x, env)))
  }

  # add params from params list:
  dist_params <- append(dist_params, params)
  # add asis.params from asis.params list:
  asis.parnames <- NULL
  if (length(asis.params) > 0) {
    asis.parnames <- names(asis.params)
    dist_params <- append(dist_params, asis.params)
  }

  parnames <- names(dist_params)

  if (length(dist_params) != 0 && (is.null(parnames) || any(parnames==""))) stop("specify the name for each node argument")
  if (length(unique(parnames)) < length(dist_params)) stop("each node argument must have a unique name")

  asis.flags <- vector(mode = "list", length = length(dist_params))
  names(asis.flags) <- parnames
  asis.flags[] <- FALSE
  asis.flags[asis.parnames] <- TRUE
  attr(dist_params, "asis.flags") <- asis.flags

  if (missing(order)) {
    order <- NULL
  }

  if (!missing(EFU)) {
    EFU <- deparse(bquote2(substitute(EFU), env))
    # print("EFU: "); print(class(EFU)); print(EFU)
  } else {
    EFU <- NULL
  }

  # new check that also checks for distr in the calling environment:
  if (!exists(distr)) {
    if (!exists(distr, envir = env)) {
      stop(distr %+% ": this node distribution function could not be located")
    }
  }

  node_dist_params <- list(distr = distr, dist_params = dist_params, EFU = EFU)

  if (!missing(t)) {
    # expand the nodes into list of lists, with VarName=name%+%t_i:
    if (!is.null(t)) {
      if ((length(t)!=length(order)) & (!is.null(order))) stop("t and order arguments must have the same length")
      node_lists <- lapply(t, function(t_i) {
                              order_t <- order
                              if (!is.null(order)) order_t <- order[which(t%in%t_i)]
                              c(name = name%+%"_"%+%t_i, t = t_i, node_dist_params, order = order_t, node.env = env)
      })
      names(node_lists) <- name%+%"_"%+%t
    }
  # Keep node as is, since t is undefined:
  } else {
    node_lists <- list(c(name = name, t = NULL, node_dist_params, order = order, node.env = env))
    names(node_lists) <- name
  }
  if (!check_namesunique(node_lists)) stop("All nodes must have unique name attributes")
  node_lists <- lapply(node_lists, function(node_i) {class(node_i) <- "DAG.node"; node_i})
  class(node_lists) <- "DAG.nodelist"
  node_lists
}