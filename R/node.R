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
#' Define a single DAG node and its distribution or define many repeated-measure/time-varying nodes by using argument \code{t}.
#' The node distribution is allowed to vary as a function of time (\code{t}). Conditionaing on past nodes is accomplished by using the syntactic sugar, such as, \code{NodeName[t]}.
#' After all the nodes have been added to the DAG, call \code{\link{set.DAG}}, a DAG object constructor, and \code{\link{add.action}}, an action (intervention) constructor.
#'
#' @param name Character node name or a vector of names when specifying a multivariate node. For time-dependent nodes the names will be automatically expanded to a scheme "name_t" for each t provided specified.
#' @param t Node time-point(s). Allows specification of several time-points when t is a vector of positive integers, in which case the output will consist of a
#' named list of length(t) nodes, corresponding to each value in t.
#' @param distr Character name of the node distribution, can be a standard distribution R function, s.a. rnorm, rbinom, runif or user defined.
#' The function must accept a named argument "n" to specify the total sample size.
#' Distributional parameters (arguments) must be passed as either named arguments to node or as a named list of parameters "params".
#' @param EFU End-of-Follow Up flag for designating a survival/censoring type node, only applies to Bernoulli nodes. When \code{EFU=TRUE} this node becomes an indicator for the end of follow-up event
#' (censoring, end of study, death, etc).
#' When simulated variable with this node distribution evaluates to value 1 subsequent nodes with higher temporal \code{order} values will be set to NA by default
#' (or imputed with carry forward imputation, depending on the settings of the \code{sim} function).
#' This can only be set to \code{TRUE} and should be omitted otherwise.
#' @param order An optional integer parameter specifying the order in which these nodes will be sampled. The value of order has to start at 1 and be unique for each new node,
#' can be specified as a range / vector and has to be of the same length as the argument \code{t} above.
#' When order is left unspecified it will be automatically inferred based on the order in which the node(s) were added in relation to other nodes. See Examples and Details below.
#' @param ... Named arguments specifying distribution parameters that are accepted by the \code{distr} function. The parameters can be R expressions that are themselves formulas of the past node names.
#' @param params A list of additional named parameters to be passed on to the \code{distr} function. The parameters have to be either constants or character strings of R expressions of the past node names.
#' @param asis.params (ADVANCED USE) A list of additional named distributional parameters that will be evaluated "as is",
#' inside the currently simulated data.frame + the calling environment, without any modifications to the R expression strings inside the \code{asis.params} list.
#' There is no error-checking for existing node names and no parent node name extraction (the arrows from parents will not appear in \code{plotDAG}).
#' Time varying nodes should be referenced by their names as they appear in the simulated data, as in \code{"TVar_t"}.
#' See details and examples 7 and 8 below.
#'
#' @section Details:
#'
#' The combination of \code{name} and \code{t} must uniquely identify each node in the DAG. Use argument \code{t} to identify measurements of the same attribute (e.g. 'A1c') at various time points.
#' The collection of all unique \code{t} values, across all nodes, should consist of non-negative integers (i.e., starting at 0).
#'
#' The optional \code{order} argument can be specified, used for determining the sampling order of each node.
#' When \code{order} not specified, it is automatically inferred based on the actual order in which the nodes were added to the DAG (earlier added nodes get lower \code{order} value and are sampled first)
#'
#' All node calls that share the same generic name \code{name} must also share the same \code{EFU} value (if any is specified in at least one of them).
#' A value of \code{TRUE} for the \code{EFU} indicates that if a simulated value for a measurement of the attribute represented by node is 1
#' then all the following nodes with that measurement (in terms of higher \code{t} values) in the DAG will be unobserved (i.e., their simulated value will be set to NA).
#'
#' @section Node formulas (parameters of the distribution):
#'
#' Each user-supplied argument to the node function is an evaluable R expression, their evaluation is delayed until the actual simulation time.
#' These arguments can refer to standard or user-specified R functions that must only apply to the values of parent nodes,
#' i.e. a subset of the node(s) with an \code{order} value strictly lower than that of the node characterized by the formula.
#' Formulas must reference the parent nodes with unique \code{name} identifiers, employing the square bracket vector subsetting \code{name[t]} for referencing a
#' parent node at a particular time point \code{t} (if any time-points were specified).
#' The square bracket notation is used to index a generic name with the relevant time point as illustrated in the examples.
#' When an input node is used to define several nodes (i.e., several measurement of the same attribute, \code{t=0:5}), the formula(s) specified in that node can apply
#' to each node indexed by a given time point denoted by \code{t}. This generic expression \code{t} can then be referenced within a formula to simultaneously identify a
#' different set of parent nodes for each time point as illustrated below. Note that the parents of each node represented by a given \code{node} object are implicitly defined
#' by the nodes referenced in formulas of that \code{node} call.
#'
#' @section Different types of evaluation for node function arguments:
#'
#' There is quite a bit of flexibility in the way in which the \code{node} function arguments can be evaluated.
#' By default, the named arguments specified as expressions are first captured by delayed-evaluation and then
#' modified by \code{simcausal} to enable the special types of functional syntax.
#' For example, simcausal will over-ride the subsetting operators '\code{[...]}' (for time varying nodes) and '[[...]]' (for networks), implying
#' that these operators can no longer be used in their typical \code{R} way.
#' Furthermore, simcausal will over-ride the standard `c` function, with its own definition. Similarly, it will over-ride any calls to \code{sum} and \code{mean} functions
#' with their row-matrix counterpart functions \code{rowSums} and \code{rowMeans}.
#' When programming with \code{simcausal} (such as passing node arguments inside a function, prior to defining the node), it may be helpful to instead pass
#' such node arguments as character strings, rather than as R expressions. In this case one should use the argument \code{params}
#' by adopting the following syntax \code{node(...,params = list(mean="A+B"))}, which in this case is equivalent to: \code{node(..., mean = A+B)}.
#'
#' There are also instances when it might be desirable to retain the original behavior of all \code{R} expressions and functions and evaluate a particular node argument "as is".
#' For example, the user may wish to retain the
#' original \code{R} functionality of all its operators, including those of \code{[...]} and \code{[[...]]}.
#' In this case the node argument (or a specific part of the node argument) should be wrapped in \code{.()} or \code{eval()}.
#' Note that once the expression has been wrapped with \code{.(...)} (or \code{eval(...)}), the \code{simcausal} definitions of operators \code{[...]} and \code{[[...]]} no
#' longer apply to these expressions and no error checking for "correctness" of these node arguments will be performed.
#'
#' The forced-evaluation operator \code{.()} can be also used as part of an expression,
#' which will prevent the typical \code{simcausal} evaluation on only that specific part of the expression. Example 8 below demonstrates the following use case
#' for the expression \code{.(coefAi[t]) * A[t-1]},
#' which will look for vector \code{coefAi} and then subset it by current value of \code{t} (and return a scalar),
#' while \code{A[t-1]} will evaluate to the entire column vector of variable \code{A} for time point \code{t-1}.
#' Such an expression will multiply the entire time-varying vector \code{A[t-1]} by scalar value determined
#' by current value of \code{t} and the previously defined vector \code{coefAi}.
#'
#' Furthermore, even when a vector or a matrix is wrapped in .(...) it still will be automatically re-parsed into K column matrix with n rows.
#' When this is not desired, for example, when defining a multivariate node distribution, the user may pass such vector or matrix node arguments as a character string
#' in a list argument \code{asis.params}. See Example 7 and 8 below for additional details.

#' @section Multivariate random variables (multivariate nodes):
#'
#' Starting from v.0.5, a single \code{node} call can be used for defining a multivariate (and possibly correlated) random vector.
#' To define a random vector that has more than 1 dimension, use the argument \code{name} to specify a vector with names for each dimension, e.g.,
#'
#'\code{node(c("X1","X2"), distr = "rmvnorm", mean = c(0,1)), sigma = matrix(c(1,0.75,0.75,1), ncol=2)}
#'
#' will define a bi-variate (correlated) normally distributed node,
#' the simulated data set will contain this bi-variately distributed random variable in columns "X1" and "X2".
#' Note that the multivariate sampling distribution function (such as function \code{rmvnorm} from the package \code{mvtnorm}) must return a matrix of
#' \code{n} rows (number of observations) and \code{length(name)} columns (dimensionality). See additional examples below.
#'
#' Note that one can also define time-varying multivariate nodes, e.g.,
#'
#'\code{node(c("X1","X2"), t=0:5, distr = "rmvnorm", mean = c(0,1))}.
#'
#' @return A list containing node object(s) (expanded to several nodes if t is an integer vector of length > 1)
#' @example tests/examples/set.DAG.R
#' @export
# Constructor for node objects, uses standard R distribution functions,
# added optional attributes that are saved with the node (such as custom distribution functions, etc)
node <- function(name, t, distr, EFU, order, ..., params = list(), asis.params = list()) {
  env <- parent.frame()

  if (all(grepl("_", name, fixed = TRUE))) stop("node names with underscore character '_' are not allowed")

  # collect all distribution parameters with delayed evaluation (must be named):
  dist_params <- eval(substitute(alist(...)))
  if (length(dist_params)>0) {
    # dist_params <- lapply(dist_params, function(x) deparse(bquote2(x, env)))
    dist_params <- lapply(dist_params, function(x) deparse(x))
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
    EFU <- deparse(substitute(EFU))
    # EFU <- deparse(bquote2(substitute(EFU), env))
  } else {
    EFU <- NULL
  }

  # new check that also checks for distr in the calling environment:
  if (!exists(distr)) {
    if (!exists(distr, envir = env)) {
      namespace_distr <- unlist(strsplit(distr, "::"))
      res <- tryCatch(getFromNamespace(x = namespace_distr[2], ns = namespace_distr[1], envir = env))
      if (inherits(res, "try-error")) stop(distr %+% ": this node distribution function could not be located")
    }
  }

  node_dist_params <- list(distr = distr, dist_params = dist_params, EFU = EFU)

  # If the name arg is a vector of names, this node is a multivar RV
  # Make sure one character string is used for naming the list object:
  if (length(name)>1) {
    node.name <- paste0(name, collapse=".")
    mv.names <- name
    # attr(node_dist_params$dist_params, "asis.flags")[] <- TRUE
  } else {
    node.name <- name
    mv.names <- name
  }

  if (!missing(t)) {
    # expand the nodes into list of lists, with VarName=name%+%t_i:
    if (!is.null(t)) {
      if ((length(t)!=length(order)) & (!is.null(order))) stop("t and order arguments must have the same length")
      node_lists <- lapply(t, function(t_i) {
                              order_t <- order
                              if (!is.null(order)) order_t <- order[which(t%in%t_i)]
                              c(list(name = node.name%+%"_"%+%t_i, mv.names = mv.names%+%"_"%+%t_i, t = t_i), node_dist_params, list(order = order_t, node.env = env))
                              # c(name = name%+%"_"%+%t_i, t = t_i, node_dist_params, order = order_t, node.env = env)
      })
      names(node_lists) <- node.name%+%"_"%+%t
    }
  # Keep node as is, since t is undefined:
  } else {
    node_lists <- list(c(list(name = node.name, mv.names = mv.names, t = NULL), node_dist_params, list(order = order, node.env = env)))
    # node_lists <- list(c(list(name = name, t = NULL), node_dist_params, list(order = order, node.env = env)))
    # node_lists <- list(c(name = name, t = NULL, node_dist_params, order = order, node.env = env))
    # print("node_lists: "); print(node_lists)
    names(node_lists) <- node.name
  }
  if (!check_namesunique(node_lists)) stop("All nodes must have unique name attributes")
  node_lists <- lapply(node_lists, function(node_i) {class(node_i) <- "DAG.node"; node_i})
  class(node_lists) <- "DAG.nodelist"
  node_lists
}