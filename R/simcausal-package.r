#' Simulating Longitudinal Data with Causal Inference Applications
#'
#' The \pkg{simcausal} R package is a tool for specification and simulation of complex longitudinal data structures that are 
#' based on structural equation models. The package provides a flexible tool for conducting transparent and reproducible 
#' simulation studies, with a particular emphasis on the types of data and interventions frequently encountered in typical 
#' causal inference problems, such as, observational data with time-dependent confounding, selection bias, and random monitoring processes.
#' The package interface allows for concise expression of complex functional dependencies between a large number of nodes, 
#' where each node may represent a time-varying random variable. 
#' The package allows for specification and simulation of counterfactual data under various user-specified interventions 
#' (e.g., static, dynamic, deterministic, or stochastic). 
#' In particular, the interventions may represent exposures to treatment regimens, the occurrence or non-occurrence of right-censoring 
#' events, or of clinical monitoring events. \pkg{simcausal} enables the computation of a selected set of user-specified 
#' features of the distribution of the counterfactual data that represent common causal quantities of interest, 
#' such as, treatment-specific means, the average treatment effects and coefficients from working marginal structural models. 
#' For additional details and examples please see the package vignette and the function-specific documentation.
#'
#' @section Documentation:
#' \itemize{
#' \item To see the package vignette use: \code{vignette("simcausal_vignette", package="simcausal")} 
#' \item To see all available package documentation use: \code{help(package = 'simcausal')}
#' }
#'
#' @section Routines:
#' The following routines will be generally invoked by a user, in the same order as presented below.
#' \describe{
#' \item{\code{\link{DAG.empty}}}{Initiates an empty \code{DAG} object that contains no nodes.}
#' \item{\code{\link{node}}}{Defines node(s) in the structural equation model and its conditional distribution(s) using a language of vector-like R expressions. A call to \code{node} can specify either a single node or multiple nodes at once.}
#' \item{\code{\link{add.nodes}} or \code{+\link{node}}}{Provide two equivalent ways of growing the structural equation model by adding new nodes and their conditional distributions. 
#' Sequentially define nodes in the \code{DAG} object, with each node representing the outcomes of one or more structural equation(s), altogether making-up the causal model of interest.}
#' \item{\code{\link{set.DAG}}}{Performs consistency checks and locks the \code{DAG} object so that no additional nodes can be subsequently added to the structural equation model.}
#' \item{\code{\link{sim}} or \code{\link{simobs}}}{Simulates iid observations of the complete node sequence defined by the \code{DAG} object. The output dataset is stored as a \code{data.frame} and is referred to as the \emph{observed data}.}
#' \item{\code{\link{add.action}} or \code{+\link{action}}}{Provide two equivalent ways to define one or more actions.
#' Each action modifies the conditional distribution for a subset of nodes in the original \code{DAG} object. The resulting data generating distribution is referred to as the post-intervention distribution. 
#' It is saved in the \code{DAG} object alongside the original structural equation model (\code{DAG} object).}
#' \item{\code{\link{sim}} or \code{\link{simfull}}}{Simulates independent observations from one or more post-intervention distribution(s). 
#' Produces a named list of \code{data.frame}s, collectively referred to as the \emph{full data}. 
#' The number of output \code{data.frame}s is equal to the number of post-intervention distributions specified in the \code{actions} argument, where each \code{data.frame} object is an iid sample from a particular post-intervention distribution.}
#' \item{\code{\link{set.targetE}} and \code{\link{set.targetMSM}}}{Define two distinct types of target causal parameters. 
#' The function \code{set.targetE} defines causal parameters as the expected value(s) of \code{DAG} node(s) under one post-intervention distribution or the contrast of such expected value(s) from two post-intervention distributions. 
#' The function \code{set.targetMSM} defines causal parameters based on a user-specified \bold{working} marginal structural model.}
#' \item{\code{\link{eval.target}}}{Evaluates the previously defined causal parameter using simulated full data}
#' }
#' 
#' @section Data structures:
#' The following most common types of output are produced by the package:
#' \itemize{
#' \item \emph{parameterized causal \code{DAG} model} - object that specifies the structural equation model, along with interventions and the causal target parameter of interest.
#' \item \emph{observed data} - data simulated from the (pre-intervention) distribution specified by the structural equation model.
#' \item \emph{full data} - data simulated from one or more post-intervention distributions defined by actions on the structural equation model.
#' \item \emph{causal target parameter} - the true value of the causal target parameter evaluated with full data. 
#' }
#'
#' @section Updates:
#' Check for updates and report bugs at \url{http://github.com/osofr/simcausal}.
#'
#' @docType package
#' @name simcausal
#'
NULL

#' @importFrom graphics legend par plot
#' @importFrom stats as.formula glm na.exclude rbinom reshape rnorm runif setNames terms.formula
#' @importFrom utils head str
NULL


# \item {\code{node}} - defines a node in the structural equation model and
# its conditional distribution, i.e., the outcome of one equation in the
# structural equation model and the formula that links the outcome value
# to that of earlier covariates, referred to as parent nodes. A call to \code{node}
# can specify either a single node or multiple nodes at once, with \code{name}
# and \code{distr} being the only required arguments. To specify multiple
# nodes with a single \code{node} call, one must also provide an indexing
# vector of integers as an argument \code{t}. In this case, each node shares
# the same name, but is indexed by distinct values in \code{t}. The simultaneous
# specification of multiple nodes is particularly relevant for providing
# a shorthand syntax for defining a time-varying covariate, i.e., for defining
# repeated measurements over time of the same subject-matter attribute.

# \item {\code{add.nodes} or \code{D + node}} - provide two equivalent ways of growing the structural equation model by adding new nodes and their conditional distributions. Informally, these routines are intended to be used to sequentially populate a \code{DAG} object with all the structural equations that make up the causal model of interest.

# \item [{\code{set.DAG}}] performs consistency checks and locks the \code{DAG} object so that no additional nodes can be subsequently added to the structural equation model.
# In addition, this routine performs several consistency checks of
# the user-populated \code{DAG} object. In particular, the routine attempts
# to simulate observations to verify that all conditional distributions in
# the \code{DAG} object are well-defined.

# \item [{\code{sim}}] simulates iid observations of the complete node sequence defined by a \code{DAG} object. The output dataset is stored as a \code{data.frame} and is referred to as the \emph{observed data}. 
# The output data can be structured in either long or wide formats.

# \item [{\code{add.action} or \code{D + action}}] provides two equivalent ways to define one or more actions. 
# An action modifies the conditional distribution of one or more nodes of the structural equation model. 
# The resulting data generating distribution is referred to as the post-intervention distribution. It is saved in the \code{DAG} object alongside the original structural equation model.

# \item [{\code{sim}}] can also be used for simulating independent observations from one or more post-intervention distributions, as specified by the \code{actions} argument.
# The output is a named list of \code{data.frame} objects, collectively referred to as the \emph{full data}. 
# The number of \code{data.frame} objects in this list is equal to the number of post-intervention distributions specified in the \code{actions}
# argument, where each \code{data.frame} object is an iid sample from a particular post-intervention distribution.

# \item [{\code{set.targetE} and \code{set.targetMSM}}]  
# The function \code{set.targetE} defines causal parameters as the expected value(s) of \code{DAG} node(s) under one post-intervention distribution or the contrast of such expected value(s) from two post-intervention distributions. 
# The function \code{set.targetMSM} defines causal parameters based on a user-specified \textbf{working} marginal structural model.
# The true value of the causal parameter is defined by one or several post-intervention distributions and can thus be approximated using full data.
# The output is the modified \code{DAG} object with the definition of the target causal parameter saved alongside the interventions.

# \item [{\code{eval.target}}] evaluates the causal parameter of interest using simulated full data. As input, it can take previously simulated full data (i.e., the output of a call to the \code{simfull} function) or, alternatively, the user can specify the sample size \code{n}, based on which full data will be simulated first.









