#' Simulating Longitudinal Data with Causal Inference Applications
#'
#' Simcausal is designed for simulating longitudinal data based on the data-generating process specified by the \emph{Structural Equation Model} (SEM).
#' Following the specification of the distribution of the observed data (the \code{DAG} object), the user can set the actions (interventions) by changing the distribution for selected \code{DAG} object nodes and simulate the data based on those actions ("\emph{full data}").
#' The package enables evaluation of various target causal parameters (by using the simulated full data), such as the expectations of selected \code{DAG} object nodes or parameters defined as coeficients of the working marginal structural model (MSM).
#'
#' @section Documentation:
#' \itemize{
#' \item To see the package vignette run: \code{vignette("simcausal_vignette", package="simcausal")} 
#' \item To see all of the available package documentation run: \code{help(package = 'simcausal')}
#' \item To see the latest updates to this version of the package run: \code{news(package = "simcausal")}
#' }
#' @section Details:
#' The most important functions in \pkg{simcausal} are:
#' \itemize{
#' \item \code{\link{node}} - Specify node or several time-varying nodes at once using a flexible language of vector-like R expressions.
#' \item \code{\link{set.DAG}} - Specify the data-generating distribution of the observed data via DAG (directed acyclic graph).
#' \item \code{\link{sim}} or \code{\link{simobs}} - Simulate observations from the observed data specified by DAG.
#' \item \code{\link{action}} or \code{\link{add.action}} - Specify action (intervention) formulas for particular nodes in the DAG.
#' \item \code{\link{sim}} or \code{\link{simfull}} - Simulate data based on the intervened DAGs produced by \code{setActions} (Full Data).
#' \item \code{\link{set.targetE}} - Define the target causal parameter as expectations of the \code{DAG} object nodes for previously defined \code{actions}.
#' \item \code{\link{set.targetMSM}} - Define the target causal parameter as coeficients of the working marginal structural model (MSM) (set.targetMSM) for the previously defined \code{actions}.
#' \item \code{\link{eval.target}} - Evaluate the true value of the previously defined target parameter.
#' }
#' 
#' For additional details and examples please see the package vignette and the function-specific documentation.
#'
#' @section Updates:
#' Check for updates and report bugs at \url{http://github.com/osofr/simcausal}.
#'
#' @docType package
#' @name simcausal
#'
NULL








