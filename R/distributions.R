
#' Bernoulli Node Distribution
#'
#' Wrapper for Bernoulli node distribution.
#'
#' @param n Sample size.
#' @param prob A vector of success probabilities.
#' @return Binary vector of length \code{n}.
#' @examples
#'
#'#---------------------------------------------------------------------------------------
#'# Specifying and simulating from a DAG with 3 Bernoulli nodes
#'#---------------------------------------------------------------------------------------
#'D <- DAG.empty()
#'D <- D + node("W1", distr="rbern", prob=0.05)
#'D <- D + node("W2", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
#'D <- D + node("W3", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
#'Dset <- set.DAG(D)
#'simdat <- sim(Dset, n=200, rndseed=1)
#' @export
rbern <- function(n, prob) {
	rbinom(n=n, prob=prob, size=1)
}

#' Constant (Degenerate) Node Distribution
#'
#' Wrapper for constant value (degenerate) distribution.
#'
#' @param n Sample size.
#' @param const Either a vector with one constant value (replicated \code{n} times)
#' or a vector of length \code{n} or a matrix with \code{n} rows (for a multivariate node).
#' @return A vector of constants of length \code{n}.
#' @examples
#'
#'#---------------------------------------------------------------------------------------
#'# Specifying and simulating from a DAG with 1 Bernoulli and 2 constant nodes
#'#---------------------------------------------------------------------------------------
#'D <- DAG.empty()
#'D <- D + node("W1", distr="rbern", prob=0.05)
#'D <- D + node("W2", distr="rconst", const=1)
#'D <- D + node("W3", distr="rconst", const=ifelse(W1==1,5,10))
#'create_mat <- function(W1, W2, W3) cbind(W1,W2,W3)
#'# Two identical ways of creating a multivariate node (just repeating W1, W2, W3):
#'D <- D + node(c("W4.1", "W4.2", "W4.3"), distr="rconst", const=cbind(W1, W2, W3))
#'D <- D + node(c("cW4.1", "cW4.2", "cW4.3"), distr="rconst", const=create_mat(W1, W2, W3))
#'Dset <- set.DAG(D)
#'simdat <- sim(Dset, n=200, rndseed=1)
#' @export
rconst <- function(n, const) {
	if (n==0) {
		return(vector(length = n))
	} else if (is.matrix(const)) {
		return(const)
	} else if (length(const) == 1) {
		return(rep.int(const, n))
	} else if (length(const) < n) {
		stop("the length of const arg is not 1 and is less than n; it needs to be one or the other")
	} else if (length(const) > n) {
		warning("the length of const arg is more than n; const was truncated to length n")
		return(const[1:n])
	} else {
		return(const)
	}
}

#' Categorical Node Distribution (Factor)
#'
#' Matrix version of the categorical distribution. The argument \code{probs} can be a matrix of n rows, specifying individual (varying in sample) categorical probabilities. The number of categories generated is equal to \code{ncol(probs)+1}, the categories are labeled as: \code{1,...,ncol(probs)+1}.
#'
#' @param n Sample size.
#' @param probs Either a vector or a matrix of success probabilities. When \code{probs} is a vector, \code{n} identically distributed random categorical variables are generated with categories: 1, 2, ..., length(probs)+1. When \code{probs} is a matrix, the categorical probabilities of the \code{k}th sample are determined by the \code{k}th row of \code{probs} matrix, i.e., \code{probs[k,]}.
#' @return A factor of length \code{n} with levels: \code{1,2, ...,ncol(probs)+1}.
#' @examples
#'
#'#---------------------------------------------------------------------------------------
#'# Specifying and simulating from a DAG with one categorical node with constant
#'# probabilities
#'#---------------------------------------------------------------------------------------
#'D <- DAG.empty()
#'D <- D + node("race",t=0,distr="rcategor",probs=c(0.2,0.1,0.4,0.15,0.05,0.1))
#'Dset <- set.DAG(D)
#'simdat <- sim(Dset, n=200, rndseed=1)
#'
#'#---------------------------------------------------------------------------------------
#'# Specifying and simulating from a DAG with a categorical node with varying
#'# probabilities (probabilities are determined by values sampled for nodes L0 and L1)
#'#---------------------------------------------------------------------------------------
#'D <- DAG.empty()
#'D <- D + node("L0", distr="rnorm", mean=10, sd=5)
#'D <- D + node("L1", distr="rnorm", mean=10, sd=5)
#'D <- D + node("L2", distr="rcategor", probs=c(abs(1/L0), abs(1/L1)))
#'Dset <- set.DAG(D)
#'simdat <- sim(Dset, n=200, rndseed=1)
#' @export
rcategor <- function(n, probs) {
	# if (is.vector(probs)) {
	# 	probs <- matrix(data = probs, nrow = n, ncol = length(probs), byrow = TRUE)
	# }
	# probs <- cbind(probs, 1 - rowSums(probs))	# sum each row and check if some need to be normalized
	# pover1_idx <- which(probs[,ncol(probs)] < -(10^-6)) # which rows of probs need to be normalized
	# # pover1_idx <- which(probs[,ncol(probs)] < 0) # which rows of probs need to be normalized

	# # reset last category to zero if its with numeric epsilon error away from 0:
	# restto0 <- which(abs(probs[,ncol(probs)]) <= 10^-6 & abs(probs[,ncol(probs)]) > 0L)
	# if (length(restto0)>0) {
	# 	probs[restto0, ncol(probs)] <- 0L
	# }

	# if (length(pover1_idx)>0) {
	# 	warning("some categorical probabilities add up to more than 1, normalizing to add to 1")
	# 	probs[pover1_idx, ncol(probs)] <- 0
	# 	probs[pover1_idx, ] <- probs[pover1_idx, ,drop = FALSE] / rowSums(probs[pover1_idx, ,drop = FALSE]) # normalize
	# }
	# probs_cum <- matrix(nrow=nrow(probs), ncol=ncol(probs))
	# probs_cum[,1] <- probs[,1]
	# for (i in seq(ncol(probs))[-1]) {
	# 	probs_cum[,i] <- rowSums(probs[,c(1:i)])
	# }
	# samples <- rowSums(probs_cum - runif(nrow(probs_cum)) < 0) + 1
	# as.factor(samples)
	as.factor(rcategor.int(n = n, probs = probs))
}

#' Categorical Node Distribution (Integer)
#'
#' Same as \code{rcategor}, but returning a vector of sampled integers 1, 2, ..., \code{ncol(probs)+1} instead of a factor.
#' @param n Sample size.
#' @param probs Either a vector or a matrix of success probabilities. When probs is a vector, \code{n} identically distributed random categorical variables are generated with categories: 1, 2, ..., \code{length(probs)+1}. When \code{probs} is a matrix, the categorical probabilities of the \code{k}th sample are determined by the \code{k}th row of probs matrix, i.e., \code{probs[k,]}.
#' @return An integer vector of length \code{n} with values: \code{1,2, ...,ncol(probs)+1}.
#' @export
rcategor.int <- function(n, probs) {
	if (n==0) {
		probs <- matrix(nrow = n, ncol = length(probs), byrow = TRUE)
	}
	if (is.vector(probs) && n>0) {
		probs <- matrix(data = probs, nrow = n, ncol = length(probs), byrow = TRUE)
	}
	probs <- cbind(probs, 1 - rowSums(probs))	# sum each row and check if some need to be normalized
	pover1_idx <- which(probs[,ncol(probs)] < -(10^-6)) # which rows of probs need to be normalized
	# reset last category to zero if its with numeric epsilon error away from 0:
	restto0 <- which(abs(probs[,ncol(probs)]) <= 10^-6 & abs(probs[,ncol(probs)]) > 0L)
	if (length(restto0)>0) {
		probs[restto0, ncol(probs)] <- 0L
	}
	if (length(pover1_idx)>0) {
		warning("some categorical probabilities add up to more than 1, normalizing to add to 1")
		probs[pover1_idx, ncol(probs)] <- 0
		probs[pover1_idx, ] <- probs[pover1_idx, ,drop = FALSE] / rowSums(probs[pover1_idx, ,drop = FALSE]) # normalize
	}
	probs_cum <- matrix(nrow = nrow(probs), ncol = ncol(probs))
	probs_cum[,1] <- probs[,1]
	for (i in seq(ncol(probs))[-1]) {
		probs_cum[,i] <- rowSums(probs[,c(1:i),drop = FALSE])
	}
	cat_sample <- probs_cum - runif(nrow(probs_cum)) < 0
	if (is.matrix(cat_sample)) {
		samples <- rowSums(cat_sample) + 1
	} else {
		samples <- cat_sample
	}
	as.integer(samples)
}

#' List All Custom Distribution Functions in \code{simcausal}.
#'
#' @export
distr.list <- function() {
	message("All custom distributions defined in SimCausal:\n")
	print(ls("package:simcausal", pattern="^[r]"))
	# print(ls("package:simcausal", pattern="^[S]L"))
	invisible(ls("package:simcausal"))
}

#' Template for Writing Custom Distribution Functions
#'
#' Template function for writing \code{SimCausal} custom distribution wrappers.
#'
#' One of the named arguments must be 'n', this argument is passed on to the function automatically by the package and is assigned to the number of samples that needs to be generated from this distribution.
#' Other arguments (in this example arg1 and arg2) must be declared by the user as arguments inside the node() function that uses this distribution,
#' e.g., \code{node("Node1"}, \code{distr="distr.template"}, \code{arg1 = ...}, \code{arg2 = ...)}.
#' Both, arg1 and arg2, can be either numeric constants or formulas involving past node names. The constants get passed on to the distribution function unchanged. The formulas are evaluated inside the environment of the simulated data and are passed on to the distribution functions as vectors.
#' The output of the distribution function is expected to be a vector of length n of the sampled covariates.
#'
#' @param n Sample size that needs to be generated
#' @param arg1 Argument 2
#' @param arg2 Argument 1
#' @return A vector of length \code{n}
#' @param ... Additional optional parameters
#' @export
rdistr.template <- function(n, arg1, arg2, ...) {
	if (length(arg1) == 1L) arg1 <- rep.int(arg1, n)
 	if (length(arg2) == 1L) arg2 <- rep.int(arg2, n)
	if (length(arg1) != n || length(arg2) != n) stop("inputs arguments should all have the same length")
	out <- as.numeric(arg1 + arg2)
	stopifnot(length(out)==n)
	# length(out) <- n
	out
}