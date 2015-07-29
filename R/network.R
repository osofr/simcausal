# ------------------------------------------------------------------------------------
# CONVERTING THE NETWORK FROM SPARSE ADJACENCY MATRIX FORMAT INTO simcausal/tmlenet INPUT FORMAT (NetInd_k)
# Returns NetInd_k - (n, Kmax) matrix of friend IDs (rows)) listed as columns, fill remainders with NAs
# - add to tmlenet & simcausal
# ------------------------------------------------------------------------------------
igraph_to_sparseAdjMat <- function(igraph_network) {
  # ************************************************************************
  # NOTE: For directed graphs THE FRIEND IDs POINTING INTO VERTEX i ARE IN COLUMN i (i.e, which(adjmat[,i]) are friends of i)
  # ************************************************************************
  sparseAdjMat <- igraph::as_adjacency_matrix(igraph_network, sparse = TRUE, edges = FALSE)
  return(sparseAdjMat)
}

sparseAdjMat_to_NetInd <- function(sparseAdjMat) {
  assertthat::assert_that("dgCMatrix" %in% class(sparseAdjMat))
  # ************************************************************************
  # NOTE: For directed graphs THE FRIEND IDs POINTING INTO VERTEX i ARE IN COLUMN i (i.e, which(adjmat[,i]) are friends of i)
  # ************************************************************************
  # sparseAdjMat:
    # i: These are the 0-based row numbers for each non-zero element in the matrix.
    # Object of class "integer" of length nnzero (number of non-zero elements). These are the 0-
    # based row numbers for each non-zero element in the matrix, i.e., i must be in 0:(nrow(.)-1).
    # p: integer vector for providing pointers, one for each column, to the initial (zero-based) index of elements in the column.
    # .@p is of length ncol(.) + 1, with p[1] == 0 and
    # p[length(p)] == nnzero, such that in fact, diff(.@p) are the number of non-zero elements for each column.

  # 1) The number of friends for each observation:
  nF <- as.integer(diff(sparseAdjMat@p))
  # 2) Column based cummulative number of non-zero entries (cummulative nF)
  cumFindx <- sparseAdjMat@p
  # 3) All non-zero elements as a vector of 0-based row numbers:
  base0_IDrownums <- sparseAdjMat@i
  # 4) Figure out the dim of the result mat NetInd_k and initiate:
  Kmax <- max(nF)
  NetInd_k <- matrix(NA_integer_, nrow = length(nF), ncol = Kmax)
  # 5) For each non-zero elements in nF, populate NetInd_k with friend IDs:
  non0nF.idx <- which(nF > 0L)
  for (idx in non0nF.idx) {
    Fidx_base0 <- cumFindx[idx] : (cumFindx[idx + 1] - 1)
    FriendIDs <- base0_IDrownums[Fidx_base0 + 1] + 1
    NetInd_k[idx, seq_len(nF[idx])] <- FriendIDs
  }
  # Check the total n of non-zero elements is the same as in original sparseAdjMat:
  nnonzero <- sum(!is.na(NetInd_k))
  stopifnot(nnonzero==sparseAdjMat@p[ncol(sparseAdjMat)+1])
  return(list(NetInd_k = NetInd_k, nF = nF, Kmax = Kmax))
}

# ------------------------------------------------------------------------------------
# CONVERTING NetInd_k NETWORK INTO THE SPARSE ADJACENCY MATRIX FORMAT - add to tmlenet & simcausal
# ------------------------------------------------------------------------------------
NetInd_to_sparseAdjMat <- function(NetInd_k, nF) {
  nobs <- nrow(NetInd_k)
  sdims <- c(nobs, nobs)
  nnonzero <- sum(!is.na(NetInd_k))
  sparse_p <- as.integer(c(0, cumsum(nF)))
  sparse_x <- rep.int(1L, nnonzero)
  sparse_iwNA <- as.vector(t(NetInd_k))
  sparse_i <- sparse_iwNA[!is.na(sparse_iwNA)] - 1
  out_sparseAdjMat <-  Matrix::sparseMatrix(i = sparse_i, p = sparse_p, x = sparse_x, dims = sdims, index1 = FALSE)
  return(out_sparseAdjMat)
}
# ------------------------------------------------------------------------------------
# CONVERTING FROM SPARSE ADJACENCY MAT INTO IGRAPH OBJECT - add to tmlenet & simcausal
# ------------------------------------------------------------------------------------
sparseAdjMat_to_igraph <- function(sparseAdjMat, mode = "directed") {
  igraph::graph_from_adjacency_matrix(sparseAdjMat, mode = mode)
}

#-----------------------------------------------------------------------------
# ALL NETWORK VARIABLE NAMES MUST BE CONSTRUCTED BY CALLING THIS FUNCTION.
# In the future might return the network variable (column vector) itself.
# Helper function that for given variable name (varnm) and friend index (fidx) 
# returns the characeter name of that network variable varnm[fidx], 
# for fidx = 0 (var itself), ..., kmax. fidx can be a vector, in which case a 
# character vector of network names is returned. If varnm is also a vector, a 
# character vector for all possible combinations of (varnm x fidx) is returned.
#-----------------------------------------------------------------------------
# OUTPUT format: Varnm_net.j:
netvar <- function(varnm, fidx) {
  cstr <- function(varnm, fidx) {
    slen <- length(fidx)
    rstr <- vector(mode = "character", length = slen)
    netidxstr <- ! (fidx %in% 0L)
    rstr[netidxstr] <- stringr::str_c('_netF', fidx[netidxstr])  # vs. 1
    # rstr[netidxstr] <- str_c('.net.', fidx[netidxstr])  # vs. 2
    return(stringr::str_c(varnm, rstr))
  }
  if (length(varnm) > 1) {
    return(unlist(lapply(varnm, cstr, fidx)))
  } else {
    return(cstr(varnm, fidx))
  }
}
# Examples:
# netvar("A", (0:5))
# netvar("A", c(0:5, 0, 3))
# netvar(c("A", "W"), c(0:5, 0, 3))
# netvar(c("A", "W"), c(0:5, 0, 3))

## ---------------------------------------------------------------------
# Class holds and creates NetInd_k, the matrix of network connection indices in Odata of dim = (nobs x Kmax)
# Also calculates a vector nF - number of friends for each unit
# When class = FALSE, a pointer "self" is still created, but parent.env(self) is the enclosing environment
## ---------------------------------------------------------------------
NetIndClass <- R6Class("NetIndClass",
  class = TRUE,
  portable = TRUE,
  public = list(
    NetInd_k = matrix(),       # matrix (n x Kmax) of network (friend) indices (rows) in observed data
    nF = integer(),            # number of friends, integer vector of length n
    nobs = NA_integer_,        # n observations
    Kmax = NA_integer_,        # max number of friends

    initialize = function(nobs, Kmax = 1) {
      self$nobs <- nobs
      assert_that(is.count(Kmax))
      self$Kmax <- Kmax
      self$nF <- rep.int(0L, nobs)
      self$NetInd_k <- matrix(NA_integer_, nrow = nobs, ncol = Kmax)
      invisible(self)
    },

    #------------------------------------------------------------------------------
    # Netwk matrix of columns of friend indices (NetInd_k) from ids strings for network
    # Net_str - a string vector of friend IDs (rows in obs data)
    # Kmax - max number of friends
    # sep - character symbol separating two friend IDs in data[i, NETIDnode] for observation i
    makeNetInd.fromIDs = function(Net_str, NETIDnode, sep = ' ') {
      # Turn string of IDs into a vector, trim extra spaces on both edges
      splitstr_tovec <- function(Net_str_i) stringr::str_trim(unlist(strsplit(Net_str_i, sep, fixed=TRUE)), side = "both")
      # Turn a vector of character IDs into integer row numbers
      getRowsfromIDs <- function(NetIDvec) as.integer(sapply(NetIDvec, function(x) which(IDs %in% x)))
      # Turn any vector of IDs into a vector of length Kmax, filling remainder with trailing NA's
      makeKmaxIDvec <- function(NetIDVec) c(as.integer(NetIDVec), rep_len(NA_integer_, self$Kmax - length(NetIDVec)))
      # Net_str <- as.character(data[,NETIDnode])
      NetRows_l <- lapply(Net_str, splitstr_tovec) # Get list of n NET ID (character) vectors from NETIDnode
      # Make an array (n x Kmax) of network rows (filling remainder of each row with NA's)
      self$NetInd_k <- as.matrix(vapply(NetRows_l, makeKmaxIDvec, FUN.VALUE = rep.int(0L, self$Kmax), USE.NAMES = FALSE))
      if (self$Kmax > 1L) self$NetInd_k <- t(self$NetInd_k) # for Kmax > 1 need to transpose since the output mat will have dims (Kmax x nrow(data))
      self$make.nF()
      invisible(list(nF = self$nF, NetInd_k = self$NetInd_k)) # invisible(self)
      # return(list(nF = nF, NetInd_k = NetInd_k)) # invisible(self)
    },

    make.nF = function(NetInd_k = self$NetInd_k, nobs = self$nobs, Kmax = self$Kmax) {
      self$nF <- as.integer(.rowSums(! is.na(NetInd_k), m = nobs, n = Kmax))
      invisible(self$nF)
    },

    mat.nF = function(nFnode) {
      assert_that(is.string(nFnode))
      self$nF <- as.matrix(self$nF)
      colnames(self$nF) <- nFnode # colnames(nF) <- "nF"
      invisible(self$nF)
    }
  ),

  active = list(
    NetInd = function(NetInd_k) {
      if (missing(NetInd_k)) {
        self$NetInd_k
      } else {
        assert_that(is.matrix(NetInd_k))
        assert_that(nrow(NetInd_k) == self$nobs)
        assert_that(ncol(NetInd_k) == self$Kmax)
        self$NetInd_k[, ] <- NetInd_k
      }
    },

    wipeoutNetInd = function() {
      self$NetInd_k[,] <- matrix(NA, nrow = self$nobs, ncol = self$Kmax)
      invisible(self)
    }
  )
)


#' Create Network Object
#'
#' This function provides a convenient way to define a node and its distribution in a time-varying format without unnecessary code repetition. 
#' The node distribution is allowed to vary as a function of time (\code{t}), with subsetting of the past nodes accomplished via \code{NodeName[t]}.
#' Intended for use in conjunction with functions \code{\link{set.DAG}}, a DAG object constructor, and \code{\link{add.action}}, an action (intervention) constructor.
#'
#' The combination of a generic name \code{name} and time point \code{t} must be unique in the sense that no other user-specified input node can result in the same 
#'combination of \code{name} and time point \code{t}. 
#'In other words, the combination of \code{name} and \code{t} must uniquely identify each node in the DAG. 
#'The user should use the same \code{name} to identify measurements of the same attribute (e.g. 'A1c') at various time points.
#'
#' All nodes indexed by the same time point \code{t} value must have consecutive \code{order} values. 
#'The \code{order} values of all nodes indexed by the same \code{t} value must have their \code{order} values: 
#'1) strictly greater than the \code{order} values of all nodes indexed by a strictly lower \code{t} value and 
#'2) strictly lower than the \code{order} values of all nodes indexed by a strictly higher \code{t} value. 
#'All nodes of a DAG must have consecutive \code{order} values starting at one. 
#'The collection of unique \code{t} values of all nodes of a DAG must be consecutive values starting at 0.
#'
#' All node calls that share the same generic name \code{name} must also share the same \code{EFU} value (if any is specified in at least one of them). 
#'A value of \code{TRUE} for the \code{EFU} indicates that if a simulated value for a measurement of the attribute represented by node is 1 
#'then all the following nodes with that measurement (in terms of higher \code{t} values) in the DAG will be unobserved (i.e., their simulated value will be set to NA).
#'
#' Each formula of an input node is an evaluable R expression. All formulas are delayed in the evaluation until the simulation time.
#'Formulas can refer to standard or user-specified R functions that must only apply to the values of parent nodes, 
#'i.e. a subset of the node(s) with an \code{order} value strictly lower than that of the node characterized by the formula. 
#'Formulas must reference the parent nodes with unique \code{name} identifiers, employing the square bracket vector subsetting \code{name[t]} for referencing a 
#'parent node at a particular time point \code{t} (if any time-points were specified). 
#'The square bracket notation is used to index a generic name with the relevant time point as illustrated in the examples. 
#'When an input node is used to define several nodes (i.e., several measurement of the same attribute, \code{t=0:5}), the formula(s) specified in that node can apply 
#'to each node indexed by a given time point denoted by \code{t}. This generic expression \code{t} can then be referenced within a formula to simultaneously identify a 
#'different set of parent nodes for each time point as illustrated below. Note that the parents of each node represented by a given \code{node} object are implicitly defined 
#'by the nodes referenced in formulas of that \code{node} call.
#'
#' Distribution parameters (mean, probs, sd, unifmin and unifmax) are passed down with delayed evaluation, to force immediate evaluation of any variable 
#'inside these expressions wrap the variable with \code{.()} function, see Example 2 for \code{.(t_end)}.
#'
#' @param name Character node name, for time-dependent nodes the names will be automatically expanded to a scheme "name_t" for each t provided specified
#' @param Kmax An R expression that evalutes to a constant specifying the maximum number of friends for any observation.
#' @param netfun Character name of the network generator function, can be any R function that returns a matrix of IDs (friends) of dimension \code{c(n, Kmax)}. The function must accept a named argument "n" to specify the total sample size. Distributional parameters (arguments) must be passed as either named arguments to node or as a named list of parameters "params".
#' @param ... Named arguments specifying distribution parameters that are accepted by the \code{distr} function. The parameters can be R expressions that are themselves formulas of the past node names.
#' @param params A list of additional named parameters to be passed on to the \code{distr} function. The parameters have to be either constants or character strings of R expressions of the past node names.
#' @return A list containing the network object(s) of type \code{DAG.net}.
#' @example tests/RUnit/set.DAG.R
#' @export
# Constructor for node objects, uses standard R distribution functions, 
# added optional attributes that are saved with the node (such as custom distribution functions, etc)
network <- function(name, Kmax, netfun, ..., params = list()) {
  env <- parent.frame()
  # print("ls(env) for parent frame in node: "); print(ls(env))

  if (missing(netfun)) stop("netfun argument must be specified")
  # collect all distribution parameters with delayed evaluation (must be named)
  dist_params <- eval(substitute(alist(...)))
  if (length(dist_params)>0) {
    dist_params <- lapply(dist_params, function(x) deparse(bquote2(x, env)))
  }
  dist_params <- append(dist_params, params)
  parnames <- names(dist_params)
  if (length(dist_params) != 0 && (is.null(parnames) || any(parnames==""))) {
    stop("please specify name for each attribute")
  }

  if (missing(Kmax)) stop("Kmax argument must be specified")
  dist_params$Kmax <- Kmax
  assert_that(is.count(dist_params$Kmax))
  # if (is.null(dist_params$Kmax)) stop("Kmax argument must be specified")
  # dist_params$Kmax <- eval(parse(text = dist_params$Kmax))
  # assert_that(is.count(dist_params$Kmax))
  
  net_dist_params <- list(netfun = netfun, dist_params = dist_params)

  # check the distribution function exists, if not found also check the calling environment:
  if (!exists(netfun)) {
    # message("network generator exists(netfun, envir = env): " %+% exists(netfun, envir = env))
    if (!exists(netfun, envir = env)) {
      stop("network generator function '"%+%netfun%+% "' cannot be found")
    }
  }

  net_lists <- list(c(name = name, Kmax = dist_params$Kmax, net_dist_params))
  names(net_lists) <- name
  net_lists <- lapply(net_lists, function(node_i) {class(node_i) <- "DAG.net"; node_i})
  class(net_lists) <- "DAG.netlist"
  net_lists
}