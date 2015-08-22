#' @importFrom assertthat is.string

is.integerish <- function (x) is.integer(x) || (is.numeric(x) && all(x == as.integer(x)))

#' Define a Network Generator
#'
#' Define a network generator by providing a function (using the argument \code{netfun}) which will simulate a network of connected friends for observations \code{i} in \code{1:n}.
#' This network then serves as a backbone for defining and simulating from the structural equation models for dependent data. 
#' In particular, the network allows new nodes to be defined as functions of the previously simulated node values of \code{i}'s friends, across all observations \code{i}.
#' Let \code{F_i} denote the set of friends of one observation \code{i} (observations in \code{F_i} are assumed to be "connected" to \code{i}) and
#' refer to the union of these sets \code{F_i} as a "network" on \code{n} observations, denoted by \code{F}.
#' A user-supplied network generating function \code{netfun} should be able to simulate such network \code{F} by returning a matrix of \code{n} rows,
#' where each row \code{i} defines a friend set \code{F_i}, i.e., row \code{i} should be a vector of observations in \code{1:n} that are connected to \code{i} (friends of \code{i}),
#' with the remainder filled by \code{NA}s.
#' Each friend set \code{F_i} can contain up to \code{Kmax} unique indices \code{j} from \code{1:n}, except for \code{i} itself. 
#' \code{F_i} is also allowed to be empty (row \code{i} has only \code{NA}s), implying that \code{i} has no friends. 
#' The functionality is illustrated in the examples below. For additional information see Details. 
#' To learn how to use the \code{node} function for defining a node as a function of the friend node values, see Syntax and Network Summary Measures.
#' 
#' Without the network of friends, the \code{DAG} objects constructed by calling the \code{node} function can only specify structural equation models for independent and identically distributed data.
#' That is, if no network is specified, for each observation \code{i} a node can be defined conditionally only on \code{i}'s own previously simulated node values.
#' As a result, any two observations simulated under such data-generating model are always independent and identically distributed.
#' Defining a network \code{F} allows one to define a new structural equation model where a node for each observation \code{i} can depend
#' on its own simulated past, but also on the previously simulated node values of \code{i}'s friends (\code{F_i}).
#' This is accomplished by allowing the data generating distribution for each observation \code{i}'s node to be defined conditionally
#' on the past node values of \code{i}'s friends (observations in \code{F_i}).
#' The network of friends can be used in subsequent calls to \code{node} function where new nodes (random variables) defined by the \code{node} function can depend on the node values of \code{i}'s friends
#' (observations in the set \code{F_i}). During simulation it is assumed observations on \code{F_i} can simultaneously influence \code{i}.
#' 
#' Note that the current version of the package does not allow combining time-varying node indexing \code{Var[t]} and network node indexing \code{Var[[net_indx]]}
#' for the same data generating distribution.
#'
#' Each argument for the input network can be an evaluable R expression. All formulas are captured by delayed evaluation and are evaluated during the simulation.
#' Formulas can refer to standard or user-specified R functions that must only apply to the values of previously defined nodes
#' (i.e. node(s) that were called prior to \code{network()} function call).
#'
#' To force the immediate evaluation of any variable inside these expressions wrap the variable with \code{.()} function, see Example 2 for \code{.(t_end)} in \code{\link{node}}.
#' 
#' @section Syntax:
#' The \code{network} function call that defines the network of friends can be added to a growing \code{DAG} object by using \code{'+'} syntax, much like a new \code{node} is added to a \code{DAG}. 
#' Subsequently defined nodes (\code{node} function calls) can employ the double square bracket subsetting syntax to reference previously simulated node values
#' for specific friends in \code{F_i} simultaneusly across all observations \code{i}.
#' For example, \code{VarName[[net_indx]]} can be used inside the \code{node} formula to reference the node \code{VarName} values of \code{i}'s friends in \code{F_i[net_indx]}, 
#' simultaneously across all \code{i} in \code{1:n}.
#' 
#' The friend subsetting index \code{net_indx} can be any non-negative integer vector that takes values from 0 to \code{Kmax},
#' where 0 refers to the \code{VarName} node values of observation \code{i} itself (this is equivalent to just using \code{VarnName} in the \code{node} formula),
#' \code{net_indx} value of 1 refers to node \code{VarName} values for observations in \code{F_i[1]}, across all \code{i} in \code{1:n}
#' (that is, the value of \code{VarName} of \code{i}'s first friend \code{F_i[1]}, if the friend exists and \code{NA} otherwise),
#' and so on, up to \code{net_indx} value of \code{Kmax}, which would reference to the last friend node values of \code{VarName}, as defined by observations in \code{F_i[Kmax]} across all \code{i}.
#' Note that \code{net_indx} can be a vector (e.g, \code{net_indx=c(1:Kmax)}),
#' in which case the result of the query \code{VarName[[c(1:Kmax)]]} is a matrix of \code{Kmax} columns and \code{n} rows.
#'
#' By default, \code{VarName[[j]]} evaluates to missing (\code{NA}) when observation \code{i} does not have a friend under \code{F_i[j]} (i.e., in the \code{j}th spot of \code{i}'s friend set).
#' This default behavior however can be changed to return 0 instead of \code{NA}, by passing an additional argument \code{replaceNAw0 = TRUE} to the corresponding \code{node} function.
#'
#' @section Network Summary Measures:
#' One can also define summary measures of the network covariates by specifying a node formula that applies an R function to the result of \code{VarName[[net_indx]]}.
#' The rules for defining and applying such summary measures are identical to the rules for defining summary measures for time-varying nodes VarName[t_indx].
#' For example, use \code{sum(VarName[[net_indx]])} to define a summary measure as a sum of \code{VarName} values of friends in \code{F_i[net_indx]}, across all observations \code{i} in \code{1:n}.
#' Similarly, use \code{mean(VarName[[net_indx]])} to define a summary measure as a mean of \code{VarName} values of friends in \code{F_i[net_indx]}, across all \code{i}.
#' For more details on defining such summary functions see the \code{simcausal} vignette.
#'
#' @param name Character name for the network, to be used in future versions
#' @param Kmax Either an R expression that evalutes to an integer constant or an integer specifying the maximum number of friends (connections) any simulated observation can have.
#' @param netfun Character name of the user-defined network generating function, can be any R function that returns a matrix of friend IDs of dimension \code{c(n, Kmax)}. 
#' The function must accept a named argument \code{n} that specifies the total sample size of the network.
#' The matrix of network IDs should have \code{n} rows and \code{Kmax} columns, where each row \code{i} contains a vector of unique IDs in \code{1:n} that are \code{i}'s friends
#' (observations that can influence \code{i}'s node distribution), except for \code{i} itself.
#' Arguments to \code{netfun} can be either passed as named arguments to \code{network} function itself or as a named list of parameters \code{params}.
#' These network arguments can themselves be functions of the previously defined node names,
#' allowing for network sampling itself to be dependent on the previously simulated node values, as shown in Example 2.
#' @param ... Named arguments specifying distribution parameters that are accepted by the network sampling function in \code{netfun}. 
#' These parameters can be R expressions that are themselves formulas of the past node names.
#' @param params A list of additional named parameters to be passed on to the \code{netfun} function. 
#' The parameters have to be either constants or character strings of R expressions of the past node names.
#' @return A list containing the network object(s) of type \code{DAG.net}, this will be utilized when data is simulated with \code{sim} function.
#' @example tests/RUnit/example.simnets.R
# @family network functions
#' @seealso \code{\link{igraph.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.NetInd}}; \code{\link{NetInd.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.igraph}}
#' @export
network <- function(name, Kmax, netfun, ..., params = list()) {
  env <- parent.frame()
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
  assert_that(is.count(Kmax))
  dist_params$Kmax <- Kmax

  # check the distribution function exists, if not found also check the calling environment:
  if (!exists(netfun)) {
    # message("network generator exists(netfun, envir = env): " %+% exists(netfun, envir = env))
    if (!exists(netfun, envir = env)) {
      stop(netfun %+% ": this network generator function could not be located")
    }
  }

  net_dist_params <- list(name = name, Kmax = Kmax, netfun = netfun, dist_params = dist_params, node.env = env)
  net_lists <- list(net_dist_params)
  names(net_lists) <- name

  net_lists <- lapply(net_lists, function(node_i) {class(node_i) <- "DAG.net"; node_i})
  class(net_lists) <- "DAG.netlist"
  net_lists
}

# ------------------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------------------
#' Convert igraph Network Object into Sparse Adjacency Matrix
#' 
#' Convert igraph network object into its sparse adjacency matrix representation using \code{as_adjacency_matrix} function from the \code{igraph} package.
#' @param igraph_network Network as an \code{igraph} object
#' @return Sparase adjacency matrix returned by \code{igraph::as_adjacency_matrix} function. 
#' NOTE: for directed graphs the friend IDs pointing into vertex \code{i} are assumed to be listed in the column \code{i} 
#' (i.e, \code{which(adjmat[,i])} are friends of \code{i}).
# @family network functions
#' @seealso \code{\link{network}}; \code{\link{sparseAdjMat.to.NetInd}}; \code{\link{NetInd.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.igraph}};
#' @export
igraph.to.sparseAdjMat <- function(igraph_network) {
  return(igraph::as_adjacency_matrix(igraph_network, sparse = TRUE, edges = FALSE))
}

#' Convert Network from Sparse Adjacency Matrix into Network IDs Matrix
#'
#' Convert network represented by a sparse adjacency matrix into \code{simcausal} network IDs matrix (\code{NetInd_k}).
#' @param sparseAdjMat Network represented as a sparse adjacency matrix (S4 class object \code{dgCMatrix} from package \code{Matrix}).
#' NOTE: The friends (row numbers) of observation \code{i} are assumed to be listed in column \code{i}
#' (i.e, \code{which(sparseAdjMat[,i])} are friends of \code{i}).
#' @return A named list with 3 items: 1) \code{NetInd_k}; 2) \code{nF}; and 3) \code{Kmax}.
#' 1) \code{NetInd_k} - matrix of network IDs of dimension \code{(n=nrow(sparseAdjMat),Kmax)}, where each row \code{i} consists of the network IDs (friends) for observation \code{i}. 
#' Remainders are filled with NAs.
#' 2) \code{nF} - integer vector of length \code{n} specifying the number of friends for each observation.
#' 3) \code{Kmax} - integer constant specifying the maximum observed number of friends in input \code{sparseAdjMat} (this is the column dimension for the output matrix \code{NetInd_k}).
#' 
# @family network functions
#' @seealso \code{\link{network}}; \code{\link{NetInd.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.igraph}}; \code{\link{igraph.to.sparseAdjMat}};
#' @export
sparseAdjMat.to.NetInd <- function(sparseAdjMat) {
  assertthat::assert_that("dgCMatrix" %in% class(sparseAdjMat))
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

#' Convert Network IDs Matrix into Sparse Adjacency Matrix
#'
#' Convert \code{simcausal} network ID matrix (\code{NetInd_k}) into a network represented by a sparse adjacency matrix.
#' @param NetInd_k Matrix of network IDs of dimension \code{(n=nrow(sparseAdjMat),Kmax)}, 
#' where each row \code{i} consists of the network IDs (row number of friends) of observation \code{i}. Remainders are filled with \code{NA}s.
#' @param nF Integer vector of length \code{n} specifying the number of friends for each observation.
#' @return Network represented as a sparse adjacency matrix (S4 class object \code{dgCMatrix} from package \code{Matrix}).
#' NOTE: The friend IDs for observation \code{i} will be listed in column \code{i}
#' (i.e, \code{which(sparseAdjMat[,i])} are friends of \code{i}).
# @family network functions
#' @seealso \code{\link{network}}; \code{\link{sparseAdjMat.to.igraph}}; \code{\link{igraph.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.NetInd}};
#' @export
NetInd.to.sparseAdjMat <- function(NetInd_k, nF) {
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

#' Convert Network from Sparse Adjacency Matrix into igraph Object
#'
#' Uses \code{graph_from_adjacency_matrix} function from the \code{igraph} package to convert the network in sparse adjacency matrix format into \code{igraph} network object.
#' @param sparseAdjMat Network represented as a sparse adjacency matrix (S4 class object \code{dgCMatrix} from package \code{Matrix}).
#' NOTE: for directed graphs the friend IDs pointing into vertex \code{i} are assumed to be listed in the column \code{i} 
#' (i.e, \code{which(sparseAdjMat[,i])} are friends of \code{i}).
#' @param mode Character scalar, passed on to \code{igraph::graph_from_adjacency_matrix}, specifies how igraph should interpret the supplied matrix. 
#' See \code{?igraph::graph_from_adjacency_matrix} for details.
#' @return A list containing the network object(s) of type \code{DAG.net}.
# @family network functions
#' @seealso \code{\link{network}}; \code{\link{igraph.to.sparseAdjMat}}; \code{\link{sparseAdjMat.to.NetInd}}; \code{\link{NetInd.to.sparseAdjMat}};
#' @export
sparseAdjMat.to.igraph <- function(sparseAdjMat, mode = "directed") {
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
#' R6 class for creating and storing a friend matrix (network IDs) for simulating network data
#'
#' This R6 class defines fields and methods for creating and storing \code{NetInd_k}, 
#' a matrix of friend indices (network IDs) of \code{dim = (nobs x Kmax)}.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords R6 class
#' @details
#' \itemize{
#' \item{NetInd_k} - Matrix of friend indices (network IDs) of \code{dim = (nobs x Kmax)}.
#' \item{nF} - Vector of integers, where \code{nF[i]} is the integer number of friends (0 to \code{Kmax}) for observation \code{i}.
#' \item{nobs} - Number of observations
#' \item{Kmax} - Maximum number of friends for any observation.
#' }
#' @section Methods:
#' \describe{
#'   \item{\code{new(nobs, Kmax = 1)}}{Uses \code{nobs} and \code{Kmax} to instantiate an object of R6 class and pre-allocate memory 
#'          for the future network ID matrix.}
#'   \item{\code{makeNetInd.fromIDs(Net_str, IDs_str = NULL, sep = ' ')}}{Build the matrix of network IDs (\code{NetInd_k}) from IDs string vector,
#'          all friends of one observation \code{i} are located in a string Net_str[i], with two distinct friend IDs of \code{i} 
#'          separated by character \code{sep}. If \code{IDs_str} is NULL it is assumed that the friends in Net_str are 
#'          actual row numbers in \code{1:nobs}, otherwise IDs from Net_str will be used for looking up the observation row numbers in \code{IDs_str}.}
#'   \item{\code{make.nF(NetInd_k = self$NetInd_k, nobs = self$nobs, Kmax = self$Kmax)}}{This method calculates the integer number of 
#'         friends for each row of the network ID matrix (\code{self$NetInd_k}). The result is assigned to a field \code{self$nF} and 
#'         is returned invisibly.}
#'   \item{\code{mat.nF(nFnode)}}{\code{nFnode} - the character name for the number of friends variable that is assigned as a column 
#'   name to a single column matrix in \code{self$nF}.}
#' }
#' @export
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
      assert_that(is.integerish(Kmax))
      self$Kmax <- as.integer(Kmax)
      self$nF <- rep.int(0L, nobs)
      self$NetInd_k <- matrix(NA_integer_, nrow = nobs, ncol = Kmax)
      invisible(self)
    },

    #------------------------------------------------------------------------------
    # Network matrix of columns of friends indices (NetInd_k) from IDs strings for network
    # Net_str - a string vector of friend IDs (rows in obs data)
    # IDs_str - a string vector of observation IDs that identify observation row numbers from Net_str
    # sep - character symbol separating two friend IDs in data[i, NETIDnode] for observation i
    makeNetInd.fromIDs = function(Net_str, IDs_str = NULL, sep = ' ') {
      # Turn string of IDs into a vector, trim extra spaces on both edges
      splitstr_tovec <- function(Net_str_i) stringr::str_trim(unlist(strsplit(Net_str_i, sep, fixed = TRUE)), side = "both")
      # Turn a vector of character IDs into integer row numbers
      getRowsfromIDs <- function(NetIDvec, IDs_str) as.integer(sapply(NetIDvec, function(x) which(IDs_str %in% x)))
      # Turn any vector of IDs into a vector of length Kmax, filling remainder with trailing NA's
      makeKmaxIDvec <- function(NetIDVec) c(as.integer(NetIDVec), rep_len(NA_integer_, self$Kmax - length(NetIDVec)))
      NetIDs_l <- lapply(Net_str, splitstr_tovec) # Get list of n NET ID (character) vectors from Net_str
      NetRows_l <- NetIDs_l
      # if IDnode was provided, get the network row #s from IDs:
      if (!is.null(IDs_str)) NetRows_l <- lapply(NetIDs_l, getRowsfromIDs, IDs_str)
      # Make an array (n x Kmax) of network rows (filling remainder of each row with NA's)
      self$NetInd_k <- as.matrix(vapply(NetRows_l, makeKmaxIDvec, FUN.VALUE = rep.int(0L, self$Kmax), USE.NAMES = FALSE))
      if (self$Kmax > 1L) self$NetInd_k <- t(self$NetInd_k) # for Kmax > 1 need to transpose since the output mat will have dims (Kmax x nrow(data))
      self$make.nF()
      invisible(list(nF = self$nF, NetInd_k = self$NetInd_k)) # invisible(self)
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