###################################################################

###################################################################
# Node and Action Constructors
# Indexing/Subsetting DAG and actions will be here
###################################################################

#' Subsetting/indexing DAG nodes
#' @param DAG A DAG object that was defined using functions \code{\link{node}} and \code{\link{set.DAG}}
#' @return returns a list of nodes that can be indexed as a typical named list "[[]]"
#' @examples
#' 
#'D <- DAG.empty()
#'D <- D + node(name="W1", distr="rbern", prob=plogis(-0.5))
#'D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- set.DAG(D)
#' #Returns all nodes from DAG D
#'N(D)
#' #Returns node W1 from DAG D
#'N(D)["W1"]
#' @export
N <- function(DAG) {
  if (!is.DAG(DAG) && !is.DAGnodelist(DAG)) {
    stop("Not a DAG object")
  }
  nodecount <- length(DAG)
  res <- seq_len(nodecount)
  class(res) <- "DAG.nodelist"
  ne <- new.env()
  assign("DAG", DAG, envir=ne)
  attr(res, "env") <- ne
  # the idea is to return the environment variable to avoid copying the DAG while subsetting
  # res
  # for now returning just the DAG itself
  attr(res, "env")$DAG
}
# select DAG nodes by t vector attribute
Ntvec <- function(DAG, tvec) {
  node_nms <- sapply(N(DAG), '[[', "name")
  # get actual t for each node and return only nodes that pass
  N_t_idx <- sapply(N(DAG), function(node) is.null(node[["t"]]) || (node[["t"]]%in%tvec))
  N_t <- N(DAG)[N_t_idx]
  class(N_t) <- "DAG.nodelist"
  N_t
}
# return a list of attribute values for a given attr name and list of nodes (DAG)
Nattr <- function(DAG, attr) {
  lapply(N(DAG), '[[', attr)
}

#' Subsetting/indexing actions defined for DAG object
#' @param DAG A DAG object that was defined using functions \code{\link{node}}, \code{\link{set.DAG}} and \code{\link{action}}
#' @return returns a list of actions, which are intervened versions of the original observed data DAG.
#' @examples
#' 
#'D <- DAG.empty()
#'D <- D + node(name="W1", distr="rbern", prob=plogis(-0.5))
#'D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 + 0.5*W1+ 0.5*W2))
#'D <- set.DAG(D)
#' # Define two actions, acting on node "A"
#'D <- D + action("A0", nodes=node("A", distr="rbern", prob=0))
#'D <- D + action("A1", nodes=node("A", distr="rbern", prob=1))
#' # Select both actions
#'A(D)
#' # Select action "A1" only
#'A(D)["A1"]
#' @export
A <- function(DAG) {
  if (!is.DAG(DAG)) {
    stop("Not a DAG object")
  }
  res <- attributes(DAG)$actions
  if (is.null(res)) {
    NULL
  } else {
    # class(res) <- "DAG.action"
    res
  }
}

# # @export
# "[.DAG.nodelist" <- function(x, i) {
#   i <- substitute(i)
#   if (is.numeric(i) || is.integer(i)) {
#     # simple indexing by node ids
#     res <- i[ i %in% x ]
#     attributes(res) <- attributes(x)
#   } else if (is.logical(i)) {
#     # simple indexing by logical vector
#     res <- as.numeric(x) [ i ]
#     attributes(res) <- attributes(x)
#   } else if (is.character(i)) {
#     res <- as.DAG.nodes(get("DAG", attr(x, "env")), i)
#     attributes(res) <- attributes(x)
#   } else {
#     # language expression, can also be an attribute based indexing
#     DAG <- get("DAG", attr(x, "env"))
#     i <- eval....
#     if (is.numeric(i) || is.integer(i)) {
#       i <- as.numeric(i)
#       res <- i[ i %in% x ]
#       attributes(res) <- attributes(x)
#     } else if (is.logical(i)) {
#       res <- as.numeric(x) [ i ]
#       attributes(res) <- attributes(x)
#     } else if (is.character(i)) {
#       res <- as.DAG.nodes(get("DAG", attr(x, "env")), i)
#       attributes(res) <- attributes(x)
#     } else {
#       stop("invalid indexing of the node")
#     }
#   }
#   res
# }
# as.DAG.nodes <- function(DAG, node) {
# ....
# }