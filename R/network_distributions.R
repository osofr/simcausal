#' Call \code{igraph::sample_gnp} to generate random graph object according to the G(n,p) Erdos-Renyi model
#'
#' Call \code{igraph::sample_gnp} and convert the output to \code{simcausal} network matrix.
#' @param n Size of the network graph (number of nodes).
#' @param p Same as \code{igraph::sample_gnp}: The probability for drawing an edge between two arbitrary vertices (G(n,p) graph).
#' @return A matrix with n rows, each row lists the indices of friends connected to that particular observation.
#' @seealso \code{\link{rnet.gnm}}
#' @export
rnet.gnp <- function(n, p) {
  igraph.gnm <- igraph::sample_gnp(n = n, p = p, directed = TRUE)
  sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(igraph.gnm)
  NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
  return(NetInd_out$NetInd_k)
}

#' Call \code{igraph::sample_gnm} to generate random graph object according to the G(n,m) Erdos-Renyi model
#'
#' Call \code{igraph::sample_gnm} and convert the output to \code{simcausal} network matrix.
#' The parameter \code{m} of \code{igraph::sample_gnm} is derived from \code{n} and \code{m_pn} as \code{as.integer(m_pn*n)}
#' @param n Size of the network graph (number of nodes).
#' @param m_pn The total number of edges as a fraction of the sample size \code{n}.
#' @return A matrix with n rows, each row lists the indices of friends connected to that particular observation.
#' @seealso \code{\link{rnet.gnp}}
#' @export
rnet.gnm <- function(n, m_pn) {
  m <- as.integer(m_pn*n)
  if (n <= 10) m <- 20
  igraph.gnm <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
  sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(igraph.gnm)
  NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
  return(NetInd_out$NetInd_k)
}

#' List All Custom Network Generator Functions in \code{simcausal}.
#'
#' @export
net.list <- function() {
  message("All custom network generators defined in SimCausal:\n")
  print(ls("package:simcausal", pattern="^(rnet)"))
  invisible(ls("package:simcausal"))
}