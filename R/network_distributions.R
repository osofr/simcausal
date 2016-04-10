#' Call \code{igraph::sample_gnp} to Generate Random Graph Object According to the G(n,p) Erdos-Renyi Model
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

#' Call \code{igraph::sample_gnm} to Generate Random Graph Object According to the G(n,m) Erdos-Renyi Model
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

#' Call \code{igraph::sample_smallworld} to Generate Random Graph Object from the Watts-Strogatz Small-World Model
#'
#' Call \code{igraph::sample_smallworld} and convert the output to \code{simcausal} network matrix.
#' The parameters are the same as those of \code{igraph::sample_smallworld}.
#' The loop edges aren't allowed (\code{loops = FALSE}) and the multiple edges aren't allowed either \code{multiple = FALSE}.
#' @param n Size of the network graph (the number of nodes).
#' @param dim Same as in \code{igraph::sample_smallworld}: Integer constant, the dimension of the starting lattice.
#' @param nei Same as in \code{igraph::sample_smallworld}: Integer constant, the neighborhood within which the vertices of the lattice will be connected.
#' @param p Same as in \code{igraph::sample_smallworld}: Real constant between zero and one, the rewiring probability.
#' @return A matrix with n rows, each row lists the indices of friends connected to that particular observation.
#' @seealso \code{\link{rnet.gnp}}, \code{\link{rnet.gnm}}
#' @export
#'
rnet.SmWorld <- function(n, dim, nei, p) {
    g <- igraph::sample_smallworld(dim = dim, size = n, nei = nei, p = p, loops = FALSE, multiple = FALSE)
    g <- igraph::as.directed(g, mode = c("mutual"))
    sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(g)
    NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
    return(NetInd_out[["NetInd_k"]])
}

#' List All Custom Network Generator Functions in \code{simcausal}.
#'
#' @export
net.list <- function() {
  message("All custom network generators defined in SimCausal:\n")
  print(ls("package:simcausal", pattern="^(rnet)"))
  invisible(ls("package:simcausal"))
}