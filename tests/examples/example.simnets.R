#--------------------------------------------------------------------------------------------------
# EXAMPLE 1. USING igraph R PACKAGE TO SIMULATE NETWORKS
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# Example of a network sampler, will be provided as "netfun" argument to network(, netfun=);
# Generates a random graph according to the G(n,m) Erdos-Renyi model using the igraph package;
# Returns (n,Kmax) matrix of net IDs (friends) by row;
# Row i contains the IDs (row numbers) of i's friends;
# i's friends are assumed connected to i and can influence i in equations defined by node())
# When i has less than Kmax friends, the remaining i row entries are filled with NAs;
# Argument m_pn: > 0
# a total number of edges in the network as a fraction (or multiplier) of n (sample size)
#--------------------------------------------------------------------------------------------------
gen.ER <- function(n, m_pn, ...) {
  m <- as.integer(m_pn*n)
  if (n<=10) m <- 20
  igraph.ER <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
  sparse_AdjMat <- igraph.to.sparseAdjMat(igraph.ER)
  NetInd_out <- sparseAdjMat.to.NetInd(sparse_AdjMat)
  return(NetInd_out$NetInd_k)
}

D <- DAG.empty()
# Sample ER model network using igraph::sample_gnm with m_pn argument:
D <- D + network("ER.net", netfun = "gen.ER", m_pn = 50)
# W1 - categorical (6 categories, 1-6):
D <- D +
  node("W1", distr = "rcategor.int",
        probs = c(0.0494, 0.1823, 0.2806, 0.2680, 0.1651, 0.0546)) +
# W2 - binary infection status, positively correlated with W1:
  node("W2", distr = "rbern", prob = plogis(-0.2 + W1/3)) +
# W3 - binary confounder:
  node("W3", distr = "rbern", prob = 0.6)
# A[i] is a function W1[i] and the total of i's friends values W1, W2 and W3:
D <- D + node("A", distr = "rbern",
              prob = plogis(2 + -0.5 * W1 +
                            -0.1 * sum(W1[[1:Kmax]]) +
                            -0.4 * sum(W2[[1:Kmax]]) +
                            -0.7 * sum(W3[[1:Kmax]])),
              replaceNAw0 = TRUE)
# Y[i] is a function of netW3 (friends of i W3 values) and the total N of i's friends
# who are infected AND untreated:
D <- D + node("Y", distr = "rbern",
              prob = plogis(-1 + 2 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) +
                            -2 * sum(W3[[1:Kmax]])
                            ),
              replaceNAw0 = TRUE)
# Can add N untreated friends to the above outcome Y equation: sum(1 - A[[1:Kmax]]):
D <- D + node("Y", distr = "rbern",
              prob = plogis(-1 + 1.5 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) +
                            -2 * sum(W3[[1:Kmax]]) +
                            0.25 * sum(1 - A[[1:Kmax]])
                            ),
              replaceNAw0 = TRUE)
# Can add N infected friends at baseline to the above outcome Y equation: sum(W2[[1:Kmax]]):
D <- D + node("Y", distr = "rbern",
              prob = plogis(-1 + 1 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) +
                            -2 * sum(W3[[1:Kmax]]) +
                            0.25 * sum(1 - A[[1:Kmax]]) +
                            0.25 * sum(W2[[1:Kmax]])
                            ),
              replaceNAw0 = TRUE)
Dset <- set.DAG(D, n.test = 100)
# Simulating data from the above sem:
datnet <- sim(Dset, n = 1000, rndseed = 543)
head(datnet)
# Obtaining the network object from simulated data:
net_object <- attributes(datnet)$netind_cl
# Max number of friends:
net_object$Kmax
# Network matrix
head(attributes(datnet)$netind_cl$NetInd)

#--------------------------------------------------------------------------------------------------
# EXAMPLE 2. USING CUSTOM NETWORK GENERATING FUNCTION
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# Example of a user-defined network sampler(s) function
# Arguments K, bslVar[i] (W1) & nF are evaluated in the environment of the simulated data then
# passed to genNET() function
  # - K: maximum number of friends for any unit
  # - bslVar[i]: used for contructing weights for the probability of selecting i as
  # someone else's friend (weighted sampling), when missing the sampling goes to uniform
  # - nF[i]: total number of friends that need to be sampled for observation i
#--------------------------------------------------------------------------------------------------
genNET <- function(n, K, bslVar, nF, ...) {
  prob_F <- plogis(-4.5 + 2.5*c(1:K)/2) / sum(plogis(-4.5 + 2.5*c(1:K)/2))
  NetInd_k <- matrix(NA_integer_, nrow = n, ncol = K)
  nFriendTot <- rep(0L, n)
  for (index in (1:n)) {
    FriendSampSet <- setdiff(c(1:n), index)
    nFriendSamp <- max(nF[index] - nFriendTot[index], 0L)
    if (nFriendSamp > 0) {
      if (length(FriendSampSet) == 1)  {
        friends_i <- FriendSampSet
      } else {
        friends_i <- sort(sample(FriendSampSet, size = nFriendSamp,
                          prob = prob_F[bslVar[FriendSampSet] + 1]))
      }
      NetInd_k[index, ] <- c(as.integer(friends_i),
                            rep_len(NA_integer_, K - length(friends_i)))
      nFriendTot[index] <- nFriendTot[index] + nFriendSamp
    }
  }
  return(NetInd_k)
}

rcategor.int.base0 <- function(n, probs) rcategor.int(n, probs)-1
D <- DAG.empty()
D <- D +
# W1 - categorical or continuous confounder (5 categories, 0-4):
  node("W1", distr = "rcategor.int.base0",
        probs = c(0.0494, 0.1823, 0.2806, 0.2680, 0.1651, 0.0546)) +
# W2 - binary infection status at t=0, positively correlated with W1:
  node("W2", distr = "rbern", prob = plogis(-0.2 + W1/3)) +
# W3 - binary confounder:
  node("W3", distr = "rbern", prob = 0.6)

# def.nF: total number of friends for each i (0-K), each def.nF[i] is influenced by categorical W1
K <- 10
set.seed(12345)
normprob <- function(x) x / sum(x)
p_nF_W1_mat <- apply(matrix(runif((K+1)*6), ncol = 6, nrow = (K+1)), 2, normprob)
colnames(p_nF_W1_mat) <- paste0("p_nF_W1_", c(0:5))
create_probs_nF <- function(W1) t(p_nF_W1_mat[,W1+1])
vecfun.add("create_probs_nF")
D <- D + node("def.nF", distr = "rcategor.int.base0", probs = create_probs_nF(W1))

# Adding the network generator that depends on nF and categorical W1:
D <- D + network(name="net.custom", netfun = "genNET", K = K, bslVar = W1, nF = def.nF)
# Define A[i] is a function W1[i] as well as the total sum of i's friends values for W1, W2 and W3:
D <- D + node("A", distr = "rbern",
              prob = plogis(2 + -0.5 * W1 +
                            -0.1 * sum(W1[[1:Kmax]]) +
                            -0.4 * sum(W2[[1:Kmax]]) +
                            -0.7 * sum(W3[[1:Kmax]])),
              replaceNAw0 = TRUE)
# Y[i] is a the total N of i's friends who are infected AND untreated
# + a function of friends W3 values
D <- D + node("pYRisk", distr = "rconst",
              const = plogis(-1 + 2 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) +
                              -1.5 * sum(W3[[1:Kmax]])),
              replaceNAw0 = TRUE)

D <- D + node("Y", distr = "rbern", prob = pYRisk)
Dset <- set.DAG(D, n.test = 100)

# Simulating data from the above sem:
datnet <- sim(Dset, n = 1000, rndseed = 543)
head(datnet, 10)
# Obtaining the network object from simulated data:
net_object <- attributes(datnet)$netind_cl
# Max number of friends:
net_object$Kmax
# Network matrix
head(attributes(datnet)$netind_cl$NetInd)
plotDAG(Dset)
