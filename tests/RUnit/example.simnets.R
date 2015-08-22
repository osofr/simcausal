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
generate.igraph.ER <- function(n, m_pn, Kmax, ...) {
  m <- as.integer(m_pn[1]*n)
  if (n<=10) m <- 20
  igraph.ER <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
  sparse_AdjMat <- igraph.to.sparseAdjMat(igraph.ER)
  NetInd_out <- sparseAdjMat.to.NetInd(sparse_AdjMat)
  if (Kmax < NetInd_out$Kmax) message("Kmax changed, new Kmax = ", NetInd_out$Kmax)
  return(NetInd_out$NetInd_k)
}

Kmax <- 5
D <- DAG.empty()

# Sample ER model network using igraph::sample_gnm with m_pn argument:
D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generate.igraph.ER", m_pn = 50)

# W1 - categorical (5 categories, 0-4):
nW1cat <- 6
rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
D <- D + node("W1", distr = "rbinom2", size = (nW1cat-1), prob = c(0.4, 0.5, 0.7, 0.4))

# W2 - binary infection status at t=0, positively correlated with W1:
prob_W2 <- seq.int(0.45, 0.8, length.out = nW1cat)
D <- D + node("W2", distr = "rbern", asis.params = list(prob = "prob_W2[W1+1]"))

# W3 - binary confounder:
prob_W3 <- 0.6
D <- D + node("W3", distr = "rbern", prob = prob_W3)

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

Dset <- set.DAG(D)

# Simulating data from the above sem:
datnet <- sim(Dset, n = 1000, rndseed = 543)
head(datnet, 100)

# Extracting the network matrix from the simulated data:
attributes(datnet)$netind_cl
head(attributes(datnet)$netind_cl$NetInd)

#--------------------------------------------------------------------------------------------------
# EXAMPLE 2. USING CUSTOM NETWORK GENERATING FUNCTION
#--------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------
# Example of a user-defined network sampler(s) function
# Arguments Kmax, bslVar[i] (W1) & nF are evaluated in the environment of the simulated data then 
# passed to generateNET() function
  # - unif.F: when TRUE sample friends set as discrete uniform distr (no weighting by bslVar)
  # - bslVar[i]: used for contructing weights for the probability of selecting i as 
  # someone else's friend (weighted sampling), when missing the sampling goes to uniform
  # - nF[i]: total number of friends that need to be sampled for observation i
#--------------------------------------------------------------------------------------------------
generateNET <- function(n, Kmax, unif.F = FALSE, bslVar, nF, ...) {
  nW1cat <- 6
  W1cat_arr <- c(1:nW1cat)/2
  prob_F <- plogis(-4.5 + 2.5*W1cat_arr) / sum(plogis(-4.5 + 2.5*W1cat_arr))
  NetInd_k <- matrix(NA_integer_, nrow = n, ncol = Kmax)
  nFriendTot <- rep(0L, n)

  for (index in (1:n)) {
    FriendSampSet <- setdiff( c(1:n), index)
    nFriendSamp <- max(nF[index] - nFriendTot[index], 0L)
    if (nFriendSamp>0) {
      if (length(FriendSampSet)==1)  {
        friends_i <- FriendSampSet
      } else {
        if (missing(bslVar) || unif.F[1]) {
           # Sample with uniform prob, no weighting by bslVar:
          friends_i <- sort(sample(FriendSampSet, size = nFriendSamp))
        } else {
          # Sample from the possible friend set, with prob for selecting each j
          # being based on categorical bslVar[j]
          # bslVar[i] affects the probability of having [i] selected as someone's friend bslVar
          friends_i <- sort(sample(FriendSampSet, size = nFriendSamp,
                            prob = prob_F[bslVar[FriendSampSet] + 1]))
        }
      }
      NetInd_k[index, ] <- c(as.integer(friends_i), rep_len(NA_integer_, Kmax - length(friends_i)))
      nFriendTot[index] <- nFriendTot[index] + nFriendSamp
    }
  }
  return(NetInd_k)
}

Kmax <- 6
D <- DAG.empty()

# W1 - categorical or continuous confounder (5 categories, 0-4):
nW1cat <- 6
rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
D <- D + node("W1", distr = "rbinom2", size = (nW1cat-1), prob = c(0.4, 0.5, 0.7, 0.4))

# W2 - binary infection status at t=0, positively correlated with W1:
prob_W2 <- seq.int(0.45, 0.8, length.out = nW1cat)
D <- D + node("W2", distr = "rbern",
            prob = (W1==0)*.(prob_W2[1]) + (W1==1)*.(prob_W2[2]) + (W1==2)*.(prob_W2[3]) +
                   (W1==3)*.(prob_W2[4]) + (W1==4)*.(prob_W2[5]) + (W1==5)*.(prob_W2[6]))

# W3 - binary confounder:
prob_W3 <- 0.6
D <- D + node("W3", distr = "rbern", prob = prob_W3)

# nF: total number of friends for each i (nF[i]), each nF[i] is influenced by categorical W1 
normprob <- function(x) x / sum(x)
k_arr <-c(1:Kmax)
pN_0 <- 0.02
prob_Ni_W1_0 <- normprob(c(pN_0, plogis(-3 - 0 - k_arr / 2)))    # W1=0 probabilities of |F_i|
prob_Ni_W1_1 <- normprob(c(pN_0, plogis(-1.5 - 0 - k_arr / 3)))  # W1=1 probabilities of |F_i|
prob_Ni_W1_2 <- normprob(c(pN_0, pnorm(-2*abs(2 - k_arr) / 5)))  # W1=2 probabilities of |F_i|
prob_Ni_W1_3 <- normprob(c(pN_0, pnorm(-2*abs(3 - k_arr) / 5)))  # W1=3 probabilities of |F_i|
prob_Ni_W1_4 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 2))))  # W1=4 probabilities of |F_i|
prob_Ni_W1_5 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 3))))  # W1=5 probabilities of |F_i|

D <- D + node("nF.plus1", distr = "rcategor.int",
              probs = (W1 == 0)*.(prob_Ni_W1_0) + (W1 == 1)*.(prob_Ni_W1_1) +
                      (W1 == 2)*.(prob_Ni_W1_2) + (W1 == 3)*.(prob_Ni_W1_3) +
                      (W1 == 4)*.(prob_Ni_W1_4) + (W1 == 5)*.(prob_Ni_W1_5))

# Adding the network generator that depends on nF and categorical W1:
D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generateNET", bslVar = W1, nF = nF.plus1 - 1)

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
Dset <- set.DAG(D)

# Simulating data from the above sem:
datnet <- sim(Dset, n = 1000, rndseed = 543)
