#------------------------------------------------------------------------------------------------------------
# Using structural equation models with networks
# Tests for generating networks for a biased network sample (conditional on bslVar) of size nF
# author: Oleg Sofrygin 
#------------------------------------------------------------------------------------------------------------
`%+%` <- function(a, b) paste0(a, b)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
allNA = function(x) all(is.na(x))


# (1) Previously t was set to null when no time-varying nodes were defined (even if user defined t in the user environment)
# (2) Previously special vars Kmax,nF were set to NULL if no network (even if user defined Kmax or nF in user environment)
test.tKmaxnet = function() {
  # ------------------------------------------------------------------
  # THIS NOW WORKS:
  t <- 5
  D.t <- DAG.empty()
  D.t <- D.t + node("t.null", distr = "rconst", const = t)
  D.t <- D.t + node("t", distr = "rconst", const = t)
  Dset0 <- set.DAG(D.t)
  dat0 <- sim(Dset0, n=20)
  # ------------------------------------------------------------------
  # WORKS AS BEFORE (i.e, t<100 IS IGNORED)
  t <- 100
  D.t <- DAG.empty()
  D.t <- D.t + node("A", t=0:1, distr = "rconst", const = t)
  D.t <- D.t + node("B", t=0:1, distr = "rconst", const = A[t]+1)
  Dset0 <- set.DAG(D.t)
  dat0 <- sim(Dset0, n=20)
  # ------------------------------------------------------------------
  # GIVES AN ERROR AS BEFORE:
  t <- 1
  D.t <- DAG.empty()
  D.t <- D.t + node("A", distr = "rconst", const = t)
  D.t <- D.t + node("B", distr = "rconst", const = A[t]+1)
  checkException(Dset0 <- set.DAG(D.t))
  # ------------------------------------------------------------------
  # GIVES AN ERROR - CAN'T FIND THE VAR:
  t <- 1
  D.t <- DAG.empty()
  D.t <- D.t + node("A", distr = "rconst", const = t)
  D.t <- D.t + node("B", distr = "rconst", const = A[xyz]+1)
  checkException(Dset0 <- set.DAG(D.t))
  # THE INDEXING VAR IS A FUNCTION -> GIVES A WARNING AND AN ERROR:
  D.t <- DAG.empty()
  D.t <- D.t + node("A", distr = "rconst", const = t)
  D.t <- D.t + node("B", distr = "rconst", const = A[rnorm]+1)
  checkException(Dset0 <- set.DAG(D.t))
  # ------------------------------------------------------------------
  # USING nF WITHOUT NETWORK:
  nF <- 20
  D.nF <- DAG.empty()
  D.nF <- D.nF + node("nF.null", distr = "rconst", const = nF)
  Dset0 <- set.DAG(D.nF)
  dat0 <- sim(Dset0, n=20)
  # ------------------------------------------------------------------
  # USING Kmax WITHOUT THE NETWORK, DEFINING Kmax node AND THEN RE-USING IT AGAIN
  # (uses the node "Kmax" value first, if found, if not found, goes to user defined value)
  Kmax <- 5
  D.Kmax <- DAG.empty()
  D.Kmax <- D.Kmax + node("Kmax0", distr = "rconst", const = Kmax)
  D.Kmax <- D.Kmax + node("Kmax", distr = "rconst", const = Kmax+1)
  D.Kmax <- D.Kmax + node("Kmax2", distr = "rconst", const = Kmax+1)
  Dset0 <- set.DAG(D.Kmax)
  dat0 <- sim(Dset0, n=20)
  # ------------------------------------------------------------------
  # USING Nsamp (always uses the internal Nsamp and ignores the user defined Nsamp):
  Nsamp <- 5
  D.Nsamp <- DAG.empty()
  D.Nsamp <- D.Nsamp +
    node("Nsamp.null", distr = "rconst", const = Nsamp) +
    node("Nsamp", distr = "rconst", const = Nsamp)
  Dset0 <- set.DAG(D.Nsamp)
  dat0 <- sim(Dset0, n=20)
  checkEquals(dat0$Nsamp[1], 20L)

  # ------------------------------------------------------------------
  # EXAMPLE 1 WITH REGULAR NETWORK:
  # PASSING USER DEFINED KMAX TO THE NETWORK FUNCTION,
  # DEFINING Kmax IT AS A NODE -> SHOULD BE USING THE TRUE Kmax value
  generate.igraph.k.regular <- function(n, Kmax, ...) {
    if (n < 20) Kmax <- 5
    igraph.reg <- igraph::sample_k_regular(no.of.nodes = n, k = Kmax, directed = TRUE, multiple = FALSE)
    sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(igraph.reg)
    NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
    return(NetInd_out$NetInd_k)
  }
  Kmax <- 20
  D <- DAG.empty()
  D <- D + network("NetInd", netfun = "generate.igraph.k.regular", Kmax = Kmax)
  D <- D +
    node("Kmax", distr = "rconst", const = Kmax) +
    node("nF", distr = "rconst", const = nF) +
    node("W", distr = "rbern", prob = 0.3) +
    node("netsumW", distr = "rconst", const = sum(W[[1:Kmax]]), replaceNAw0 = TRUE)
  Dset.net1 <- set.DAG(D, n.test = 200)
  dat.net1 <- sim(Dset.net1, n=50)

  # ------------------------------------------------------------------
  # EXAMPLE 2 WITH SMALL WORLD NETWORK (ARGUMENT Kmax to generate.igraph.smallwld doesn't do anything)
  # PASSING USER DEFINED KMAX TO THE NETWORK FUNCTION,
  # DEFINING Kmax IT AS A NODE -> SHOULD BE USING THE TRUE Kmax value
  generate.igraph.smallwld <- function(n, Kmax, dim, nei, p, ...) {
    g <- igraph::sample_smallworld(dim = 1, size = n, nei = nei, p = p, loops = FALSE, multiple = FALSE)
    g <- igraph::as.directed(g, mode = c("mutual"))
    sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(g)
    NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
    return(NetInd_out$NetInd_k)
  }
  Kmax <- 20
  D <- DAG.empty()
  D <- D +
    network("NetInd", netfun = "generate.igraph.smallwld", Kmax = Kmax, dim = 1, nei = 9, p = 0.1)
    # network("NetInd", netfun = "generate.igraph.smallwld", Kmax = Kmax, dim = 1, nei = 4, p = 0.05)
  D <- D +
    node("Kmax", distr = "rconst", const = Kmax) +
    node("nF", distr = "rconst", const = nF) +
    node("W", distr = "rbern", prob = 0.3) +
    node("netsumW", distr = "rconst", const = sum(W[[1:Kmax]]), replaceNAw0 = TRUE)
  Dset.net2 <- set.DAG(D, n.test = 200)
  dat.net2 <- sim(Dset.net2, n=50)

}



test.networkgen1 <- function() {
  #------------------------------------------------------------------------------------------------------------
  # EXAMPLE 1. SIMULATING NETWORKS WITH igraph R package (gen.ER)
  #------------------------------------------------------------------------------------------------------------

  #------------------------------------------------------------------------------------------------------------
  # The user-defined network sampler(s) from igraph (ER model)
  #------------------------------------------------------------------------------------------------------------
  # Generate random graphs according to the G(n,p) or G(n,m) Erdos-Renyi model
  # p - Probability of making an edge between two observations
  # m_pn - above 0, a total number of edges in the network as a proportion of n (sample size)
  # m_psquare - define m argument to igraph::sample_gnm as sqrt(n)
  gen.ER <- function(n, p, m_pn, m_psquare, ...) {
    if (!missing(p)) {
      igraph.ER <- igraph::sample_gnp(n = n, p = p[1], directed = TRUE)
    } else if (!missing(m_pn)) {
      m <- as.integer(m_pn[1]*n)
      if (n < 100) m <- n
      message("simulating network with ER model using m: " %+% m)
      igraph.ER <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
    } else if (!missing(m_psquare)) {
      m <- as.integer(n^2/2)
      if (n < 100) m <- n
      message("simulating network with ER model using m: " %+% m)
      igraph.ER <- igraph::sample_gnm(n = n, m = m, directed = TRUE)
    } else {
      stop("one of the arguments: p, m_pn or m_psquare must be specified")
    }
    # From igraph object to sparse adj. matrix:
    sparse_AdjMat <- igraph.to.sparseAdjMat(igraph.ER)
    # From igraph object to simcausal/tmlenet input (NetInd_k, nF, Kmax):
    NetInd_out <- sparseAdjMat.to.NetInd(sparse_AdjMat)
    # print("old Kmax"); print(Kmax)
    print("automatic Kmax: "); print(NetInd_out$Kmax)
    print("NetInd_k"); print(head(NetInd_out$NetInd_k))
    # if (Kmax < NetInd_out$Kmax) message("new network has larger Kmax value than requested, new Kmax = " %+% NetInd_out$Kmax)
    return(NetInd_out$NetInd_k)
  }

  D <- DAG.empty()
  # ------------------------------------------------------------------------------------
  # PUT THIS IN A FUNCTION....
  # Sample ER model network using igraph::sample_gnp with p argument:
  p.set <- 0.05
  D <- D + network("Net.sample", netfun = "gen.ER", p = p.set)
  D1 <- set.DAG(D)
  # Sample ER model network using igraph::sample_gnm with m_pn argument:
  D <- D + network("Net.sample", netfun = "gen.ER", m_pn = 50)
  D2 <- set.DAG(D)
  # Sample ER model network using igraph::sample_gnm with m_psquare argument (m is sqrt(n)):
  D <- D + network("Net.sample", netfun = "gen.ER", m_psquare = TRUE)
  D3 <- set.DAG(D)
# ------------------------------------------------------------------------------------

  nW1cat <- 6 # W1 - categorical or continuous confounder (5 categories, 0-4)
  rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
  D <- D + node("W1", distr = "rbinom2", size = (nW1cat-1), prob = c(0.4, 0.5, 0.7, 0.4))

  # W2 - binary infection status at t=0, positively correlated with W1
  prob_W2 <- seq(0.45, 0.8, by=0.3/nW1cat)[1:nW1cat]
  # prob_W2 <- seq.int(0.45, 0.8, length.out = nW1cat)
  length(prob_W2)
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

  # Y[i] is a function of netW3 (friends of i W3 values) and the total N of i's friends who are infected AND untreated:
  D <- D + node("pYRisk", distr = "rconst",
                const = plogis(-1 + 2 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) + -1.5 * sum(W3[[1:Kmax]])),
                replaceNAw0 = TRUE)
  D <- D + node("Y", distr = "rbern", prob = pYRisk)
  Dset <- set.DAG(D)
  # plotDAG(Dset)
  dat <- sim(Dset, n = 1000, rndseed = 543)
  # To examine the network:
  attributes(dat)$netind_cl
  head(attributes(dat)$netind_cl$NetInd)
}


#------------------------------------------------------------------------------------------------------------
# Adding time-varying network node indexing
#------------------------------------------------------------------------------------------------------------
test.networkgen_time <- function() {
  #------------------------------------------------------------------------------------------------------------
  # EXAMPLE. SIMULATING NETWORKS WITH igraph R package (gen.k.regular)
  #------------------------------------------------------------------------------------------------------------
  gen.k.regular <- function(n, K, ...) {
      if (n < 20) K <- 5
      igraph.reg <- igraph::sample_k_regular(no.of.nodes = n,
                                          k = K,
                                          directed = TRUE,
                                          multiple = FALSE)
      sparse_AdjMat <- simcausal::igraph.to.sparseAdjMat(igraph.reg)
      NetInd_out <- simcausal::sparseAdjMat.to.NetInd(sparse_AdjMat)
      return(NetInd_out$NetInd_k)
  }

  K <- 10
  #------------------------------------------------------------------------------------------------------------
  # Test for error when net referrencing non-existing var
  #------------------------------------------------------------------------------------------------------------
  W2 <- rnorm(100)
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("F.W2", distr = "rconst", t = 0, const = W2[[0]])
  checkException(Dset <- set.DAG(D))

  #------------------------------------------------------------------------------------------------------------
  # Test for error when net referrencing gets matrix with >1 columns to eval
  #------------------------------------------------------------------------------------------------------------
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("F.W1", distr = "rconst", t = 0:1, const = W1[[0]]) +
      node("F.W1", distr = "rconst", t = 2, const = F.W1[0:1][[0]])
  checkException(Dset <- set.DAG(D))

  #------------------------------------------------------------------------------------------------------------
  # First references the time-point -> then the network var of that time-point
  #------------------------------------------------------------------------------------------------------------
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("W2", distr = "rbern", prob = 0.3) +
      node("W3", distr = "rbern", prob = 0.3) +
      node("A.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
      node("A", distr = "rnorm", mean = A.mu, sd = 1) +
      node("F.A", distr = "rconst", t = 0, const = A[[0]]) +
      # reference previous value of F.A, then reference the first friend:
      node("F.A", distr = "rconst", t = 1:5, const = F.A[t-1][[1]])
  Dset <- set.DAG(D)
  dat <- sim(Dset, n = 500, rndseed = 543)
  # To examine the network:
  # attributes(dat)$netind_cl
  # head(attributes(dat)$netind_cl$NetInd)
  head(dat)

  #------------------------------------------------------------------------------------------------------------
  # References several time-points -> create 1dim summary meaure -> then the network var value of that summary for one friend
  #------------------------------------------------------------------------------------------------------------
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("W2", distr = "rbern", prob = 0.3) +
      node("W3", distr = "rbern", prob = 0.3) +
      node("A.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
      node("A", distr = "rnorm", mean = A.mu, sd = 1) +
      node("F.A", distr = "rconst", t = 0, const = A[[0]]) +
      # reference previous value of F.A, then reference the first friend:
      node("F.A", distr = "rconst", t = 1:5, const = sum(F.A[0:(t-1)])[[0]])
  Dset <- set.DAG(D)
  dat <- sim(Dset, n = 500, rndseed = 543)
  # To examine the network:
  # attributes(dat)$netind_cl
  # head(attributes(dat)$netind_cl$NetInd)
  head(dat)

  # plotDAG(Dset, tmax = 1, xjitter = 0.09, yjitter = 0.25,
  #         edge_attrs = list(width = 0.5, arrow.width = 0.4, arrow.size = 0.6),
  #         vertex_attrs = list(size = 12, label.cex = 0.7))
  # , customvlabs = labs
  # plotDAG(Dset, tmax = 2)

  #------------------------------------------------------------------------------------------------------------
  # References several time-points -> create 1dim summary meaure -> reference several friend values of that summary
  # -> create 1 dim summary measure of those time-point summaries
  #------------------------------------------------------------------------------------------------------------
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("W2", distr = "rbern", prob = 0.3) +
      node("W3", distr = "rbern", prob = 0.3) +
      node("A.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
      node("A", distr = "rnorm", mean = A.mu, sd = 1) +
      node("F.A", distr = "rconst", t = 0, const = A[[0]]) +
      # reference previous value of F.A, then reference the first friend:
      node("F.A", distr = "rconst", t = 1:5, const = mean(sum(F.A[0:(t-1)])[[0:Kmax]]))
  Dset <- set.DAG(D)
  dat <- sim(Dset, n = 500, rndseed = 543)
  head(dat)

  # plotDAG(Dset, tmax = 1, xjitter = 0.09, yjitter = 0.25,
  #         edge_attrs = list(width = 0.5, arrow.width = 0.4, arrow.size = 0.6),
  #         vertex_attrs = list(size = 12, label.cex = 0.7))
  # # , customvlabs = labs
  # plotDAG(Dset, tmax = 2)


  #------------------------------------------------------------------------------------------------------------
  # Going the other way: indexing friends, then time-points of those friends:
  # Breaks down because time indexing is implemented in a completely different way,
  # Every expression w/ t, s.a., "Var[t]" is converted to "Var_t", which must already exist in the data.frame
  #------------------------------------------------------------------------------------------------------------
  K <- 10
  D <- DAG.empty()
  D <- D + network("Net1", netfun = "gen.k.regular", K = K)
  D <- D +
      node("W1", distr = "rbern", prob = 0.5) +
      node("W2", distr = "rbern", prob = 0.3) +
      node("W3", distr = "rbern", prob = 0.3) +
      node("A.mu", distr = "rconst", const = (0.98 * W1 + 0.58 * W2 + 0.33 * W3)) +
      node("A", distr = "rnorm", mean = A.mu, sd = 1) +
      node("F.A", distr = "rconst", t = 0, const = A[[0]]) +
      # reference previous value of F.A, then reference the first friend:
      node("F.A", distr = "rconst", t = 1:5, const = (F.A[[1]])[t-1])
  checkException(Dset <- set.DAG(D))
}

test.networkgen2 <- function() {
  #------------------------------------------------------------------------------------------------------------
  # EXAMPLE 2. SIMULATING DATA FROM SEM WITH A CUSTOM NETWORK GENERATING FUNCTION generateNET()
  #------------------------------------------------------------------------------------------------------------
  K <- 6
  D <- DAG.empty()
  #------------------------------------------------------------------------------------------------------------
  # An example of a user-defined network sampler(s) function that will be specified as "netfun" argument to the network() function
  #------------------------------------------------------------------------------------------------------------
  # Builds NetInd_k matrix of friends/connections
  # In general this can be any function that:
    # Takes "n" (# of obs) as a first argument
    # Returns a matrix with n rows and K columns with row i consisting of IDs (row numbers) of i's friends (connections that can influence i)
  # Arguments K, bslVar[i] (W1) & nF are evaluated in the environment of the simulated data then passed to generateNET() function
    # - unif.F: when TRUE sample set F (connections/friends) from the uniform discrete distr (no weighting by bslVar)
    # - bslVar[i]: used for contructing weights for the probability of selecting i as someone else's friend (weighted sampling), when missing the sampling goes to uniform
    # - nF[i]: total number of friends that need to be sampled for observation i
  genNET <- function(n, K, unif.F = FALSE, bslVar, nF, ...) {
    print("K in genNET: " %+% K);
    nW1cat <- 6
    W1cat_arr <- c(1:nW1cat)/2
    prob_F <- plogis(-4.5 + 2.5*W1cat_arr) / sum(plogis(-4.5 + 2.5*W1cat_arr))

    NetInd_k <- matrix(NA_integer_, nrow = n, ncol = K)
    nFriendTot <- rep(0L, n)
    for (index in (1:n)) {
      FriendSampSet <- setdiff( c(1:n), index)  #set of possible friends to sample, anyone but itself
      nFriendSamp <- max(nF[index] - nFriendTot[index], 0L) #check i`s network is not already filled to max
      if (nFriendSamp>0) {
        if (length(FriendSampSet)==1)  {  # To handle the case with |FriendSampSet| = 1
          friends_i <- FriendSampSet
        } else {
          if (missing(bslVar) || unif.F[1]) {
            friends_i <- sort(sample(FriendSampSet, size = nFriendSamp)) # sample with uniform prob, no weighting by bslVar
            # friends_i <- FriendSampSet[1:nFriendSamp] # another alternative to just pick first nF from the list of available friends
          } else {
            #sample from the possible friend set, with prob for selecting each j being based on categorical bslVar[j]
            # bslVar[i] affects the probability of having [i] selected as someone's friend bslVar
            friends_i <- sort(sample(FriendSampSet, size = nFriendSamp, prob = prob_F[bslVar[FriendSampSet] + 1]))
          }
        }
        # Turn any vector of IDs into a vector of length K, filling each remainder with trailing NA's:
        NetInd_k[index, ] <- c(as.integer(friends_i), rep_len(NA_integer_, K - length(friends_i)))
        nFriendTot[index] <- nFriendTot[index] + nFriendSamp
      }
    }
    return(NetInd_k)
  }

  normprob <- function(x) x / sum(x)
  k_arr <-c(1:K)
  pN_0 <- 0.02
  prob_Ni_W1_0 <- normprob(c(pN_0, plogis(-3 - 0 - k_arr / 2)))    # W1=0 probabilities of |F_i|
  prob_Ni_W1_1 <- normprob(c(pN_0, plogis(-1.5 - 0 - k_arr / 3)))  # W1=1 probabilities of |F_i|
  prob_Ni_W1_2 <- normprob(c(pN_0, pnorm(-2*abs(2 - k_arr) / 5)))  # W1=2 probabilities of |F_i|
  prob_Ni_W1_3 <- normprob(c(pN_0, pnorm(-2*abs(3 - k_arr) / 5)))  # W1=3 probabilities of |F_i|
  prob_Ni_W1_4 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 2))))  # W1=4 probabilities of |F_i|
  prob_Ni_W1_5 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 3))))  # W1=5 probabilities of |F_i|
  
  # W1 - categorical or continuous confounder (5 categories, 0-4):
  nW1cat <- 6
  rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
  D <- D + node("W1", distr = "rbinom2", size = (nW1cat-1), prob = c(0.4, 0.5, 0.7, 0.4))

  # W2 - binary infection status at t=0, positively correlated with W1:
  # prob_W2 <- seq.int(0.45, 0.8, length.out = nW1cat)
  prob_W2 <- seq(0.45, 0.8, by=0.3/nW1cat)[1:nW1cat]
  # D <- D + node("W2", distr = "rbern",
  #             prob = (W1==0)*.(prob_W2[1]) + (W1==1)*.(prob_W2[2]) + (W1==2)*.(prob_W2[3]) +
  #                    (W1==3)*.(prob_W2[4]) + (W1==4)*.(prob_W2[5]) + (W1==5)*.(prob_W2[6]))
  D <- D + node("W2", distr = "rbern", asis.params = list(prob = "prob_W2[W1+1]"))

  # W3 - binary confounder:
  prob_W3 <- 0.6
  D <- D + node("W3", distr = "rbern", prob = prob_W3)

  # nF: total number of friends (connections) for each i (nF[i]), each nF[i] is influenced by categorical W1 (infect risk)
  probs_Nimat <- rbind(prob_Ni_W1_0, prob_Ni_W1_1, prob_Ni_W1_2, prob_Ni_W1_3, prob_Ni_W1_4, prob_Ni_W1_5)
  # D <- D + node("nF.plus1", distr = "rcategor.int",
  #               probs = (W1 == 0)*.(prob_Ni_W1_0) + (W1 == 1)*.(prob_Ni_W1_1) + (W1 == 2)*.(prob_Ni_W1_2) +
  #                       (W1 == 3)*.(prob_Ni_W1_3) + (W1 == 4)*.(prob_Ni_W1_4) + (W1 == 5)*.(prob_Ni_W1_5))
  D <- D + node("nF.plus1", distr = "rcategor.int", asis.params = list(probs = "probs_Nimat[W1+1,]"))

  # Adding the network generator that depends on nF and categorical W1:
  # Arguments to network function:
    # "NetInd_k" - the name for the output network matrix (TBD on how to allow access to it)
    # K: max number of friends
    # netfun: name of the user-defined function that builds NetInd_k matrix, the function takes n as first argument, has to return a matrix with n rows and K columns
    # bslVar, nF: additional named arguments that will be passed on to netfun function
  D <- D + network("Net1", netfun = "genNET", K = K, bslVar = W1, nF = nF.plus1 - 1)

  # Define A[i] is a function W1[i] as well as the total sum of i's friends values for W1, W2 and W3:
  D <- D + node("A", distr = "rbern",
                prob = plogis(2 + -0.5 * W1 +
                              -0.1 * sum(W1[[1:Kmax]]) +
                              -0.4 * sum(W2[[1:Kmax]]) +
                              -0.7 * sum(W3[[1:Kmax]])),
                replaceNAw0 = TRUE)
  # Could also first generate the above summary measures as below, then use those for defining node A:
  # D <- D + node("sNetW1", distr = "rconst", const = sum(W1[[1:Kmax]]), replaceNAw0 = TRUE)
  # D <- D + node("sNetW2", distr = "rconst", const = sum(W2[[1:Kmax]]), replaceNAw0 = TRUE)
  # D <- D + node("sNetW3", distr = "rconst", const = sum(W3[[1:Kmax]]), replaceNAw0 = TRUE)

  # Y[i] is a function of netW3 (friends of i W3 values) and the total N of i's friends who are infected AND untreated:
  D <- D + node("pYRisk", distr = "rconst",
              const = plogis(-1 + 2 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) + -1.5 * sum(W3[[1:Kmax]])),
              replaceNAw0 = TRUE)
  # Could also add N untreated friends of i to the above SEM: sum(1 - A[[1:Kmax]])
  # Could also add N infected friends of i to the above SEM: sum(W2[[1:Kmax]])

  D <- D + node("Y", distr = "rbern", prob = pYRisk)
  Dset <- set.DAG(D)

  # plotDAG(Dset) # Visualize the resulting DAG:

  #-------------------------------------------
  # SIMULATING DATA FROM THE ABOVE SEM
  #-------------------------------------------
  dat <- sim(Dset, n = 1000, rndseed = 543)

  #--------------------------------------------------
  # EXCTRACTING THE NETWORK FROM THE SIMULATED DATA:
  #--------------------------------------------------
  net <- cbind(attributes(dat)$netind_cl$NetInd_k,
        nF = attributes(dat)$netind_cl$nF,
        datnF = dat$nF.plus1-1,
        diff_nF = attributes(dat)$netind_cl$nF - (dat$nF.plus1-1))
  # n Friends per obs:
  nFriends <- attributes(dat)$netind_cl$nF

  #-------------------------------------------
  # TO MATCH WITH PREV RESULTS GENERATED MANUALLY:
  # dat <- sim(Dset, n = 10000, rndseed = 543)
  # head(dat, 10)
  #    ID W1 W2 W3 nF.plus1 A     pYRisk Y
  # 1   1  3  1  1        1 1 0.26894142 0
  # 2   2  4  1  1        6 0 0.92414182 1
  # 3   3  3  0  1        3 0 0.73105858 0
  # 4   4  1  0  0        3 1 0.07585818 0
  # 5   5  2  0  0        4 0 0.50000000 1
  # 6   6  4  1  1        5 0 0.01798621 0
  # 7   7  4  1  1        7 0 0.99752738 1
  # 8   8  1  0  0        1 0 0.26894142 0
  # 9   9  1  0  1        2 1 0.73105858 0
  # 10 10  2  0  1        4 0 0.11920292 0
  # table(dat$W1)
  # w/ rbinom2:
  #   0    1    2    3    4    5
  # 446 1760 2890 2696 1674  534
  # w/ rbinom:
  #   0    1    2    3    4    5
  # 760 2629 3429 2303  786   93
  # c(mean(dat$W1), mean(dat$W2), mean(dat$W3))
  # [1] 2.4994 0.5734 0.6031
}
