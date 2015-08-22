#------------------------------------------------------------------------------------------------------------
# Using structural equation models with networks
# Tests for generating networks for a biased network sample (conditional on bslVar) of size nF
# author: Oleg Sofrygin 
#------------------------------------------------------------------------------------------------------------
`%+%` <- function(a, b) paste0(a, b)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
allNA = function(x) all(is.na(x))


test.networkgen1 <- function() {
  #------------------------------------------------------------------------------------------------------------
  # EXAMPLE 1. SIMULATING NETWORKS WITH igraph R package (generate.igraph.ER)
  #------------------------------------------------------------------------------------------------------------

  #------------------------------------------------------------------------------------------------------------
  # The user-defined network sampler(s) from igraph (ER model)
  #------------------------------------------------------------------------------------------------------------
  # Generate random graphs according to the G(n,p) or G(n,m) Erdos-Renyi model
  # p - Probability of making an edge between two observations
  # m_pn - above 0, a total number of edges in the network as a proportion of n (sample size)
  # m_psquare - define m argument to igraph::sample_gnm as sqrt(n)
  generate.igraph.ER <- function(n, p, m_pn, m_psquare, Kmax, ...) {
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
    print("old Kmax"); print(Kmax)
    print("new Kmax"); print(NetInd_out$Kmax)
    print("NetInd_k"); print(head(NetInd_out$NetInd_k))
    if (Kmax < NetInd_out$Kmax) message("new network has larger Kmax value than requested, new Kmax = " %+% NetInd_out$Kmax)
    return(NetInd_out$NetInd_k)
  }

  Kmax <- 5
  D <- DAG.empty()

  # ------------------------------------------------------------------------------------
  # PUT THIS IN A FUNCTION....
  # Sample ER model network using igraph::sample_gnp with p argument:
  D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generate.igraph.ER", p = 0.05)
  D1 <- set.DAG(D)
  # Sample ER model network using igraph::sample_gnm with m_pn argument:
  D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generate.igraph.ER", m_pn = 50)
  D2 <- set.DAG(D)
  # Sample ER model network using igraph::sample_gnm with m_psquare argument (m is sqrt(n)):
  D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generate.igraph.ER", m_psquare = TRUE)
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

  # Can plot the observed network with igraph.... Future implementations
  # ...
}


test.networkgen2 <- function() {
  #------------------------------------------------------------------------------------------------------------
  # EXAMPLE 2. SIMULATING DATA FROM SEM WITH A CUSTOM NETWORK GENERATING FUNCTION generateNET()
  #------------------------------------------------------------------------------------------------------------
  Kmax <- 6
  D <- DAG.empty()
  #------------------------------------------------------------------------------------------------------------
  # An example of a user-defined network sampler(s) function that will be specified as "netfun" argument to the network() function
  #------------------------------------------------------------------------------------------------------------
  # Builds NetInd_k matrix of friends/connections
  # In general this can be any function that:
    # Takes "n" (# of obs) as a first argument
    # Returns a matrix with n rows and Kmax columns with row i consisting of IDs (row numbers) of i's friends (connections that can influence i)
  # Arguments Kmax, bslVar[i] (W1) & nF are evaluated in the environment of the simulated data then passed to generateNET() function
    # - unif.F: when TRUE sample set F (connections/friends) from the uniform discrete distr (no weighting by bslVar)
    # - bslVar[i]: used for contructing weights for the probability of selecting i as someone else's friend (weighted sampling), when missing the sampling goes to uniform
    # - nF[i]: total number of friends that need to be sampled for observation i
  generateNET <- function(n, Kmax, unif.F = FALSE, bslVar, nF, ...) {
    print("Kmax in generateNET: " %+% Kmax);
    nW1cat <- 6
    W1cat_arr <- c(1:nW1cat)/2
    prob_F <- plogis(-4.5 + 2.5*W1cat_arr) / sum(plogis(-4.5 + 2.5*W1cat_arr))

    NetInd_k <- matrix(NA_integer_, nrow = n, ncol = Kmax)
    nFriendTot <- rep(0L, n)
    for (index in (1:n)) {
      FriendSampSet <- setdiff( c(1:n), index)  #set of possible friends to sample, anyone but itself
      nFriendSamp <- max(nF[index] - nFriendTot[index], 0L) #check i's network is not already filled to max
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
        # Turn any vector of IDs into a vector of length Kmax, filling each remainder with trailing NA's:
        NetInd_k[index, ] <- c(as.integer(friends_i), rep_len(NA_integer_, Kmax - length(friends_i)))
        nFriendTot[index] <- nFriendTot[index] + nFriendSamp
      }
    }
    return(NetInd_k)
  }

  normprob <- function(x) x / sum(x)
  k_arr <-c(1:Kmax)
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
    # Kmax: required argument for max number of friends
    # netfun: name of the user-defined function that builds NetInd_k matrix, the function takes n as first argument, has to return a matrix with n rows and Kmax columns
    # bslVar, nF: additional named arguments that will be passed on to netfun function
  D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generateNET", bslVar = W1, nF = nF.plus1 - 1)

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
  cbind(attributes(dat)$netind_cl$NetInd_k,
        nF = attributes(dat)$netind_cl$nF,
        datnF = dat$nF.plus1-1,
        diff_nF = attributes(dat)$netind_cl$nF - (dat$nF.plus1-1))
  # n Friends per obs:
  attributes(dat)$netind_cl$nF


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