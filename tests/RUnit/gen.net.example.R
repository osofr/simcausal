#------------------------------------
# Structural equaion model for simulating networks with simcausal R package
# To be added to the test set for simcausal
# author: Oleg Sofrygin 
#------------------------------------
  # library(simcausal)
 
  #------------------------------------------------------------------------------------------------------------
  # The user-defined network sampler fun passed as "netfun" argument to network()
  #------------------------------------------------------------------------------------------------------------
  # Builds NetInd_k matrix of friends/connections
  # Can be any function that:
    # Takes "n" (# of obs) as a first argument
    # Return a matrix with n rows and Kmax columns
  # Arguments Kmax, bslVar[i] (W1) & nF are evaluated in the environment of the simulated data and then passed to generateNET
    # - bslVar[i] has an affects on the probability of selecting i as someone else's friend (weighted sampling)
    # - nF[i] is the total number of friends that need to be sampled for observation i
  generateNET <- function(n, Kmax, bslVar, nF, ...) {
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
          #sample from the possible friend set, with prob for selecting each j being based on categorical bslVar[j]
          # bslVar[i] affects the probability of having [i] selected as someone's friend bslVar
          friends_i <- sort(sample(FriendSampSet, size = nFriendSamp, prob = prob_F[bslVar[FriendSampSet] + 1]))
          # Alternatives:
          # friends_i <- sort(sample(FriendSampSet, size = nFriendSamp)) # no weighting of obs by prob_F
          # friends_i <- FriendSampSet[1:nFriendSamp] # just picking first nF from the list
        }
        # Turn any vector of IDs into a vector of length Kmax, filling each remainder with trailing NA's:
        NetInd_k[index, ] <- c(as.integer(friends_i), rep_len(NA_integer_, Kmax - length(friends_i)))
        nFriendTot[index] <- nFriendTot[index] + nFriendSamp
      }
    }
    return(NetInd_k)
  }

  #------------------------------------------------------------------------------------------------------------
  # THE SEM FOR NETWORKS
  #------------------------------------------------------------------------------------------------------------
  Kmax <- 6
  D <- DAG.empty()

  # W1 - categorical or continuous confounder (5 categories, 0-4)
  nW1cat <- 6
  rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
  D <- D + node("W1", distr = "rbinom2", size = (nW1cat-1), prob = c(0.4, 0.5, 0.7, 0.4))

  # W2 - binary infection status at t=0, positively correlated with W1
  prob_W2 <- seq(0.45, 0.8, by=0.3/nW1cat)[1:nW1cat]
  # prob_W2 <- seq.int(0.45, 0.8, length.out = nW1cat)
  length(prob_W2)
  # D <- D + node("W2", distr = "rbern",
  #             prob = (W1==0)*.(prob_W2[1]) +
  #                    (W1==1)*.(prob_W2[2]) +
  #                    (W1==2)*.(prob_W2[3]) +
  #                    (W1==3)*.(prob_W2[4]) +
  #                    (W1==4)*.(prob_W2[5]) +
  #                    (W1==5)*.(prob_W2[6]))
  # Shorter alternative to above (downside is that parents for node W2 can no longer be extracted from W2 formula):
  D <- D + node("W2", distr = "rbern", asis.params = list(prob = "prob_W2[W1+1]"))
  # same as: asisW2 <- rbinom(n = nrow(dat), size = 1, prob = prob_W2[dat$W1+1])

  # W3 - binary confounder:
  prob_W3 <- 0.6
  D <- D + node("W3", distr = "rbern", prob = prob_W3)

  # nF: samling the total number of friends for each i (nF[i]), each nF[i] is influenced by categorical W1 - inf. risk
  normprob <- function(x) x / sum(x)
  k_arr <-c(1:Kmax)
  pN_0 <- 0.02
  prob_Ni_W1_0 <- normprob(c(pN_0, plogis(-3 - 0 - k_arr / 2)))    # W1=0 probabilities of |F_i|
  prob_Ni_W1_1 <- normprob(c(pN_0, plogis(-1.5 - 0 - k_arr / 3)))  # W1=1 probabilities of |F_i|
  prob_Ni_W1_2 <- normprob(c(pN_0, pnorm(-2*abs(2 - k_arr) / 5)))  # W1=2 probabilities of |F_i|
  prob_Ni_W1_3 <- normprob(c(pN_0, pnorm(-2*abs(3 - k_arr) / 5)))  # W1=3 probabilities of |F_i|
  prob_Ni_W1_4 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 2))))  # W1=4 probabilities of |F_i|
  prob_Ni_W1_5 <- normprob(c(pN_0, plogis(-4 + 2 * (k_arr - 3))))  # W1=5 probabilities of |F_i|
  
  # D <- D + node("nF.plus1", distr = "rcategor.int",
  #               probs = (W1 == 0)*.(prob_Ni_W1_0) +
  #                       (W1 == 1)*.(prob_Ni_W1_1) +
  #                       (W1 == 2)*.(prob_Ni_W1_2) +
  #                       (W1 == 3)*.(prob_Ni_W1_3) +
  #                       (W1 == 4)*.(prob_Ni_W1_4) +
  #                       (W1 == 5)*.(prob_Ni_W1_5))
  # Shorter alternative (the downside to above is that nF.plus1 parents cannot be extracted from node formula):
  probs_Nimat <- rbind(prob_Ni_W1_0, prob_Ni_W1_1, prob_Ni_W1_2, prob_Ni_W1_3, prob_Ni_W1_4, prob_Ni_W1_5)
  D <- D + node("nF.plus1", distr = "rcategor.int", asis.params = list(probs = "probs_Nimat[W1+1,]"))

  # Adding the network generator that depends on nF and categorical W1:
  D <- D + network("NetInd_k", Kmax = Kmax, netfun = "generateNET", bslVar = W1, nF = nF.plus1 - 1)
    # Arguments to network function:
      # "NetInd_k" - the name for the output network matrix (TBD on how to allow access to it)
      # Kmax: required argument for max number of friends
      # netfun: name of the user-defined function that builds NetInd_k matrix, the function takes n as first argument, has to return a matrix with n rows and Kmax columns
      # bslVar, nF: additional named arguments that will be passed on to netfun function


  # A[i] is a function W1[i] and the total of i's friends values W1, W2 and W3:
  betaA0 <- 2; betaA.W1 <- -0.5; betaA.netW1 <- -0.1; betaA.netW2 <- -0.4; betaA.netW3 <- -0.7
  D <- D + node("A", distr = "rbern", 
                prob = plogis(betaA0 + betaA.W1 * W1 +
                              betaA.netW1 * sum(W1[[1:Kmax]]) +
                              betaA.netW2 * sum(W2[[1:Kmax]]) +
                              betaA.netW3 * sum(W3[[1:Kmax]])),
                replaceNAw0 = TRUE)
  # Can also directly generate the summary measures, then use those for generating A:
  # D <- D + node("sNetW1", distr = "rconst", const = sum(W1[[1:Kmax]]), replaceNAw0 = TRUE)
  # D <- D + node("sNetW2", distr = "rconst", const = sum(W2[[1:Kmax]]), replaceNAw0 = TRUE)
  # D <- D + node("sNetW3", distr = "rconst", const = sum(W3[[1:Kmax]]), replaceNAw0 = TRUE)  

  # Y[i] is a function of netW3 (friends of i W3 values) and the total N of i's friends who are infected AND untreated:
  betaY0 <- -1; betaY.AW2 <- 2; betaY.W3 <- -1.5
  D <- D + node("pYRisk", distr = "rconst", 
                const = plogis(betaY0 +
                              betaY.AW2 * sum(W2[[1:Kmax]] * (1 - A[[1:Kmax]])) +
                              betaY.W3 * sum(W3[[1:Kmax]])),
                replaceNAw0 = TRUE)
  # Could also add N untreated friends of i: sum(1 - A[[1:Kmax]])
  # Could also add N infected friends of i: sum(W2[[1:Kmax]])
  D <- D + node("Y", distr = "rbern", prob = pYRisk)

  Dset <- set.DAG(D)

  # plotDAG(Dset)
  # 10 fold increase in n results in ~ x100 increase in sim time:
  t1 <- system.time(
    dat <- sim(Dset, n = 1000, rndseed = 543)
  )

  # t2 <- system.time(
  #   dat <- sim(Dset, n = 10000, rndseed = 543)
  # )
  # t1; t2

  head(dat, 10)
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

  table(dat$W1)
  # w/ rbinom2:
  #   0    1    2    3    4    5 
  # 446 1760 2890 2696 1674  534
  # w/ rbinom:
  #   0    1    2    3    4    5 
  # 760 2629 3429 2303  786   93 
  f.W1 <- function(n) rbinom(n, 5, prob=c(0.4, 0.5, 0.7, 0.4))
  testW1 <- f.W1(10000)
  table(testW1)
  #   0    1    2    3    4    5 
  # 455 1791 2887 2719 1604  544 
  c(mean(dat$W1), mean(dat$W2), mean(dat$W3))
  # [1] 2.4994 0.5734 0.6031
  mean(dat$W2)
  # w/ rbinom2: # [1] 2.4994 0.5734 0.6031
  # w/ rbinom:  # [1] 2.0005 0.5470 0.6031
  mean(dat$A) 
  # w/ rbinom2: # [1] 0.1973
  # w/ rbinom:  # [1] 0.2653
  mean(dat$pYRisk) 
  # w/ rbinom2: # [1] 0.4274796
  # w/ rbinom:  # [1] 0.3714751
  mean(dat$Y) 
  # w/ rbinom2: # [1] 0.4316
  # w/ rbinom2: # [1] 0.371
  mean(dat$nF.plus1-1)
  # w/ rbinom2: # [1] 3.2603
  # w/ rbinom2: # [1] 2.8722

  # Friends network:
  cbind(attributes(dat)$netind_cl$NetInd_k, 
        nF = attributes(dat)$netind_cl$nF, 
        datnF = dat$nF.plus1-1, 
        diff_nF = attributes(dat)$netind_cl$nF - (dat$nF.plus1-1))
  # n Friends per obs:
  attributes(dat)$netind_cl$nF


  # ************************************
  # TESTING MAKING COPIES ($clone()) for R6 objects:
  # ************************************
    newNetcl <- attributes(dat)$netind_cl$clone()
    newNetcl$wipeoutNetInd
    newNetcl$NetInd_k
    attributes(dat)$netind_cl$NetInd_k


