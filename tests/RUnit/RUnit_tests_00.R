### --- Test setup ---
 
if(FALSE) {
  library("RUnit")
  library("roxygen2")
  library("devtools")
  setwd(".."); setwd(".."); getwd()
  document()
  load_all("./") # load all R files in /R and datasets in /data. Ignores NAMESPACE:
  # simcausal:::debug_set() # SET TO DEBUG MODE

  setwd("..");
  install("simcausal", build_vignettes = FALSE) # INSTALL W/ devtools:

  # system("echo $PATH") # see the current path env var
  # system("R CMD Rd2pdf simcausal")  # just create the pdf manual from help files

  # CHECK AND BUILD PACKAGE:
  getwd()
  # setwd("./simcausal"); setwd(".."); getwd()
  devtools::check() # runs full check
  devtools::check(args = c("--no-vignettes"), build_args = c("--no-build-vignettes")) # runs faster
  devtools::build_win(args = "--compact-vignettes") # build package on CRAN servers (windows os?)
  devtools::build(args = "--compact-vignettes") # build package tarball compacting vignettes
  # devtools::build(args = "--no-build-vignettes") # build package tarball compacting vignettes
  # devtools::build() # build package tarball

  # check reverse dependencies:
  devtools::revdep(dependencies = c("Depends", "Imports", "Suggests", "LinkingTo"),
                    recursive = FALSE, ignore = NULL)
  res <- devtools::revdep_check()
  devtools::revdep_check_summary(res)
  # revdep_check_save_logs(res)

  setwd("..")
  
  system("R CMD check --as-cran simcausal_0.5.0.tar.gz") # check R package tar ball prior to CRAN submission
      ## system("R CMD check --no-manual --no-vignettes simcausal") # check without building the pdf manual and not building vignettes
      ## system("R CMD build simcausal --no-build-vignettes")
      ## system("R CMD build simcausal")  
  # devtools::use_travis() # SET UP TRAVIS CONFIG FILE
  # INSTALLING FROM SOURCE:
  # install.packages("./simcausal_0.2.2.tar.gz", repos = NULL, type="source", dependencies=TRUE)
  # library(simcausal)
  # simcausal:::addvectorfcn("poisson")
  # simcausal:::debug_set() # SET TO DEBUG MODE
  # simcausal:::debug_off() # SET DEBUG MODE OFF

  # To install a specific branch:
  # devtools::install_github('osofr/simcausal', ref = "simnet", build_vignettes = FALSE)
  # options(simcausal.verbose = FALSE)
  # devtools::install_github('osofr/simcausal', build_vignettes = FALSE)
  # devtools::install_github('osofr/tmlenet', build_vignettes = FALSE)

  # TEST COVERATE:
  # if your working directory is in the packages base directory
  # package_coverage()
  # or a package in another directory
  # cov <- package_coverage("simcausal")
  # view results as a data.frame
  # as.data.frame(cov)
  # zero_coverage() can be used to filter only uncovered lines.
  # zero_coverage(cov)
}

psi_RDs_DAG2a <- NULL
psi_RDs_DAG2b <- NULL

sample_checks <- function() {   # doesnt run, this is just to show what test functions can be used
  print("Starting tests...")
  checkTrue(1 < 2, "check1")     ## passes fine
  ## checkTrue(1 > 2, "check2")  ## appears as failure in the test protocol
  v <- 1:3
  w <- 1:3
  checkEquals(v, w)               ## passes fine
  names(v) <- c("A", "B", "C")
  ## checkEquals(v, w)            ## fails because v and w have different names
  checkEqualsNumeric(v, w)        ## passes fine because names are ignored
  x <- rep(1:12, 2)
  y <- rep(0:1, 12)
  res <- list(a=1:3, b=letters, LM=lm(y ~ x))
  res2 <- list(a=seq(1,3,by=1), b=letters, LM=lm(y ~ x))
  checkEquals( res, res2)        ## passes fine
  checkIdentical( res, res)
  checkIdentical( res2, res2)
  ## checkIdentical( res, res2)  ## fails because element 'a' differs in type
  fun <- function(x) {
   if(x)
   {
    stop("stop conditions signaled")
   }
   return()
  }
  checkException(fun(TRUE))      ## passes fine
  ## checkException(fun(FALSE))  ## failure, because fun raises no error
  checkException(fun(TRUE), silent=TRUE)
  ##  special constants
  ##  same behaviour as for underlying base functions
  checkEquals(NA, NA)
  checkEquals(NaN, NaN)
  checkEquals(Inf, Inf)
  checkIdentical(NA, NA)
  checkIdentical(NaN, NaN)
  checkIdentical(-Inf, -Inf)
}

`%+%` <- function(a, b) paste0(a, b)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
allNA = function(x) all(is.na(x))


# test that all new nodes added after time-var nodes have a t argument:
test.t.error <- function() {
  D <- DAG.empty()
  D <- D + node("timevar", t=0:5, distr="rconst", const = 1)
  checkException(D <- D + node("nottimevar", distr="rconst", const = 5))
}

# testing n.test arg to set.DAG(), including when n.test=0L
test.Nsamp.n.test <- function() {
  # testing categorical for no errors and no warnings with empty returns (n=0)
  checkEquals(length(rcategor.int(n=0, probs = c(0.3,0.3,0.4))),0)
  checkEquals(length(rcategor(n=0, probs = c(0.3,0.3,0.4))),0)
  checkEquals(length(rcategor.int(n=0, probs = matrix(data=c(0.3,0.3,0.4), nrow=5,ncol=3))),0)
  checkEquals(length(rcategor(n=0, probs = matrix(data=c(0.3,0.3,0.4), nrow=5,ncol=3))),0)

  D <- DAG.empty()
  D <- D + node("A", distr = "rbern", prob=0.5) + 
    node("N", distr = "rconst", const=Nsamp)
  Dset <- set.DAG(D)
  dat1 <- sim(Dset, n = 100)

  Dset.ntest <- set.DAG(D, n.test = 200)

  # simulate empty dataset:
  empty.dat1 <- sim(Dset, n = 0)

  Dset.0obs <- set.DAG(D, n.test = 0L)
}


# Adding test for latent vars
test.latent <- function() {
  D <- DAG.empty()
  D <- D +
  node("I", distr = "rcategor.int",
    probs = c(0.1, 0.2, 0.2, 0.2, 0.1, 0.1, 0.1)) +
  node("W1", distr = "rnorm",
    mean = ifelse(I == 1, 0, ifelse(I == 2, 3, 10)) + 0.6 * I, sd = 1) +
  node("W2", distr = "runif",
    min = 0.025*I, max = 0.7*I) +
  node("W3", distr = "rbern",
    prob = plogis(-0.5 + 0.7*W1 + 0.3*W2 - 0.2*I)) +
  node("A", distr = "rbern",
    prob = plogis(+4.2 - 0.5*W1 + 0.2*W2/2 + 0.2*W3)) +
  node("U.Y", distr = "rnorm", mean = 0, sd = 1) +
  node("Y", distr = "rconst",
    const = -0.5 + 1.2*A + 0.1*W1 + 0.3*W2 + 0.2*W3 + 0.2*I + U.Y)
  Dset1 <- set.DAG(D, latent.v = c("I", "U.Y"))
  plotDAG(Dset1)

  # testing n.test=0 data sim for empty returns:
  Dset1.n0 <- set.DAG(D, latent.v = c("I", "U.Y"), n.test=0)
  plotDAG(Dset1.n0)

  # testing data sim for empty returns:
  Odatsim.empty <- simobs(Dset1, n = 0, rndseed = 1)
  checkEquals(nrow(Odatsim.empty),0)

  # testing data sim with 1 obs:
  Odatsim.1obs <- simobs(Dset1, n = 1, rndseed = 1)
  checkEquals(nrow(Odatsim.1obs),1)

  Odatsim <- simobs(Dset1, n = 200, rndseed = 1)  

  Dset1 <- Dset1 + action("A1", nodes = node("A", distr = "rbern", prob = 1))
  Fdatsim <- sim(DAG = Dset1, actions = c("A1"), n = 200, rndseed = 123)
  checkException(
    Dset1 <- Dset1 + action("A1.latent", nodes = node("I", distr = "rbern", prob = 1))
  )
}

# Adding test for custom distr funs and an error message for non-existing distribution functions:
test.noexistdistr <- function() {
  # TEST 1: No longer rroduces an error, since a separate user.env is now captured by each node, not just set.DAG
  funDAG1 <- function() {
    D <- DAG.empty()
    rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
    D <- D + node("W1", distr = "rbinom2", size = 4, prob = c(0.4, 0.5, 0.7, 0.4))
  }
  D <- funDAG1()
  Dset <- set.DAG(D)

  # TEST 2: This always worked, since rbinom2 and set.DAG are now in the same environment:
  funDAG2 <- function() {
    D <- DAG.empty()
    rbinom2 <- function(n, size, prob) rbinom(n, size = size, prob = prob[1,])
    D <- D + node("W1", distr = "rbinom2", size = 4, prob = c(0.4, 0.5, 0.7, 0.4))
    Dset <- set.DAG(D)
    Dset
  }
  Dset <- funDAG2()

  # TEST 3: Exception at undeclared distributions
  D <- DAG.empty()
  checkException(D <- D + node("W1", distr = "rbinom3", size = 4, prob = c(0.4, 0.5, 0.7, 0.4)))
  checkException(D <- D + network("net", netfun = "rbinom3", Kmax = 5, size = 4, prob = c(0.4, 0.5, 0.7, 0.4)))

  # TEST 4: Overridding package distributions. What happens?
  # NOTHING. THE PACKAGE DEFINED DISTRIBUTIONS (OR ANY OTHER PACKAGE FUNCTIONS) CANNOT BE OVERRIDDEN!
  rbern <- function(n, prob) {
    message("i've been overridden!")
    print("i've been overridden!")
    rbinom(n, size = 1, prob = prob)
  }
  D <- DAG.empty()
  D <- D + node("W1", distr = "rbern", prob = 0.5)
  Dset <- set.DAG(D)
  dat <- sim(Dset, n = 20)
}

# ADDING FUNCTIONALITY THAT ALLOWS EFU ARG TO BE ANY LOGICAL R EXPRESSION (allows for conditional right-censoring)
test.EFUeval <- function(){
  Drm <- DAG.empty()
  Drm <- Drm +
    node("C.time", t = 0:5, distr = "rbern", prob = if(t==0) {0.2} else {ifelse(C.time[t-1]==1,1,0.2)}) + # value 1 indicates that censoring time has arrived
    node("Y", t = 0:5, distr = "rbern", prob = 0.3, EFU = ifelse(C.time[t]==1,TRUE,FALSE)) + # outcome that becomes a censoring var only after C.time[t]=1
    node("D", t = 0:5, distr = "rbern", prob = 0.2, EFU = TRUE) # another outcome that is always a censoring variable
  D <- set.DAG(Drm)
  dat <- sim(D, n=100)

  # testing n.test=0 sim for empty returns:
  D.n0 <- set.DAG(Drm, n.test=0)
  plotDAG(D.n0)

  # testing data sim for empty returns:
  Odatsim.empty <- simobs(D, n = 0, rndseed = 1)
  checkEquals(nrow(Odatsim.empty),0)
  checkIdentical(names(dat), names(Odatsim.empty))
  
  # testing data sim with 1 obs:
  Odatsim.1obs <- simobs(D, n = 1, rndseed = 1)
  checkEquals(nrow(Odatsim.1obs),1)
}


# DAG2 (from tech specs): defining actions with a new constructor and passing attributes
test.set.DAG_DAG2b_newactions <- function() {
    library(simcausal)
    #-------------------------------------------------------------
    # EXAMPLE 1: lets start with a simple example of a (W,A,Y) DAG
    #-------------------------------------------------------------
    # Allowing user.env scalar vars to be used in node formulas:
    rconst <- 0.02
    D <- DAG.empty()
    D <- D+ node("W1", distr = "rbern", prob = plogis(-0.5)) + 
            node("W2", distr = "rbern", prob = plogis(-0.5 + 0.5*W1)) + 
            node("W3", distr = "rbern", prob = plogis(-0.5 + 0.7*W1 + 0.3*W2)) + 
            node("A",  distr = "rbern", prob = plogis(-0.5 - 0.3*W1 - 0.3*W2 - 0.2*W3)) + 
            node("Y",  distr = "rbern", prob = plogis(-0.1 + rconst + 1.2*A + 0.3*W1 + 0.3*W2 + 0.2*W3), EFU=TRUE)
    D_WAY <- set.DAG(D)
    D_WAY_0obs <- set.DAG(D, n.test=0)


    # Allowing user.env vectors to be used in node formulas and be indexed by t:
    rconst <- c(0.02, 0.05)
    D <- DAG.empty()
    D <- D+ node("W1", t=0:1, distr = "rbern", prob = plogis(-0.5)) + 
            node("W2", t=0:1, distr = "rbern", prob = plogis(-0.5 + 0.5*W1[t])) + 
            node("W3", t=0:1, distr = "rbern", prob = plogis(-0.5 + 0.7*W1[t] + 0.3*W2[t])) + 
            node("A",  t=0:1, distr = "rbern", prob = plogis(-0.5 - 0.3*W1[t] - 0.3*W2[t] - 0.2*W3[t])) + 
            # node("Y",  t=0:1, distr = "rbern", prob = plogis(-0.1 + 1.2*A[t] + 0.3*W1[t] + 0.3*W2[t] + 0.2*W3[t]), EFU=TRUE)
            # THIS WORKS, BUT ITS UNCLEAR WHAT IS BEING USED FOR rconst. 
            # **************** THIS SHOULD RETURN AN ERROR ********************
            node("Y",  t=0:1, distr = "rbern", prob = plogis(-0.1 + rconst + 1.2*A[0] + 0.3*W1[0] + 0.3*W2[0] + 0.2*W3[0]), EFU=TRUE)
            # THIS OBVIOUSLY FAILES, AS IT SHOULD:
            # Error in rconst[0L] : undefined time-dependent variable(s): rconst_0
            # node("Y",  t=0:1, distr = "rbern", prob = plogis(-0.1 + rconst[t] + 1.2*A[0] + 0.3*W1[0] + 0.3*W2[0] + 0.2*W3[0]), EFU=TRUE)
    D_WAY <- set.DAG(D)
    D_WAY_0obs <- set.DAG(D, n.test=0)
    datn50 <- sim(D_WAY, n=50)

    #-------------------------------------------------------------
    # EXAMPLE 1: simple example of a (W,A,Y) DAG
    #-------------------------------------------------------------
    # new interface:
    D <- DAG.empty()
    D <- D+ node("W1", distr = "rbern", prob = plogis(-0.5), order = 1) + 
            node("W2", distr = "rbern", prob = plogis(-0.5 + 0.5*W1), order = 2) + 
            node("W3", distr = "rbern", prob = plogis(-0.5 + 0.7*W1 + 0.3*W2), order = 3) + 
            node("A",  distr = "rbern", prob = plogis(-0.5 - 0.3*W1 - 0.3*W2 - 0.2*W3), order = 4) + 
            node("Y",  distr = "rbern", prob = plogis(-0.1 + 1.2*A + 0.3*W1 + 0.3*W2 + 0.2*W3), order = 5, EFU = TRUE)
    D_WAY <- set.DAG(D)
    D_WAY_0obs <- set.DAG(D, n.test=0)

    #-------------------------------------------------------------
    # Plot this DAG
    #-------------------------------------------------------------
    plotDAG(D_WAY)

    # simulate observed data data from this DAG
    O_dat_WAY <- simobs(D_WAY, n=500, rndseed = 123)
    O_dat_WAY_sim <- sim(D_WAY, n=500, rndseed = 123)
    head(O_dat_WAY, 2)
    head(O_dat_WAY_sim, 2)
    all.equal(O_dat_WAY, O_dat_WAY_sim)

    #-------------------------------------------------------------
    # Defining interventions (actions)
    #-------------------------------------------------------------
    # lets define an action setting treatment to 1
    A1 <- node("A",distr="rbern", prob=1)
    D_WAY <- D_WAY + action("A1", nodes=A1)
    D_WAY_0obs <- D_WAY_0obs + action("A1", nodes=A1)

    # lets define another action setting treatment to 0
    A0 <- node("A",distr="rbern", prob=0)
    D_WAY <- D_WAY + action("A0", nodes=A0)
    D_WAY_0obs <- D_WAY_0obs + action("A0", nodes=A0)

    # selecting actions - its just an intervened DAG
    class(A(D_WAY))
    class(A(D_WAY)[["A1"]])
    class(A(D_WAY)[["A0"]])

    #-------------------------------------------------------------
    # Simulating the counterfactual (full) data
    #-------------------------------------------------------------
    # Simulate full data for all available actions (A(D))
    X_dat1 <- simfull(A(D_WAY), n=500, rndseed = 123)
    head(X_dat1[[1]], 2); head(X_dat1[[2]], 2)

    X_dat1_0obs <- simfull(A(D_WAY), n=0, rndseed = 123)
    X_dat1_0obs

    X_dat1_sim1 <- sim(DAG=D_WAY, actions=c("A1","A0"), n=500, rndseed = 123)
    X_dat1_sim2 <- sim(DAG=D_WAY, actions=A(D_WAY), n=500, rndseed = 123)
    head(X_dat1_sim1[[1]],2); head(X_dat1_sim1[[2]],2)
    head(X_dat1_sim2[[1]],2); head(X_dat1_sim2[[2]],2)

    # Simulate full data for some actions
    X_datA1 <- simfull(A(D_WAY)["A1"], n=500, rndseed = 123)
    X_datA1_sim <- sim(DAG=D_WAY, actions="A1", n=500, rndseed = 123)
    checkIdentical(X_datA1, X_datA1_sim)

    head(X_datA1[[1]],2); 

    #-------------------------------------------------------------
    # Calculating the target parameter: counterfactual expectations
    #-------------------------------------------------------------
    # Counterfactual mean survival at time-point
    D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1")
    res <- eval.target(D_WAY, data=X_dat1)     # using previously simulated full data
    
    # fail when full data was sampled with n=0:
    checkException(eval.target(D_WAY, data=X_dat1_0obs))

    res <- eval.target(D_WAY, n=500, rndseed = 123)  # simulate full data first then evaluate param

    # Contrasts
    D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1-A0")
    res <- eval.target(D_WAY, data=X_dat1)
    res <- eval.target(D_WAY, n=500, rndseed = 123)

    # Ratios
    D_WAY <- set.targetE(D_WAY, outcome="Y", param="A1/A0")
    res <- eval.target(D_WAY, data=X_dat1)
    res <- eval.target(D_WAY, n=500, rndseed = 123)

    #-------------------------------------------------------------
    # categorical node tests 1, 2 & 3
    #-------------------------------------------------------------    
    D_cat <- DAG.empty()
    D_cat <- D_cat + node("W1", distr="rbern", prob=plogis(-0.5), order=1)
    D_cat <- D_cat + node("W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1), order=2)
    D_cat <- D_cat + node("W3", distr="rbern", prob=plogis(-0.5 + 0.7*W1 + 0.3*W2), order=3)
    D_cat <- D_cat + node("Anode", distr="rbern", prob=plogis(-0.5 - 0.3*W1 - 0.3*W2 - 0.2*W3), order=4)

    D_cat_1 <- D_cat + node("Y", distr="rcategor", probs={plogis(-0.1 + 1.2*Anode + 0.3*W1 + 0.3*W2 + 0.2*W3); plogis(-0.5 + 0.7*W1)}, order=5)
    D_cat_2 <- D_cat + node("Y", distr="rcategor", probs={0.3;0.4}, order=5)
    D_cat_3 <- D_cat + node("Y", distr="rcategor", probs={0.2; 0.1; 0.5}, order=5)

    D_cat_1 <- set.DAG(D_cat_1)
    D_cat_2 <- set.DAG(D_cat_2)
    D_cat_3 <- set.DAG(D_cat_3)
 
    A1 <- node("Anode",distr="rbern", prob=1)
    D_cat_1 <- D_cat_1 + action("A1", nodes=A1)
    # lets define another action setting treatment to 0
    A0 <- node("Anode",distr="rbern", prob=0)
    D_cat_1 <- D_cat_1 + action("A0", nodes=A0)

    O_dat_cat <- simobs(D_cat_1, n=500, rndseed = 123)    
    O_dat_cat_sim <- sim(DAG=D_cat_1, n=500, rndseed = 123)
    checkIdentical(O_dat_cat, O_dat_cat_sim)

    X_cat <- simfull(A(D_cat_1), n=500, rndseed = 123)
    X_cat_sim1 <- sim(DAG=D_cat_1, actions=c("A1", "A0"), n=500, rndseed = 123)
    X_cat_sim2 <- sim(actions=A(D_cat_1), n=500, rndseed = 123)
    checkIdentical(X_cat, X_cat_sim1)
    checkIdentical(X_cat, X_cat_sim2)

    X_cat_sim1 <- sim(DAG=D_cat_1, actions=c("A1", "A0"), n=500, rndseed = 123)

    checkException(sim(DAG=D_cat_1, actions=c("A4"), n=500, rndseed = 123))
    #-------------------------------------------------------------
    # uniform node tests 1, 2 & 3
    #-------------------------------------------------------------    
    D_unif <- DAG.empty()
    D_unif <- D_unif + node("W1", distr="rbern", prob=plogis(-0.5), order=1)
    D_unif <- D_unif + node("W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1), order=2)
    D_unif <- D_unif + node("W3", distr="runif", min=plogis(-0.5 + 0.7*W1 + 0.3*W2), max=10, order=3)
    D_unif <- D_unif + node("Anode", distr="rbern", prob=plogis(-0.5 - 0.3*W1 - 0.3*W2 - 0.2*sin(W3)), order=4)

    D_cat_1 <- D_unif + node("Y", distr="rcategor", probs={plogis(-0.1 + 1.2*Anode + 0.3*W1 + 0.3*W2 + 0.2*cos(W3)); plogis(-0.5 + 0.7*W1)}, order=5)
    D_cat_2 <- D_unif + node("Y", distr="rcategor", probs={0.3;0.4}, order=5)
    D_cat_3 <- D_unif + node("Y", distr="rcategor", probs={0.2; 0.1; 0.5}, order=5)

    D_unif <- set.DAG(D_unif)
    D_unif_0obs <- set.DAG(D_unif, n.test=0)
    D_cat_1 <- set.DAG(D_cat_1)
    D_cat_1_0obs <- set.DAG(D_cat_1, n.test=0)
    D_cat_2 <- set.DAG(D_cat_2)
    D_cat_2_0obs <- set.DAG(D_cat_2, n.test=0)
    D_cat_3 <- set.DAG(D_cat_3)
    D_cat_3_0obs <- set.DAG(D_cat_3, n.test=0)


    A1 <- node("Anode",distr="rbern", prob=1)
    D_cat_1 <- D_cat_1 + action("A1", nodes=A1)
    # lets define another action setting treatment to 0
    A0 <- node("Anode",distr="rbern", prob=0)
    D_cat_1 <- D_cat_1 + action("A0", nodes=A0)

    O_dat_cat <- simobs(D_cat_1, n=500, rndseed = 123)
    O_dat_cat_sim <- sim(D_cat_1, n=500, rndseed = 123)
    checkIdentical(O_dat_cat, O_dat_cat_sim)

    X_cat <- simfull(A(D_cat_1), n=500, rndseed = 123)
    X_cat_sim1 <- sim(DAG=D_cat_1, actions=c("A1","A0"), n=500, rndseed = 123)
    X_cat_sim2 <- sim(actions=A(D_cat_1), n=500, rndseed = 123)
    checkIdentical(X_cat, X_cat_sim1)
    checkIdentical(X_cat, X_cat_sim2)

    #-------------------------------------------------------------
    # EXAMPLE 2: longitudinal data
    #-------------------------------------------------------------
    # Define longitudinal DAG for the observed data
    # t_end <- 16
    # OLD FORMAT (STILL WORKS)
    # L2_0 <- node("L2", t=0, distr="rbern", prob=0.05, order=1)
    # L1_0 <- node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    # A1_0 <- node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    # A2_0 <- node("A2", t=0, distr="rbern", prob=1, order=4)
    # Y_0 <-  node("Y",  t=0, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    # L2_t <- node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    # A1_t <- node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    # A2_t <- node("A2", t=1:t_end, distr="rbern", prob=1, order=8+4*(0:(t_end-1)))
    # Y_t <- node( "Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    # lDAG2b <- set.DAG(c(L2_0,L1_0, A1_0, A2_0, Y_0, L2_t, A1_t, A2_t, Y_t))
    
    # new interface:
    t_end <- 16
    D <- DAG.empty()
    D <- D+ node("L2", t=0, distr="rbern", prob=0.05, order=1) +
            node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2) +
            node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3) +
            node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE) +
            node("Y",  t=0, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE) + 
            node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1))) + 
            node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1))) + 
            node("A2", t=1:t_end, distr="rbern", prob=0, order=8+4*(0:(t_end-1)), EFU=TRUE) + 
            node( "Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    lDAG2b <- set.DAG(D)

    lDAG2b_0obs <- set.DAG(D, n.test = 0)
    
    # testing data sim for empty returns. Can't evalute this one, since the error has to do with the way rowSums is called
    checkException(Odatsim.empty <- sim(lDAG2b, n = 0, rndseed = 1))
    # checkEquals(nrow(Odatsim.empty),0)
    # checkIdentical(names(dat), names(Odatsim.empty))
  
    # testing data sim with 1 obs:
    Odatsim.1obs <- sim(lDAG2b, n = 1, rndseed = 1)
    checkEquals(nrow(Odatsim.1obs),1)
    #-------------------------------------------------------------
    # Plot the observed DAG
    #-------------------------------------------------------------
    # plotDAG(lDAG2b)

    #-------------------------------------------------------------
    # Adding dynamic actions (indexed by a real-valued parameter)
    #-------------------------------------------------------------
    # Define intervention nodes
    act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    actionnodes <- c(act_t0_theta, act_tp_theta)
    D <- lDAG2b + action("A1_th0", nodes=actionnodes, theta=0)
    D <- D + action("A1_th1", nodes=actionnodes, theta=1)

    # Can add more changes to the same intervention (action)
    # .... need to do example for this....
    # D <- lDAG2b + action("A1_th0", nodes=actionnodes, theta=0)

    # Can also fully or partially overwrite existing action
    # D <- D + action("A1_th0", nodes=actionnodes, theta=1)
    # D <- D + action("A1_th0", nodes=actionnodes, theta=0)

    # Can select and subset actions using function A(D):
    actions <- A(D) # will select all available actions
    action_th0 <- A(D)["A1_th0"] # will select action indexed by theta=0

    #-------------------------------------------------------------
    # Adding actions (indexed by time-varying real valued vector)
    #-------------------------------------------------------------
    # Supppose now the intervention is indexed by some real-value that varies in time? Can we still define such an action? Yes
    act_t0_theta_t <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta[t],1,0))
    act_tp_theta_t <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta[t],1,0)))
    actionnodes_t <- c(act_t0_theta_t, act_tp_theta_t)

    # Define time-varying theta
    # theta <- seq(0, 1, length.out=(t_end+1))
    # Define action the same way
    D <- lDAG2b + action("A1_th0", nodes=actionnodes_t, theta=rep(0,17))
    # replace with D <- lDAG2b + action("A1_th0", nodes=actionnodes, theta=theta)
    D <- D + action("A1_th1", nodes=actionnodes_t, theta=rep(1,17))
    # replace with D <- D + action("A1_th1", nodes=actionnodes, theta=theta)

    #-------------------------------------------------------------
    # Plot the counterfactual (intervened) DAG (marking the intervention nodes with red)
    #-------------------------------------------------------------
    # plotDAG(A(D)[[1]])

    #-------------------------------------------------------------
    # Simulating data (observed and counterfactual (full))
    #-------------------------------------------------------------
    # Simulate observed data
    t1 <- system.time(O_dat <- simobs(D, n=500, rndseed = 123))
    print(t1)
    # for 50K
    #  user  system elapsed 
    # 2.740   0.521   3.250

    t1.long <- system.time({
      O_dat <- simobs(D, n = 500, rndseed = 123)
      dat.long1 <- DF.to.long(O_dat)
    })
    print(t1.long)
    # for 50K
    #  user  system elapsed
    # 8.932   1.058   9.954

    t2.long <- system.time(O_dat_long <- simobs(D, n=500, wide=FALSE, rndseed = 123)) # observed long format:)
    print(t2.long)
    # for 50K:
    #  user  system elapsed
    # 3.606   0.798   4.397

    # Simulate full data for given actions (A(D))
    X_dat <- simfull(A(D), n=500, rndseed = 123)
    # X_dat <- simfull(A(D), n=10000, rndseed = 123)
    X_dat_long <- simfull(A(D), n=500, wide=FALSE, rndseed = 123)  # full data in long format:
    # X_dat_long <- simfull(A(D), n=10000, wide=FALSE, rndseed = 123)  # full data in long format:

    X_dat_th0 <- simfull(A(D)["A1_th0"], n=500, rndseed = 123)
    # X_dat_th0 <- simfull(A(D)["A1_th0"], n=10000, rndseed = 123)
    head(X_dat_th0[[1]]); 
    checkException(X_dat_th0[[2]])

    #-------------------------------------------------------------
    # Target parameter: counterfactual means
    #-------------------------------------------------------------
    X_dat_big <- simfull(A(D), n=500, rndseed = 123)
    # X_dat_big <- simfull(A(D), n=1000000, rndseed = 123)
    # EXAMPLE 1: Counterfactual mean survival at time-point
    D <- set.targetE(D, outcome="Y", t=11, param="A1_th0")
    eval.target(D, data=X_dat) # use full data
    eval.target(D, n=500, rndseed = 123) # sample full data and then evaluate
    eval.target(D, n=500, actions="A1_th0", rndseed = 123)
    checkException(eval.target(D, n=500, actions="A1_th1", rndseed = 123))


    # EXAMPLE 2: Vector of counterfactual mean survival over time
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1")
    eval.target(D, data=X_dat)$res

    D <- set.targetE(D, outcome="Y", t=0:5, param="A1_th1")
    eval.target(D, data=X_dat)$res

    # Some survival plots
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1"); surv_th1 <- 1-eval.target(D, data=X_dat_big)$res
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th0"); surv_th0 <- 1-eval.target(D, data=X_dat_big)$res
    # if (FALSE) {
        plotSurvEst(surv=list(d_theta1 = surv_th1, d_theta0 = surv_th0), xindx=1:17, ylab="Counterfactual Survival, P(T>t)", ylim=c(0.75,1.0))
    # }

    # EXAMPLE 3: Contrasts
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1-A1_th0")
    eval.target(D, data=X_dat)$res

    # EXAMPLE 4: Ratios
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th0/A1_th1")
    eval.target(D, data=X_dat)$res

    #-------------------------------------------------------------
    # Target parameter: modelling survival with MSM
    #-------------------------------------------------------------
    # Suppose we are interested in describing the counterfactual survival curve as a projection on the following working model:
    msm.form <- "Y ~ theta + t + I(theta*t)"
    D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=FALSE)
    X_dat_long <- simfull(A(D), n=500, wide=FALSE, LTCF="Y", rndseed = 123) # simulate some long format full data
    # head(X_dat_long)

    # option one (supply simulated full data)
    MSMres <- eval.target(D, data=X_dat_long)
    MSMres$coef
    # option two (have the function simulate full data itself)
    MSMres <- eval.target(D, n=500, rndseed = 123)
    MSMres$coef
    # >     MSMres$coef
    #  (Intercept)        theta            t I(theta * t) 
    #  -3.71764659   0.71769800   0.15817903  -0.02957071 
    MSMres <- eval.target(D, n=500, actions="A1_th1", rndseed = 123)
    MSMres$coef
     # (Intercept)        theta            t I(theta * t) 
     #  -2.9222734           NA    0.1398531           NA 

    # plotting survival
    S_th0 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(0,17), t=0:16), type="response")
    S_th1 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(1,17), t=0:16), type="response")
    # if (FALSE) {
        plotSurvEst(surv=list(MSM_theta1 = S_th1, MSM_theta0 = S_th0), xindx=1:17, ylab="MSM Survival, P(T>t)", ylim=c(0.75,1.0))
    # }

    #-------------------------------------------------------------
    # Target parameter: modelling the descrete hazard with MSM
    #-------------------------------------------------------------
    msm.form <- "Y ~ theta + t + I(theta*t)"
    D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=TRUE)
    X_dat_long <- simfull(A(D), n=500, wide=FALSE, rndseed = 123) # simulate some long format full data

    # option one (supply simulated full data)
    MSMres <- eval.target(D, data=X_dat_long)
    MSMres$coef
    # option two (have the function simulate full data itself)
    MSMres <- eval.target(D, n=500, rndseed = 123)
    MSMres$coef
    # >     MSMres$coef
    #  (Intercept)        theta            t I(theta * t) 
    #  -4.65091943   0.64768542   0.05180731  -0.04835746 

    # plotting the hazard
    h_th0 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(0,17), t=0:16), type="response")
    h_th1 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(1,17), t=0:16), type="response")
    # if (FALSE) {
        plotSurvEst(surv=list(MSM_theta1 = h_th1, MSM_theta0 = h_th0), xindx=1:17, ylab="1 - MSM predicted hazard, P(T>t)", ylim=c(0.95,1.0))
    # }
    # Converting hazard to survival
    Surv_h_th0 <- cumprod(h_th0)
    Surv_h_th1 <- cumprod(h_th1)
    # if (FALSE) {
        plotSurvEst(surv=list(MSM_theta1 = Surv_h_th1, MSM_theta0 = Surv_h_th0), xindx=1:17, ylab="P(T>t) from hazard", ylim=c(0.75,1.0))
    # }
    #-------------------------------------------------------------
    # Estimation with lTMLE package: node means
    #-------------------------------------------------------------
    # O_dat <- simobs(D, n=100, rndseed = 123)

    # # Suppose we want to now evalute some estimator of the node mean target parameter, based on the simulated observed data
    # # D <- set.targetE(D, outcome="Y", t=0:10, param="A1_th1")
    # # D <- set.targetE(D, outcome="Y", t=10, param="A1_th1")

    # D <- set.targetE(D, outcome="Y", t=10, param="A1_th0")
    # ltmle_res <- est.targetE(D, O_dat, Aname="A1", Cname="A2", Lnames="L2")
    # ltmle_res$tmleres
    # names(ltmle_res)
 
    #-------------------------------------------------------------
    # Estimation with lTMLE package: MSMs
    #-------------------------------------------------------------
    O_dat <- simobs(D, n=100, rndseed = 123)
    # Suppose we want to now evalute some estimator of the MSM target parameter, based on the simulated observed data
    # msm.form <- "Y ~ theta + t + I(theta*t)"
    # D <- set.targetMSM(D, outcome="Y", t=0:10, form=msm.form, family="binomial", hazard=FALSE)
    # ltmleMSMres <- est.targetMSM(D, O_dat, Aname="A1", Cname="A2", Lnames="L2")
    # ltmleMSMres$tmleres

    # names(ltmleMSMres)
    # names(ltmleMSMres$lTMLEobj)

    # ltmleMSMres$lTMLEobj$msm
    # library(ltmle)
    # predict(ltmleMSMres$lTMLEobj$msm)

    #   # $tmleres
    #   # Estimator:  tmle 
    #   #              Estimate Std. Error  CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)  -6.84907    0.65397 -8.13083   -5.567  < 2e-16 ***
    #   # theta         2.25943    1.30523 -0.29876    4.818   0.0834 .  
    #   # t             0.53219    0.02885  0.47564    0.589  < 2e-16 ***
    #   # I(theta * t) -0.46401    0.10573 -0.67123   -0.257 1.14e-05 ***
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #   # $iptwres
    #   # Estimator:  iptw 
    #   #              Estimate Std. Error  CI 2.5% CI 97.5% p-value    
    #   # (Intercept)  -6.85401    0.65114 -8.13023   -5.578  <2e-16 ***
    #   # theta         2.43581    1.20471  0.07462    4.797  0.0432 *  
    #   # t             0.53197    0.02848  0.47616    0.588  <2e-16 ***
    #   # I(theta * t) -0.50410    0.05090 -0.60386   -0.404  <2e-16 ***
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #-------------------------------------------------------------
    # Checking ltmleMSM is consistent (close enough to true MSM coefficients for large enough samples) for t=0:10
    #-------------------------------------------------------------
    # msm.form <- "Y ~ theta + t + I(theta*t)"
    # D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=FALSE)
    # MSMres <- eval.target(D, n=50000, rndseed = 123)
    # MSMres$coef
    #  # (Intercept)        theta            t I(theta * t) 
    #  # -3.52595796   0.54415675   0.15189648  -0.01645792 
    # O_dat <- simobs(D, n=20000, rndseed = 123)
    # est.targetMSM(D, O_dat, Aname="A1", Cname="A2", Lnames="L2", package="ltmle")
    #   #N=20K
    #   # $tmleres
    #   # Estimator:  tmle 
    #   #               Estimate Std. Error   CI 2.5% CI 97.5% p-value    
    #   # (Intercept)  -3.534608   0.078500 -3.688466   -3.381 < 2e-16 ***
    #   # theta         0.491233   0.105749  0.283970    0.698 3.4e-06 ***
    #   # t             0.152215   0.004809  0.142789    0.162 < 2e-16 ***
    #   # I(theta * t) -0.012098   0.006648 -0.025129    0.001  0.0688 .  
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #   # $iptwres
    #   # Estimator:  iptw 
    #   #               Estimate Std. Error   CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)  -3.534774   0.078719 -3.689061   -3.380  < 2e-16 ***
    #   # theta         0.489920   0.106853  0.280492    0.699 4.54e-06 ***
    #   # t             0.152208   0.004812  0.142777    0.162  < 2e-16 ***
    #   # I(theta * t) -0.012251   0.006681 -0.025345    0.001   0.0667 .  
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    # msm.form <- "Y ~ theta + t + I(theta*t)"
    # D <- set.targetMSM(D, outcome="Y", t=0:10, form=msm.form, family="binomial", hazard=FALSE)
    # MSMres <- eval.target(D, n=50000, rndseed = 123)
    # MSMres$coef
    #   #   (Intercept)         theta             t  I(theta * t) 
    #   # -3.8872087068  0.4723120847  0.2148340943 -0.0002968336
    # O_dat <- simobs(D, n=20000, rndseed = 123)
    # est.targetMSM(D, O_dat, Aname="A1", Cname="A2", Lnames="L2", package="ltmle")
    #   #N=20K
    #   # $tmleres
    #   # Estimator:  tmle 
    #   #               Estimate Std. Error   CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)  -3.908667   0.091928 -4.088841   -3.728  < 2e-16 ***
    #   # theta         0.426927   0.118161  0.195336    0.659 0.000303 ***
    #   # t             0.217377   0.008721  0.200285    0.234  < 2e-16 ***
    #   # I(theta * t)  0.002016   0.011732 -0.020978    0.025 0.863557    
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #   # $iptwres
    #   # Estimator:  iptw 
    #   #                Estimate Std. Error    CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)  -3.9087712  0.0922606 -4.0895986   -3.728  < 2e-16 ***
    #   # theta         0.4310184  0.1191841  0.1974218    0.665 0.000299 ***
    #   # t             0.2173596  0.0087321  0.2002449    0.234  < 2e-16 ***
    #   # I(theta * t)  0.0009354  0.0117738 -0.0221409    0.024 0.936678    
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #----------------------------------------------------
    # CHECKING NP TARGET STILL MATCHES:
    #----------------------------------------------------
    # t_end <- 16
    # D <- DAG.empty()
    # D <- D + node("L2", t=0, distr="rbern", prob=0.05, order=1)
    # D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    # D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    # D <- D + node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE)
    # D <- D + node("Y",  t=0, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    # D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    # D <- D + node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    # D <- D + node("A2", t=1:t_end, distr="rbern", prob=0, order=8+4*(0:(t_end-1)), EFU=TRUE)
    # D <- D + node( "Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    # lDAG2b <- set.DAG(D)
    # act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    # act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    # actionnodes <- c(act_t0_theta, act_tp_theta)
    # D <- lDAG2b + action("A1_th0", nodes=actionnodes, theta=0)
    # D <- D + action("A1_th1", nodes=actionnodes, theta=1)

    # D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1-A1_th0")
    # timeNPsurv <- system.time(psi_RDsb <- eval.target(D, n=1000000, rndseed = 123))
     #   user  system elapsed 
     # 66.845  12.031  79.217
    # psi_RDsb
    # $res
    #  Diff_Y_0  Diff_Y_1  Diff_Y_2  Diff_Y_3  Diff_Y_4  Diff_Y_5  Diff_Y_6  Diff_Y_7  Diff_Y_8  Diff_Y_9 Diff_Y_10 Diff_Y_11 Diff_Y_12 Diff_Y_13 Diff_Y_14 Diff_Y_15 
    #  0.000000  0.005233  0.014005  0.024868  0.035301  0.043906  0.049829  0.053323  0.054445  0.054489  0.053700  0.052621  0.051190  0.049857  0.048366  0.046831 
    # Diff_Y_16 
    #  0.045756 
    # $call
    # set.targetE(DAG = D, outcome = "Y", t = 0:16, param = "A1_th1-A1_th0")

    # OLD RDs (NOT EXACTLY MATCHING BECAUSE THE SAMPLING SCHEME CHANGED)
    # Diff_Y_0  Diff_Y_1  Diff_Y_2  Diff_Y_3  Diff_Y_4  Diff_Y_5  Diff_Y_6  Diff_Y_7 
    # 0.000000  0.005328  0.014377  0.025153  0.035778  0.044602  0.050341  0.053849 
    # Diff_Y_8  Diff_Y_9 Diff_Y_10 Diff_Y_11 Diff_Y_12 Diff_Y_13 Diff_Y_14 Diff_Y_15 
    # 0.055009  0.054711  0.053982  0.052659  0.051264  0.049703  0.048757  0.047022 
    # Diff_Y_16 
    # 0.045849 

    #----------------------------------------------------
    # CHECKING NP TARGET STILL MATCHES with node & rbern:
    #----------------------------------------------------
    # t_end <- 16
    # D <- DAG.empty()
    # D <- D + node("L2", t=0, distr="rbern", prob=0.05, order=1)
    # D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    # D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    # D <- D + node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE)
    # D <- D + node("Y",  t=0, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    # D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    # D <- D + node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    # D <- D + node("A2", t=1:t_end, distr="rbern", prob=0, order=8+4*(0:(t_end-1)), EFU=TRUE)
    # D <- D + node( "Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    # lDAG2b <- set.DAG(D)
    # act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    # act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    # actionnodes <- c(act_t0_theta, act_tp_theta)
    # D <- lDAG2b + action("A1_th0", nodes=actionnodes, theta=0)
    # D <- D + action("A1_th1", nodes=actionnodes, theta=1)

    # D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1-A1_th0")
    # timeNPsurv <- system.time(psi_RDsb2 <- eval.target(D, n=1000000, rndseed = 123))
    # timeNPsurv
     #   user  system elapsed 
     # 61.668  13.720  74.974 
    # psi_RDsb2
    # $res
    #  Diff_Y_0  Diff_Y_1  Diff_Y_2  Diff_Y_3  Diff_Y_4  Diff_Y_5  Diff_Y_6  Diff_Y_7  Diff_Y_8  Diff_Y_9 Diff_Y_10 Diff_Y_11 Diff_Y_12 Diff_Y_13 Diff_Y_14 Diff_Y_15 
    #  0.000000  0.005233  0.014005  0.024868  0.035301  0.043906  0.049829  0.053323  0.054445  0.054489  0.053700  0.052621  0.051190  0.049857  0.048366  0.046831 
    # Diff_Y_16 
    #  0.045756 
    # $call
    # set.targetE(DAG = D, outcome = "Y", t = 0:16, param = "A1_th1-A1_th0")

    #-------------------------------------------------------------
    # add.action: altarnative way of adding actions to DAG object (uses underlying setAction)
    #-------------------------------------------------------------
    # APPROACH 1: saving intervention in a DAG as "action" with constant attribute theta
    t_end <- 16
    act_t0_theta <- node(name="A1",t=0, distr="rbern", prob=ifelse(L2[0]>= theta,1,0))
    act_tp_theta <- node(name="A1",t=1:eval(t_end), distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t]>= theta,1,0)))
    actionnodes <- c(act_t0_theta, act_tp_theta)

    D <- add.action(DAG=lDAG2b, name="A1_th0", nodes=actionnodes, theta=0)
    D <- add.action(DAG=D, name="A1_th1", nodes=actionnodes, theta=1)
    # D <- add.action(DAG=D, actname="A1_th0", nodes=actionnodes, theta=1)  # making sure attributes can be modified and actions overwritten:
    t_full_dag2b <- system.time(fulldf_DAG_2b <- simfull(attributes(D)$actions, n=100, rndseed = 123))

    # APPROACH 2: saving intervention in a DAG as "action" with time-varying attribute theta[t]
    act_t0_theta <- node(name="A1",t=0, distr="rbern", prob=ifelse(L2[0]>= theta[t],1,0))
    act_tp_theta <- node(name="A1",t=1:eval(t_end), distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t]>= theta[t],1,0)))
    actionnodes <- c(act_t0_theta, act_tp_theta)

    D <- add.action(DAG=lDAG2b, name="A1_th0", nodes=actionnodes, theta=rep(0, (t_end+1)))
    D <- add.action(DAG=D, name="A1_th1", nodes=actionnodes, theta=rep(1, (t_end+1)))
    # D <- add.action(DAG=D, actname="A1_th0", nodes=actionnodes, theta=rep(1, (t_end+1))) # check that existing action can be always overwritten/added to
    t_full_dag2b <- system.time(fulldf_DAG_2b <- simfull(attributes(D)$actions, n=100, rndseed = 123))

    #-------------------------------------------------------------
    # setAction test (this constructor is now hidden)
    #-------------------------------------------------------------
    # APPROACH 1: saving regimen as a separeate obj / a function of constant attribute (theta)
    act_t0_theta <- node(name="A1",t=0, distr="rbern", prob=ifelse(L2[0]>= theta,1,0))
    act_tp_theta <- node(name="A1",t=1:eval(t_end), distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t]>= theta,1,0)))
    actionnodes <- c(act_t0_theta, act_tp_theta)
    
    # action_1_DAG_2b <- simcausal:::setAction(lDAG2b, actionnodes, attr=list(theta=0))
    action_1_DAG_2b <- simcausal:::setAction(actname="A1_th0", inputDAG=lDAG2b, actnodes=actionnodes, attr=list(theta=0))
    # action_2_DAG_2b <- simcausal:::setAction(lDAG2b, actionnodes, attr=list(theta=1))
    action_2_DAG_2b <- simcausal:::setAction(actname="A1_th1", inputDAG=lDAG2b, actnodes=actionnodes, attr=list(theta=1))

    actions_DAG_2b <- list(A1_th0=action_1_DAG_2b, A1_th1=action_2_DAG_2b)

    # APPROACH 2: saving regimen as a separeate obj / a function of time varying attribute (theta)
    act_t0_theta <- node(name="A1",t=0, distr="rbern", prob=ifelse(L2[0]>= theta[0],1,0))
    act_tp_theta <- node(name="A1",t=1:eval(t_end), distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t]>= theta[t],1,0)))
    actionnodes <- c(act_t0_theta, act_tp_theta)

    # action_1_DAG_2b <- simcausal:::setAction(lDAG2b, actionnodes, attr=list(theta=rep(0, (t_end+1))))
    action_1_DAG_2b <- simcausal:::setAction(actname="A1_th0", inputDAG=lDAG2b, actnodes=actionnodes, attr=list(theta=rep(0, (t_end+1))))
    # action_2_DAG_2b <- simcausal:::setAction(lDAG2b, actionnodes, attr=list(theta=rep(1, (t_end+1))))
    action_2_DAG_2b <- simcausal:::setAction(actname="A1_th1", inputDAG=lDAG2b, actnodes=actionnodes, attr=list(theta=rep(1, (t_end+1))))
    
    actions_DAG_2b <- list(A1_th0=action_1_DAG_2b, A1_th1=action_2_DAG_2b)
}



test.longparse <- function() {
  library(simcausal)
  # Fixing the bug in modify_call() for long expressions with deparse arg set to output 1 line of text
  # Error in parse(text = deparse(x, width.cutoff = 500, nlines = 1)) : 

  # expr <- rep(1/55, 55)
  # parse(text = deparse(expr, width.cutoff = 500))[[1]]
  # x <- parse(text = deparse(x, width.cutoff = 500))[[1]]

  D <- DAG.empty()
  D2 <- D + node('group',
                 distr = 'rcategor.int',
                 probs = rep(1/55, 55))
  D2 <- set.DAG(D2)
  datD2 <- sim(D2, n = 100, rndseed = 123)


  D3 <- D + node('group',
                 distr = 'rcategor.int',
                 probs = rep(1/1933, 1933))
  D3 <- set.DAG(D3)
  datD3 <- sim(D3, n = 100, rndseed = 123)
  datD3

  # testing that rep with rcategor.int returns an error (rep functionality not implemented yet):
  D.error <- D + node('A',
                 distr = 'rconst',
                 const = 1/3)
  D.error <- D.error + node('group',
                  distr = 'rcategor.int',
                  probs = rep(A, 3))
  checkException(set.DAG(D.error))

  # using c instead of rep with rcategor.int works as cbind(A,A,A):
  D.noerror <- D + node('A',
                 distr = 'rconst',
                 const = 1/3)
  D.noerror <- D.noerror + node('group',
                  distr = 'rcategor.int',
                  probs = c(A, A, A))
  D.noerror <- set.DAG(D.noerror)
  sim(D.noerror, n = 100)
}


test.distr <- function() {
  distr.list()
  rdistr.template(n = 100, arg1 = 0.5, arg2 = 0.3)
  rdistr.template(n = 100, arg1 = rep(0.5, 100), arg2 = 0.3)
  checkException(rdistr.template(n = 100, arg1 = rep(0.5, 100), arg2 = rep(0.3, 50)))

}

test.condrcategor <- function() {
  #-------------------------------------------------------------
  # BUG WITH CONDITIONAL CATEGORICAL DISTRIBUTIONS (e.g., rcategor.int)
  # probs arg should be evaluated to a matrix of probabilities, instead its a vector of length n
  #-------------------------------------------------------------
  # library(simcausal)
  # THIS WORKS FINE, SINCE call to 'c' fun gets replaced with cbind:
  D <- DAG.empty()
  D <- D + node("W", distr = "rbern", prob = 0.3)
  D <- D + node("Cat3", distr = "rcategor.int",
                probs = (W == 0)*c(0.7,0.1,0.2) + (W==1)*c(0.2,0.1,0.7))
  Dset1 <- set.DAG(D)

  # THIS wasn't working, but was fixed by adding to parser a new if (quote(structure)):
  D <- DAG.empty()
  D <- D + node("W", distr = "rbern", prob = 0.3)
  catprob.W0 <- cbind(0.7,0.1,0.2); catprob.W1 <- cbind(0.2,0.1,0.7)
  D <- D + node("Cat3", distr = "rcategor.int",
                probs = (W==0)*.(catprob.W0) + (W==1)*.(catprob.W1))
  Dset2 <- set.DAG(D)

  # THIS still doesn't work, since catprob.W0 gets evaluated INSIDE eval, parser sees catprob.W0 as a name, hence can't modify what's inside it
  D <- DAG.empty()
  D <- D + node("W", distr = "rbern", prob = 0.3)
  catprob.W0 <- cbind(0.7,0.1,0.2); catprob.W1 <- cbind(0.2,0.1,0.7)
  D <- D + node("Cat3", distr = "rcategor.int",
                probs = (W==0)*catprob.W0 + (W==1)*catprob.W1)
  # Dset3 <- set.DAG(D)
  # catprob.W0 <- c(0.7,0.1,0.2); catprob.W1 <- c(0.2,0.1,0.7)
  # D <- D + node("Cat3", distr = "rcategor.int",
  #               probs = ifelse(W==0, catprob.W0, catprob.W1))
  # D <- D + node("Cat3", distr = "rcategor.int",
  #               probs = {if (W==0) {catprob.W0} else {catprob.W1}} )
  # catprob.W0 <- matrix(c(0.7,0.1,0.2), nrow = 10, ncol = 3, byrow=TRUE)
  # catprob.W1 <- matrix(c(0.2,0.1,0.7), nrow = 10, ncol = 3, byrow=TRUE)
  # Dset <- set.DAG(D)

  dat1a <- sim(Dset1, n=100, rndseed = 1234)
  dat1b <- simcausal:::simFromDAG(DAG = Dset1, Nsamp = 100, rndseed = 1234)
  all.equal(dat1a, dat1b)

  dat2 <- simcausal:::simFromDAG(DAG = Dset2, Nsamp = 100, rndseed = 1234)
  all.equal(dat1a, dat2)

  node_evaluator <- simcausal:::Define_sVar$new() # netind_cl = NULL
  # node_evaluator$set.user.env(attr(Dset2, "user.env"))
  eval_expr_res <- node_evaluator$eval.nodeforms(cur.node = Dset2[["Cat3"]], data.df = dat2[,c("ID","W")])
  eval_expr_res[[1]]$par.nodes
  catprobs <- eval_expr_res[[1]]$evaled_expr
  checkTrue(is.matrix(catprobs))

}


test.bugfixes <- function() {  
    #-------------------------------------------------------------
    # BUG (TO DO):
    # Should be able to handle character strings for node formulas (doesn't process them at all currently)
    # Add to node: if (is.character(x)) {x} else {deparse(x)}
      # D <- DAG.empty()
      # D <- D+ node("L2",distr="rbern",prob=0.05) +
      #         node("L2",t=0:16,distr="rbern",prob=0.05) +
      #         node("L1",t=0,distr="rbern",prob=ifelse(L2[0]==1,0.5,0.1)) +
      #         node("A1",t=0,distr="rbern",prob="ifelse(L1[0]==1 & L2[0]==0,0.5,
      #                                                          ifelse(L1[0]==0 & L2[0]==0,0.1,
      #                                                            ifelse(L1[0]==1 & L2[0]==1,0.9,0.5)))") +
      #         node("A2",t=0,distr="rbern",prob=0,order=4,EFU=TRUE)
      # D <- set.DAG(D)
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # BUG (TO DO): SEE NEW PARSER FOR TMLENET. Capture user envir and pass it as enclos = ..
      # # If node name coincides with internal function name (e.g., "A"), it wont be picked up as a parent inside the node formula
      # # 1) TO fix this need to test for parenthood INSIDE the simulatd data.frame environment
      # # or 2) Take ALL formula atoms (+, -, *, etc) and select only those that match to existing node names
      # D <- DAG.empty()
      # D <- D+node("W1",distr="rbern",prob=plogis(-0.5),order=1)
      # D <- D+node("W2",distr="rbern",prob=plogis(-0.5+0.5*W1),order=2)
      # D <- D+node("W3",distr="rbern",prob=plogis(-0.5+0.7*W1+0.3*W2),order=3)
      # D <- D+node("A",distr="rbern",prob=plogis(-0.5-0.3*W1-0.3*W2-0.2*W3),order=4)
      # D <- D+node("Y",distr="rbern",prob=plogis(-0.1+1.2*A+0.3*W1+0.3*W2+0.2*W3),order=5,EFU=TRUE)
      # D_WAY <- set.DAG(D)
      # plotDAG(D_WAY)  
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # TO DO in eval.target: 
      # "data" and "actions" arguments currently do not work in agreement, 
      # i.e. when data is specified, actions argument is completely ignored.
      # CHANGE TO: when "actions" not missing, should subset data by action names
    #-------------------------------------------------------------        

    #-------------------------------------------------------------
    # TO DO in plotDAG: 
      # add actions argument for plotting actions saved in D (rather than passing DAG.action)
      # print action name(s) on the DAG
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    ## TO DO: check if set.targetE with race/categorical works
    #-------------------------------------------------------------    

    #--------------------------------------------------------------------------------
    # TO DO:
    # DOESN'T WORK WITHIN R CMD check ENVIRONMENT, BUT WORKS FINE WHEN RAN MANUALLY:
      # showing how to define custom node formula functions (need to add "customfun" as an argument to set.DAG):
      # customfun <- function(arg, lambda) {
      #   res <- ifelse(arg==1,lambda,0.1)
      #   res
      # }
      # D <- DAG.empty()
      # D <- D + node("W1", distr="rbern", prob=0.05)
      # D <- D + node("W2", distr="rbern", prob=customfun(W1,0.5))
      # D <- D + node("W3", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
      # D1d <- set.DAG(D, vecfun=c("customfun"))
      # vecfun.reset()
      # vecfun.remove("N.sporadic.4_vec")
      # vecfun.add(c("custom2", "N.sporadic.4_vec"))
      # vecfun.print()

      # sim1d <- simobs(D1d, n=200, rndseed=1)
      # checkIdentical(as.matrix(sim1a), as.matrix(sim1d))
    #--------------------------------------------------------------------------------
    #--------------------------------------------------------------------------------
    # TO DO :
    # DOESN'T WORK WITHIN R CMD check ENVIRONMENT, BUT WORKS FINE WHEN RAN MANUALLY:  
      # showing how to define several DAGs indexed by different values passed to custom node formula functions:
      # customfun <- function(arg, lambda) {
      #   res <- ifelse(arg==1,lambda,0.1)
      #   res
      # }
      # lambdas <- c(0.5, 0.7, 0.9)
      # D_list <- list()
      # for (lambda in lambdas) {
      #     print(lambda)
      #     D <- DAG.empty()
      #     D <- D + node("W1", distr="rbern", prob=0.05)
      #     D <- D + node("W2", distr="rbern", prob=customfun(W1, .(lambda)))
      #     D <- D + node("W3", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
      #     D <- set.DAG(D, vecfun=c("customfun"))
      #     D_list <- append(D_list, list(D))
      # }
      # sim_list <- lapply(D_list, simobs, n=200, rndseed=1)
    # --------------------------------------------------------------------------------

    library(simcausal)

    #-------------------------------------------------------------
    # BUG FIX: variable from the user-env (rconst) that was not evaluable inside the DAG (const node)
    #-------------------------------------------------------------
    rconst <- 0.5
    D <- DAG.empty()
    D <- D+ node("const", distr = "rbern", prob = rconst) + 
            node("W1", distr = "rbern", prob = plogis(-0.5)) + 
            node("W2", distr = "rbern", prob = plogis(-0.5 + 0.5*W1)) + 
            node("W3", distr = "rbern", prob = plogis(-0.5 + 0.7*W1 + 0.3*W2)) + 
            node("A", distr = "rbern", prob = plogis(-0.5 - 0.3*W1 - 0.3*W2 - 0.2*W3)) + 
            node("Y", distr = "rbern", prob = plogis(-0.1 + 1.2*A + 0.3*W1 + 0.3*W2 + 0.2*W3), EFU=TRUE)
    D_WAY <- set.DAG(D)
    sim(D_WAY, n=100)

    #-------------------------------------------------------------
    # BUG (FIXED): 
      # adding time-varying node with the same name as a non time-varying (baseline) doesn't overwrite the older, 
      # but adds another node. This can lead to some unexpected behavior in the future.
      # 1 type of error:
      D <- DAG.empty()
      D <- D+ node("L2",distr="rbern",prob=0.05) +
              node("L2",t=0:16,distr="rbern",prob=0.05) +
              node("L1",t=0,distr="rbern",prob=ifelse(L2[0]==1,0.5,0.1)) +
              node("A1",t=0,distr="rbern",prob=ifelse(L1[0]==1 & L2[0]==0,0.5,
                                                 ifelse(L1[0]==0 & L2[0]==0,0.1,
                                                   ifelse(L1[0]==1 & L2[0]==1,0.9,0.5)))) +
              node("A2",t=0,distr="rbern",prob=0,order=4,EFU=TRUE)
      D <- set.DAG(D)
      # 2nd type of error:
      D <- DAG.empty()
      D <- D+ node("L2",distr="rbern",prob=0.05) +
              node("L2",t=0:16,distr="rbern",prob=0.05) +
              node("L1",t=0:16,distr="rbern",prob=ifelse(L2[0]==1,0.5,0.1))
      D <- set.DAG(D)

    #-------------------------------------------------------------
    # BUG (FIXED): 
      # adding time-varying action attribute name, the existing non time-varying action attribute is NOT OVERWRITTEN -> This can lead to some unexpected behavior in the future.
    # BUG (FIXED): 
      # adding generic action attribute name, the existing time-varying action attribute is NOT OVERWRITTEN -> This can lead to some unexpected behavior in the future.
    # BUG (FIXED):  
      # attribute is being overwritten, old values are not overwritten in "attrs" list:
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Add a warning when overwriting existing action nodes 
      D <- DAG.empty()
      D <- D+node("W1",distr="rbern",prob=plogis(-0.5),order=1)
      D <- D+node("W2",distr="rbern",prob=plogis(-0.5+0.5*W1),order=2)
      D <- D+node("W3",distr="rbern",prob=plogis(-0.5+0.7*W1+0.3*W2),order=3)
      D <- D+node("Anode",distr="rbern",prob=plogis(-0.5-0.3*W1-0.3*W2-0.2*W3),order=4)
      D <- D+node("Y",distr="rbern",prob=plogis(-0.1+1.2*Anode+0.3*W1+0.3*W2+0.2*W3),order=5,EFU=TRUE)
      D_WAY <- set.DAG(D)
      # plotDAG(D_WAY)
      A1 <- node("Anode",distr="rbern",prob=1)
      D_WAY <- D_WAY+action("A1",nodes=A1)
      A0 <- node("Anode",distr="rbern",prob=0)
      D_WAY <- D_WAY+action("A0",nodes=A0)
      # simfull(actions=A(D_WAY), n=100)
      # class(A(D_WAY))
      # class(A(D_WAY)[[1]])
      # class(A(D_WAY))
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # BUG (FIXED):
      # the issue with calling eval.target for actions as a character
      # Modified to work with action names as well
    #-------------------------------------------------------------        

    #-------------------------------------------------------------
    # BUG (FIXED):
      # Only a warning when calling set.DAG on empty DAG, no error
      D <- DAG.empty()
      set.DAG(D)
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # BUG (FIXED):
      # Gives an error when trying to add nodes after set.DAG was called
      D <- DAG.empty()
      D <- D+node("W1",distr="rbern",prob=plogis(-0.5),order=1)
      D <- set.DAG(D)
      checkException(D_2 <- D+node("Z",distr="rbern",prob=plogis(-0.5),order=6) )
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # BUG (FIXED):
      # a bug with returning a locked DAG even when sim attempt failed
      # DOESN'T RUN IN unittest mode
      # fct1 <- function()return(1)
      # W1 <- node("W1",distr="fct1") 
      # W2 <- node("W2",distr="fct1") 
      # W3 <- node("W3",distr="fct1")
      # A <- node("A",t=0:1,distr="rbern",prob=ifelse(t=0,ifelse(W1==0,1,0),ifelse(W2==W3,1,0))) 
      # Drn <- DAG.empty()
      # Drn <- Drn + W1 + W2 + W3 + A 
      # checkException(Drn <- set.DAG(Drn))
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Enabled categoricals with probs=c(...,...) (in addition to probs={...,...})
      # new categorical syntax:
      D <- DAG.empty() + node("race",t=0,distr="rcategor",probs=c(0.2,0.4),order=1)
      D <- set.DAG(D)
      sim1b <- simobs(D, n=100, rndseed=10)
      sim1b_sim <- sim(D, n=100, rndseed=10)
      checkIdentical(sim1b,sim1b_sim)
      D <- DAG.empty() + node("race",t=0,distr="rcategor",probs=c(0.2,0.1,0.4,0.15,0.05,0.1),order=1)
      D <- set.DAG(D)
      sim1a <- simobs(D, n=100, rndseed=10)
      sim1a_sim <- sim(D, n=100, rndseed=10)
      checkIdentical(sim1a,sim1a_sim)
      # old categorical syntax:
      D <- DAG.empty() + node("race",t=0,distr="rcategor",probs={0.2;0.1;0.4;0.15;0.05;0.1},order=1)
      D <- set.DAG(D)
      sim1b <- simobs(D, n=100, rndseed=10)
      sim1b_sim <- sim(D, n=100, rndseed=10)
      checkIdentical(sim1b,sim1b_sim)
      checkIdentical(as.matrix(sim1a), as.matrix(sim1b))
      # new cat syntax with formula:
      D <- DAG.empty() +  node("L0", distr="rnorm", mean=10, sd=5) + 
                          node("L1", distr="rnorm", mean=10, sd=5) + 
                          node("L2", distr="rcategor", probs=c(abs(1/L0), abs(1/L1)))
      D <- set.DAG(D)
      sim2a <- simobs(D, n=100, rndseed=10)
      sim2a_sim <- sim(D, n=100, rndseed=10)
      checkIdentical(sim2a,sim2a_sim)
      # old cat syntax with formula:
      D <- DAG.empty() +  node("L0", distr="rnorm", mean=10, sd=5) + 
                          node("L1", distr="rnorm", mean=10, sd=5) + 
                          node("L2", distr="rcategor", probs={abs(1/L0); abs(1/L1)})
      D <- set.DAG(D)
      sim2b <- simobs(D, n=100, rndseed=10)
      checkIdentical(as.matrix(sim2a), as.matrix(sim2b))
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # BUG (FIXED):
      # a bug with using mean=L0 as formula
      # version that wasn't working before:
      D <- DAG.empty() +  node("L0", distr="rnorm", mean=10, sd=5) + 
                          node("L1", distr="rnorm", mean=L0, sd=abs(L0))
      D <- set.DAG(D)
      simnorm1a <- simobs(D, n=200, rndseed=1)
      simnorm1a_sim <- sim(D, n=200, rndseed=1)
      checkIdentical(simnorm1a,simnorm1a_sim)
      # version that was working before:
      D <- DAG.empty() +  node("L0", distr="rnorm", mean=10, sd=5) + 
                          node("L1", distr="rnorm", mean=(L0), sd=abs(L0))
      D <- set.DAG(D)
      simnorm1b <- simobs(D, n=200, rndseed=1)
      simnorm1b_sim <- sim(D, n=200, rndseed=1)
      checkIdentical(simnorm1b,simnorm1b_sim)
      checkIdentical(matrix(simnorm1a), matrix(simnorm1b))
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Removed hazard being TRUE as a default value in set.targetMSM()
    # No need for hazard when outcome is not EFU=TRUE
    # Works without hazard in general and gives error when outcome is EFU=TRUE and hazard argument is missing
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Checked MSM works with a continuous node as outcome and that hazards=TRUE gives proper error message in that case
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Added additional checks in add.note() function:
    # Checking that the node distr function exists (using exist(distr))
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Fixed plotDAG function (including subsetting by t)
    #-------------------------------------------------------------

    #-------------------------------------------------------------
    # (DONE) Added print method for action object that prints attributes and action nodes (or just names of action nodes)
    # A(D) prints list attributes that are being used in a particular action
    #-------------------------------------------------------------
}

test.plotting <- function() {
    # # skipping order, gives messages by default:
    t_end <- 16
    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
    D <- D + node("A2", t=0,        distr="rbern", prob=0, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), EFU=TRUE)
    lDAG3 <- set.DAG(D)

    # turn off default messages:
    oldverboseopt <- getOption("simcausal.verbose")
    options(simcausal.verbose=FALSE)
    t_end <- 16
    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
    D <- D + node("A2", t=0,        distr="rbern", prob=0, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), EFU=TRUE)
    lDAG3 <- set.DAG(D)
    dat <- sim(lDAG3, n=50)
    options(simcausal.verbose=oldverboseopt)


    # plotDAG(lDAG3)
    # plotDAG(lDAG3, xjitter=0.3, yjitter=0.01)

    # plotDAG(lDAG3, tmax=1)
    # plotDAG(lDAG3, tmax=1, xjitter=0.2, yjitter=0.02)

    # plotDAG(lDAG3, tmax=2)
    # plotDAG(lDAG3, tmax=2, xjitter=0.3, yjitter=0.02)

    # plotDAG(lDAG3, tmax=3)
    # plotDAG(lDAG3, tmax=3, xjitter=0.3, yjitter=0.02)

    # # overriding default vertex attributes:
    # plotDAG(lDAG3a, tmax=3, vertex_attrs=list(label.cex=0.8))
    # plotDAG(lDAG3a, tmax=3, vertex_attrs=list(size=10, label.cex=0.8))
    # # overriding default edge attributes:
    # plotDAG(lDAG3a, tmax=3, 
    #         edge_attrs=list(width=0.5, arrow.width=0.4, arrow.size=0.8), 
    #         vertex_attrs=list(size=10, label.cex=0.8))    

    #-------------------------------------------------------------
    # Adding dynamic actions (indexed by a real-valued parameter) and plotting action DAGs
    #-------------------------------------------------------------
    # act_t0_theta <- node("A1",t=0, distr="Bern", prob=ifelse(L2[0] >= theta,1,0))
    act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    act_NoCens <- node("A2",t=0:t_end, distr="rbern", prob=0)
    actionnodes <- c(act_t0_theta, act_tp_theta, act_NoCens)
    Dact <- lDAG3 + action("A1_th0", nodes=actionnodes, theta=0)
    Dact <- Dact + action("A1_th1", nodes=actionnodes, theta=1)

    # DAGact1 <- A(Dact)[["A1_th0"]]
    # plotDAG(DAGact1, tmax=3, 
    #         edge_attrs=list(width=0.3, arrow.width=0.4, arrow.size=0.8),
    #         vertex_attrs=list(size=12, label.cex=0.8))

    # plotDAG(DAGact1, tmax=3, node.action.color="green",
    #         edge_attrs=list(width=0.3, arrow.width=0.4, arrow.size=0.8),
    #         vertex_attrs=list(size=12, label.cex=0.8))
}



test.node <- function() {
    library(simcausal)    
    #-------------------------------------------------------------
    # New interface (node) checks
    #-------------------------------------------------------------
    t_end <- 5

    # uniform distribution
    D <- DAG.empty()
    D <- D + node("L0", distr="rnorm", mean=50, sd=5)
    D <- D + node("L1", distr="runif", min=1/L0, max=(L0))
    D <- set.DAG(D)
    simunif <- simobs(D, n=20, rndseed=1)
    # constant distribution
    D <- DAG.empty()
    D <- D + node("L0", distr="rnorm", mean=50, sd=5)
    D <- D + node("L1", distr="rconst", const=5)
    D <- set.DAG(D)
    simconst <- simobs(D, n=20, rndseed=1)
    # categorical distribution with constant probs
    D <- DAG.empty()
    D <- D + node("L0", distr="rcategor", probs={0.1;0.2;0.7})
    D <- set.DAG(D)
    simobs(D, n=20, rndseed=1)
    # categorical distribution with probs depending on previous nodes
    D <- DAG.empty()
    D <- D + node("L0", distr="rnorm", mean=10, sd=5)
    D <- D + node("L1", distr="rnorm", mean=10, sd=5)
    D <- D + node("L2", distr="rcategor", probs={abs(1/L0); abs(1/L1)})
    D <- set.DAG(D)
    simobs(D, n=20, rndseed=1)
    # 3 node DAG with bernoulli nodes defined via rbinom() R function
    D <- DAG.empty()
    D <- D + node("W1", distr="rbinom", prob=0.05, size=1)
    D <- D + node("W2", distr="rbinom", prob=ifelse(W1==1,0.5,0.1), size=1)
    D <- D + node("W3", distr="rbinom", prob=ifelse(W1==1,0.5,0.1), size=1)
    D1a <- set.DAG(D)
    sim1a <- simobs(D1a, n=200, rndseed=1)
    # equivalently passing distribution parameters as a "params" list argument:


    rbinom.wrap <- function(n, prob, size) {
      print("n:"); print(class(n)); print(n)
      print("size: "); print(class(size)); print(size)
      print("prob: "); print(class(prob)); print(prob)
      res <- rbinom(n = n, size = size, prob = prob)
      print("res"); print(res)
      res
    }
    # rbinom(n=3, size=1, prob=c(0.05,0.05,0.05))
    # BUG: somehow the distribution params are passed to rbinom not as named args
    # D <- DAG.empty()
    # D <- D + node("W1", distr="rbinom", params=list(prob=0.05, size=1))
    # D <- D + node("W2", distr="rbinom", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    # D <- D + node("W3", distr="rbinom", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    # D <- D + node("W1", distr="rbinom.wrap", params=list(prob=0.05, size=1))
    # D <- D + node("W2", distr="rbinom.wrap", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    # D <- D + node("W3", distr="rbinom.wrap", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    D <- D + node("W1", distr="rbinom", params=list(prob=0.05, size=1))
    D <- D + node("W2", distr="rbinom", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    D <- D + node("W3", distr="rbinom", params=list(prob="ifelse(W1==1,0.5,0.1)", size=1))
    D1b <- set.DAG(D)
    sim1b <- simobs(D1b, n=200, rndseed=1)

    checkIdentical(as.matrix(sim1a), as.matrix(sim1b))
    # equivalently using a bernoulli rbern() wrapper function
    # rbern <- function(n, prob) {
    #   rbinom(n=n, prob=prob, size=1)
    # }
    D <- DAG.empty()
    D <- D + node("W1", distr="rbern", prob=0.05)
    D <- D + node("W2", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
    D <- D + node("W3", distr="rbern", prob=ifelse(W1==1,0.5,0.1))
    D1c <- set.DAG(D)
    sim1c <- simobs(D1c, n=200, rndseed=1)
    checkIdentical(as.matrix(sim1a), as.matrix(sim1c))

    # FORMULAS CAN REFERENCE TVAR NODES
    D <- DAG.empty()
    D <- D + node("L1", t=0, distr="rbinom", prob=0.05, size=1)
    D <- D + node("L2", t=0, distr="rbinom", prob=ifelse(L1[0]==1,0.5,0.1), size=1)
    D <- set.DAG(D)
    # simobs(D, n=20, rndseed=1)

    # NODES WITH t CANNOT BE ADDED AFTER NODES WITH t WERE ALREADY DEFINED:    
    D <- DAG.empty()
    D <- D + node("W1", distr="rbinom", prob=0.05, size=1)
    D <- D + node("L1", t=0:t_end, distr="rbinom", prob=ifelse(W3==1,0.5,0.1), size=1)
    checkException(D + node("W4", distr="rbinom", prob=ifelse(W1==1,0.5,0.1), size=1))

    # ADDING NODES THAT ALREADY EXIST GETS THEM OVERWRITTEN WITH A WARNING
    D <- DAG.empty()
    D <- D + node("L1", t=0:t_end, distr="rbinom", prob=ifelse(t==.(t_end),0.5,0.1), size=1)
    D <- D + node("L2", t=0:t_end, distr="rbinom", prob=ifelse(L1[0]==1,0.5,0.1), size=1)
    D <- D + node("L1", t=3:t_end, distr="rbinom", prob=0, size=5) # test existing nodes get overwritten
    D <- D + node("L1", t=4:t_end, distr="rnorm", mean=10, sd=5) # test existing nodes get overwritten for new distribution
    D <- set.DAG(D)
    simobs(D, n=20, rndseed=1)

    # ADDING NODE with t=0 AFTER ALL PRIOR NODES HAVE t > 0 WILL PUT THE NODE with t=0 at the begining
    D <- DAG.empty()
    D <- D + node("L1", t=2:t_end, distr="rbinom", prob=ifelse(t==.(t_end),0.5,0.1), size=1)
    D <- D + node("L1", t=2:t_end, distr="rbinom", prob=ifelse(t==.(t_end),0.5,0.1), size=1)
    D <- D + node("L2", t=2:t_end, distr="rbinom", prob=ifelse(L1[2]==1,0.5,0.1), size=1)
    D <- D + node("L3", t=0:t_end, distr="rbinom", prob=ifelse(t>=2, ifelse(L1[2]==1,0.5,0.1), 0), size=1)
    D <- set.DAG(D)
    simobs(D, n=20, rndseed=1)

    # enclosing t_end variable inside .() to pass its value instead of the variable name to the node formula
    D <- DAG.empty()
    D <- D + node("L1", t=2:t_end, distr="rbinom", prob=ifelse(t==.(t_end),0.5,0.1), size=1)
    D <- D + node("L2", t=2:t_end, distr="rbinom", prob=ifelse(L1[2]==1,0.5,0.1), size=1)
    D <- D + node("L3", t=0:t_end, distr="rbinom", prob=ifelse(t>=2, ifelse(L1[2]==1,0.5,0.1), 0), size=1)
    D <- set.DAG(D)
    simobs(D, n=20, rndseed=1)

    # # skipping order
    t_end <- 16
    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
    D <- D + node("A2", t=0,        distr="rbern", prob=0, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), EFU=TRUE)
    lDAG3a <- set.DAG(D)

    # using order
    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05, order=1)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    D <- D + node("A2", t=0,        distr="rbern", prob=0, order=4, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), order=8+4*(0:(t_end-1)), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    lDAG3b <- set.DAG(D)

    simDAG3a <- simobs(lDAG3a, n=50, rndseed=1)
    simDAG3b <- simobs(lDAG3b, n=50, rndseed=1)

    checkIdentical(lDAG3a, lDAG3b)
    checkIdentical(simDAG3a, simDAG3b)
}

test.experimental_parsingMSMs <- function() {
  #-------------------------------------------------------------
  # Parsing MSM formulas for S() and then evaluating those in the environment of the simulated data
  #-------------------------------------------------------------
  t_end <- 16
  D <- DAG.empty()
  D <- D + node("L2", t=0,        distr="rbern", prob=0.05, order=1)
  D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
  D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
  D <- D + node("A2", t=0,        distr="rbern", prob=0, order=4, EFU=TRUE)
  D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
  D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
  D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
  D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), order=8+4*(0:(t_end-1)), EFU=TRUE) # informative censoring
  D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
  lDAG3 <- set.DAG(D)
  #-------------------------------------------------------------
  # Adding dynamic actions (indexed by a real-valued parameter)
  #-------------------------------------------------------------
  # act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
  act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
  act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
  act_NoCens <- node("A2",t=0:t_end, distr="rbern", prob=0)
  actionnodes <- c(act_t0_theta, act_tp_theta, act_NoCens)
  D <- lDAG3 + action("A1_th0", nodes=actionnodes, theta=0)
  D <- D + action("A1_th1", nodes=actionnodes, theta=1)

  #-------------------------------------------------------------
  # Target parameter: modelling survival with MSM
  #-------------------------------------------------------------
  # FUTURE IMPLEMENTATIONS: evaluate correctly without need to wrap summary terms inside S(.)
  # FUTURE IMPLEMENTATIONS: add checks that do not allow time-var covars inside MSM, only exposures and baseline covars

  # identify all S() functions and call parser only for those functions
  # parser algorithm:
      # use environment of the full data.frames in wide format 
      # full data: apply to each actions-specific data.frame separately or rbind all datasets and then evalute
      # parse insde the I functions for S() references, save the S() calls and replace those with future vector names, XMSMterm.k

      # create DAG nodes over XMSMterm.k_t over k and t, with formulas from corresponding S() call
      # evaluate these nodes just like in the simulation, looping over nodes and t (t's from the outcome only), using environment of the full data

      # convert either the entire dataset to long format or just the vectors XMSMterm.k_t and add those to existing long format
      # replace each I(.) arg or S() in MSM formula with XMSMterm.k vector name
      # run glm (or create design mat and run glm.fit) with new MSM formula and new long format data

  msm.form_1 <- "Y ~ theta + t + I(t^2) + I(theta*t) + I(t*theta) + L1 + S(A1[max(0,t-1)]) + I(t*S(mean(A1[0:t]))*S(A1[max(0,t-1)])) + S(mean(A1[0:t])) + S(sum(A1[0:t])) + S(A1[max(0,t-2)])"
  msm.form_2 <- "Y ~ theta + t + I(theta*t) + S(A1[max(0,t-2)])"
  msm.form_3 <- "Y ~ theta + t + I(theta*t) + S(A1[max(0,t-2)]) + S(A1[max(0,t-1)])"
  msm.form_3_error <- "Y ~ theta + t + I(theta*t) + S(A1[max(0,t-2)]) + S(L1[t]) + S(A1[max(0,t-1)])"

  t_vec <- c(0:16)
  parse_form <- simcausal:::parse.MSMform(msm.form = msm.form_1, t_vec = t_vec, old.DAG = lDAG3) # parsing the msm formula
  # parse_form <- simcausal:::parse.MSMform(msm.form=msm.form_1, t_vec=t_vec) # parsing the msm formula
  attributes(parse_form$MSMtermsDAG)$user.env <- attributes(D)$user.env
  parse_form$term_maptab
  
  # parsing new msm formula based on the old map table
  parse_form_fromold <- simcausal:::parse.MSMform(msm.form=msm.form_2, t_vec=t_vec, old.DAG = lDAG3, term_map_tab_old=parse_form$term_maptab)
  parse_form_fromold <- simcausal:::parse.MSMform(msm.form=msm.form_3, t_vec=t_vec, old.DAG = lDAG3, term_map_tab_old=parse_form$term_maptab)
  # gives a gentle error when calling with S()  expressions that are not in the provided map
  checkException(
    parse_form_fromold <- simcausal:::parse.MSMform(msm.form=msm.form_3_error, t_vec=t_vec, old.DAG = lDAG3, term_map_tab_old=parse_form$term_maptab)
  )

# First model Y_t by adding a summary measure covariate that is defined as the mean of exposure A1 from time 0 to t

  # CHECKING THAT MSM WORKS WITH SUMMARY MEASURES AS IT SHOULD     
  D <- set.targetMSM(D, outcome = "Y", t = 0:16, form = msm.form_1, family = "binomial", hazard = FALSE)
  MSMres <- eval.target(DAG = D, n = 100, rndseed = 123)
  # MSMres <- eval.target(D, n=1000, rndseed = 123)
  MSMres$coef
  # > MSMres$coef
  #                                    (Intercept)                                          theta 
  #                                    -7.23896472                                     2.08336522 
  #                                              t                                         I(t^2) 
  #                                     0.65019268                                    -0.00568024 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                    -0.15037431                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                     1.07705097                                    -0.34495920 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                    -0.25705223                                     4.31423689 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    -0.12629404                                    -0.88559796 
  # >
  D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form_1, family="binomial", hazard=FALSE)    
  X_dat <- simfull(A(D), n=100, LTCF="Y", rndseed = 123)
  X_dat_sim <- sim(actions=A(D), n=100, LTCF="Y", rndseed = 123)
  X_dat_sim_names <- sim(DAG=D, actions=c("A1_th0", "A1_th1"), n=100, LTCF="Y", rndseed = 123)
  checkIdentical(X_dat, X_dat_sim)
  checkIdentical(X_dat, X_dat_sim_names)

  # X_dat <- simfull(A(D), n=1000, LTCF="Y", rndseed = 123)
  MSMres <- eval.target(DAG=D, data=X_dat)
  MSMres$coef
  # > MSMres$coef
  #                                    (Intercept)                                          theta 
  #                                    -7.23896472                                     2.08336522 
  #                                              t                                         I(t^2) 
  #                                     0.65019268                                    -0.00568024 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                    -0.15037431                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                     1.07705097                                    -0.34495920 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                    -0.25705223                                     4.31423689 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    -0.12629404                                    -0.88559796 

  D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form_1, family="binomial", hazard=TRUE)
  MSMres <- eval.target(D, n=100, rndseed = 123)
  # MSMres <- eval.target(D, n=1000, rndseed = 123)
  MSMres$coef
  # > MSMres$coef
  #                                    (Intercept)                                          theta 
  #                                   -7.576469141                                    1.260924469 
  #                                              t                                         I(t^2) 
  #                                    0.225648338                                    0.007474925 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                   -0.116863804                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                    0.812038900                                    0.157648309 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                   -3.779440388                                   -0.135280944 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    3.492317077                                   -0.528899983 
  D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form_1, family="binomial", hazard=TRUE)
  X_dat <- simfull(A(D), n=100, rndseed = 123)
  X_dat_sim <- sim(actions=A(D), n=100, rndseed = 123)
  checkIdentical(X_dat, X_dat_sim)

  # X_dat <- simfull(A(D), n=1000, rndseed = 123)
  MSMres <- eval.target(D, data=X_dat)
  MSMres$coef    
  # > MSMres$coef
  #                                    (Intercept)                                          theta 
  #                                   -7.576469141                                    1.260924469 
  #                                              t                                         I(t^2) 
  #                                    0.225648338                                    0.007474925 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                   -0.116863804                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                    0.812038900                                    0.157648309 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                   -3.779440388                                   -0.135280944 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    3.492317077                                   -0.528899983 

  # checking subsetting by t
  D <- set.targetMSM(D, outcome="Y", t=0:5, form=msm.form_1, family="binomial", hazard=TRUE)
  X_dat <- simfull(A(D), n=100, rndseed = 123)
  X_dat_sim <- sim(actions=A(D), n=100, rndseed = 123)
  checkIdentical(X_dat, X_dat_sim)

  # X_dat <- simfull(A(D), n=1000, rndseed = 123)
  MSMres <- eval.target(D, data=X_dat)
  MSMres$msm

  DF.to.longDT(X_dat[[1]])
  # Coefficients:
  #                                    (Intercept)                                           theta  
  #                                     -7.7719930                                       1.7599218  
  #                                              t                                          I(t^2)  
  #                                      0.0001168                                       0.0474057  
  #                                   I(theta * t)                                    I(t * theta)  
  #                                     -0.3396136                                              NA  
  #                                           L1_0                            S(A1[max(0, t - 1)])  
  #                                      1.1871732                                       1.4077656  
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                                S(mean(A1[0:t]))  
  #                                     -5.0037075                                      -2.4130134  
  #                                S(sum(A1[0:t]))                            S(A1[max(0, t - 2)])  
  #                                      4.7542418                                      -0.6728950  

  #*************
  # checking that can run new MSM with old long data, with warnings
  save_df_long <- MSMres$df_long
  D <- set.targetMSM(D, outcome="Y", t=0:5, form=msm.form_2, family="binomial", hazard=TRUE)
  MSMres_new <- eval.target(D, data=save_df_long)
  MSMres_new$msm
       # (Intercept)                 theta                     t          I(theta * t)  S(A1[max(0, t - 2)])  
       #    -4.15861              -0.25397              -0.01627               0.21365              -0.47936  
  D <- set.targetMSM(D, outcome="Y", t=0:5, form=msm.form_3, family="binomial", hazard=TRUE)
  MSMres_new <- eval.target(D, data=save_df_long)
  MSMres_new$msm
       # (Intercept)                 theta                     t          I(theta * t)  S(A1[max(0, t - 2)])  S(A1[max(0, t - 1)])  
       #    -4.03712              -0.37568              -0.01627               0.24282              -0.02954              -0.57131  
  # gives gentle error
  D <- set.targetMSM(D, outcome="Y", t=0:5, form=msm.form_3_error, family="binomial", hazard=TRUE)
  checkException(
    MSMres_new <- eval.target(D, data=save_df_long)
  )

  #-------------------------------------------------------------
  # Direct implementation of MSM code with summary measures
  #-------------------------------------------------------------
  # sample full data (by action)
  X_dat <- simfull(A(D), n=100, rndseed = 123)
  # X_dat <- simfull(A(D), n=200000, rndseed = 123)
  # X_dat <- simfull(A(D), n=20000, LTCF="Y", rndseed = 123)

  nrow(X_dat[[1]])    

  # ***** evaluated summary measures for FULL data
  # *****
  SMSM_Xdat <- lapply(X_dat, function(prevdat) simcausal:::simFromDAG(DAG=parse_form$MSMtermsDAG, Nsamp=nrow(prevdat), prev.data=prevdat))
  # attributes(SMSM_Xdat[[1]])
  head(SMSM_Xdat[[1]]); nrow(SMSM_Xdat[[1]])

  XMSM_terms <- as.character(parse_form$term_maptab[,"XMSMterms"])
  X_dat_MSMsum <- lapply(seq(length(X_dat)), function(i_act) {
                                              old_attr <- simcausal:::CopyAttributes(attributes(X_dat[[i_act]]))
                                              MSM_attr <- simcausal:::CopyAttributes(attributes(SMSM_Xdat[[i_act]]))
                                              MSM_sum_nodes <- attr(SMSM_Xdat[[i_act]], "node_nms") # get new node names from MSM summary data
                                              old_attr$node_nms <- c(old_attr$node_nms, MSM_attr$node_nms)
                                              old_attr$attnames <- c(old_attr$attnames, XMSM_terms)
                                              sel_cols <- !(names(SMSM_Xdat[[i_act]])%in%("ID"))
                                              dat <- cbind(X_dat[[i_act]], SMSM_Xdat[[i_act]][,sel_cols])
                                              attributes(dat) <- c(attributes(dat), old_attr)
                                              attr(dat, "target")$params.MSM.parse <- parse_form
                                              dat
                                              })
  X_dat_MSMsum_lDT <- lapply(X_dat_MSMsum, DF.to.longDT)


  DF.to.longDT(X_dat_MSMsum[[1]])

  # ---
  df_combine_DT <- data.table::rbindlist(X_dat_MSMsum_lDT, fill=TRUE)
  head(df_combine_DT,100)

  # *****
  # --- glm fit ----
  # for 100K per action
  glmt <-  system.time(msmfit <- glm(parse_form$term_form, family=binomial, data=df_combine_DT))
  glmt
   #   user  system elapsed 
   # 47.677   6.393  56.688 
  # for 200K per action
  #    user  system elapsed 
  # 109.989  29.182 168.299     
  # msmfit$coeff
  # predict_glm <- predict(msmfit)
  # head(predict_glm)
  # ---
  # *****

  # *****
  # --- speedglm fit ----
  # --- w/ default BLAS ----
  # for 100K per action
  # library(speedglm)
  # glmt <-  system.time(speedmsmfit <- speedglm(formula=parse_form$term_form, data=df_combine_DT, family = binomial()))
  # ---
  # *****
  # glmt
  # for 100K per action
  #   user  system elapsed 
  # 9.861   2.732  12.565
  # for 200K per action 
  #   user  system elapsed 
  # 23.310   8.457  38.413 
  # speedmsmfit$coeff

  # hazard:
  # X_dat <- simfull(A(D), n=1000, rndseed = 123)
  # > msmfit$coeff
  #                                    (Intercept)                                          theta 
  #                                   -7.576469141                                    1.260924469 
  #                                              t                                         I(t^2) 
  #                                    0.225648338                                    0.007474925 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                   -0.116863804                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                    0.812038900                                    0.157648309 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                   -3.779440388                                   -0.135280944 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    3.492317077                                   -0.528899983 

  # survival:
  # with X_dat <- simfull(A(D), n=1000, LTCF="Y", rndseed = 123)
  # > msmfit$coeff
  #                                    (Intercept)                                          theta 
  #                                    -7.23896472                                     2.08336522 
  #                                              t                                         I(t^2) 
  #                                     0.65019268                                    -0.00568024 
  #                                   I(theta * t)                                   I(t * theta) 
  #                                    -0.15037431                                             NA 
  #                                           L1_0                           S(A1[max(0, t - 1)]) 
  #                                     1.07705097                                    -0.34495920 
  # I(t * S(mean(A1[0:t])) * S(A1[max(0, t - 1)]))                               S(mean(A1[0:t])) 
  #                                    -0.25705223                                     4.31423689 
  #                                S(sum(A1[0:t]))                           S(A1[max(0, t - 2)]) 
  #                                    -0.12629404                                    -0.88559796 

  X_dat_l <- simfull(A(D), n=100, wide=FALSE, rndseed = 123)
  # X_dat_l <- simfull(A(D), n=1000, wide=FALSE, rndseed = 123)
  # X_dat_l <- simfull(A(D), n=1000, wide=FALSE, LTCF="Y")
  X_dat_lDF <- lapply(X_dat, DF.to.long)
  X_dat_lDT <- lapply(X_dat, DF.to.longDT)

  nrow(X_dat_l[[1]]); nrow(X_dat_lDF[[1]]); nrow(X_dat_lDT[[1]])
  nrow(X_dat_l[[2]]); nrow(X_dat_lDF[[2]]); nrow(X_dat_lDT[[2]])

  head(X_dat_l[[1]]); head(X_dat_l[[2]])
  head(X_dat_lDF[[1]]); head(X_dat_lDF[[2]])
  head(X_dat_lDT[[1]]); head(X_dat_lDT[[2]])


  checkIdentical(X_dat_l[[1]]$ID, X_dat_MSMsum_lDT[[1]]$ID)
  checkIdentical(X_dat_l[[1]]$L1_0, X_dat_MSMsum_lDT[[1]]$L1_0)
  checkIdentical(X_dat_l[[1]]$t, X_dat_MSMsum_lDT[[1]]$t)
  checkIdentical(X_dat_l[[1]]$L2, X_dat_MSMsum_lDT[[1]]$L2)
  checkIdentical(X_dat_l[[1]]$A1, X_dat_MSMsum_lDT[[1]]$A1)
  checkIdentical(X_dat_l[[1]]$A2, X_dat_MSMsum_lDT[[1]]$A2)
  checkIdentical(X_dat_l[[1]]$Y, X_dat_MSMsum_lDT[[1]]$Y)

  checkIdentical(X_dat_l[[2]]$ID, X_dat_MSMsum_lDT[[2]]$ID)
  checkIdentical(X_dat_l[[2]]$L1_0, X_dat_MSMsum_lDT[[2]]$L1_0)
  checkIdentical(X_dat_l[[2]]$t, X_dat_MSMsum_lDT[[2]]$t)
  checkIdentical(X_dat_l[[2]]$L2, X_dat_MSMsum_lDT[[2]]$L2)
  checkIdentical(X_dat_l[[2]]$A1, X_dat_MSMsum_lDT[[2]]$A1)
  checkIdentical(X_dat_l[[2]]$A2, X_dat_MSMsum_lDT[[2]]$A2)
  checkIdentical(X_dat_l[[2]]$Y, X_dat_MSMsum_lDT[[2]]$Y)
}

test.tswitch_2MSMs <- function() {
    library(simcausal)
    #-------------------------------------------------------------
    # Parsing MSM formulas for S() and then evaluating those in the environment of the simulated data
    #-------------------------------------------------------------
    t_end <- 16
    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05, order=1)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    D <- D + node("A2", t=0,        distr="rbern", prob=0, order=4, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), order=8+4*(0:(t_end-1)), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    lDAG3 <- set.DAG(D)

    # get MSM survival predictions in long format (melted) by time, t_vec, and by MSM term, MSMtermName
    # predictions from the estimated msm model (based on observational data) can be obtained by passing estimated msm model, est.msm
    # given vector of t (t_vec), results of MSM target.eval and an MSM term get survival table by action
   .f_getsurv_byMSMterm <- function(MSMres, t_vec, MSMtermName, use_actions=NULL, est.msm=NULL) {
      library(data.table)
      # look up MSMtermName in MSMterm map, if exists -> use the new name, if doesn't exist use MSMtermName
      if (!is.null(MSMres$S.msm.map)) {
        mapS_exprs <- as.character(MSMres$S.msm.map[,"S_exprs_vec"])
        XMSMterms <- as.character(MSMres$S.msm.map[,"XMSMterms"])
        map_idx <- which(mapS_exprs%in%MSMtermName)

        XMSMtermName <- XMSMterms[map_idx]
        if (!is.null(XMSMtermName)&&length(XMSMtermName)>0) {
          MSMtermName <- XMSMtermName
        }
      }
      print("MSMtermName used"); print(MSMtermName)

      t_dt <- data.table(t=as.integer(t_vec)); setkey(t_dt, t)   

      get_predict <- function(actname) {
        # setkey(MSMres$df_long[[actname]], t)
        setkeyv(MSMres$df_long[[actname]], c("t", MSMtermName))
        # MSMterm_vals <- as.numeric(MSMres$df_long[[actname]][t_dt, mult="first"][[MSMtermName]])
        # print("MSMterm_vals"); print(MSMterm_vals)
        MSMterm_vals <- as.numeric(MSMres$df_long[[actname]][t_dt, mult="last"][[MSMtermName]])
        print("MSMterm_vals last"); print(MSMterm_vals)
        newdata=data.frame(t=t_vec, MSMterm_vals=MSMterm_vals)
        colnames(newdata) <- c("t", MSMtermName)
        # print("newdata"); print(newdata)
        if (!is.null(est.msm)) {
          pred <- predict(est.msm, newdata=newdata, type="response")
        } else {
          pred <- predict(MSMres$m, newdata=newdata, type="response")
        }
        return(data.frame(t=t_vec, pred=pred))
      }

      action_names <- names(MSMres$df_long)
      if (!is.null(use_actions)) {
        action_names <- action_names[action_names%in%use_actions]
      }

      surv <- lapply(action_names, function(actname) {
                                    res <- get_predict(actname)
                                    if (MSMres$hazard) {
                                      res$surv <- cumprod(1-res$pred)  
                                    } else {
                                      res$surv <- 1-res$pred
                                    }
                                    res$pred <- NULL
                                    res$action <- actname
                                    res
                                  })
      names(surv) <- names(MSMres$df_long)
      surv_melt <- do.call('rbind', surv)
      surv_melt$action <- factor(surv_melt$action, levels=unique(surv_melt$action), ordered=TRUE)
      surv_melt
    }

    .f_plotsurv_byMSMterm <- function(surv_melt_dat) {
      library(ggplot2)
      f_ggplot_surv_wS <- ggplot(data= surv_melt_dat, aes(x=t, y=surv)) +
                          geom_line(aes(group = action, color = action), size=.4, linetype="dashed") + 
                          theme_bw()

    }

    .f_plotsurv_byMSMterm_facet <- function(surv_melt_dat1, surv_melt_dat2, msm_names=NULL) {
      library(ggplot2)
      if (is.null(msm_names)) {
        msm_names <- c("MSM1", "MSM2")
      }
      surv_melt_dat1$MSM <- msm_names[1]
      surv_melt_dat2$MSM <- msm_names[2]

      surv_melt_dat <- rbind(surv_melt_dat1,surv_melt_dat2)
      f_ggplot_surv_wS <- ggplot(data= surv_melt_dat, aes(x=t, y=surv)) +
                          geom_line(aes(group = action, color = action), size=.4, linetype="dashed") + 
                          theme_bw() + 
                          facet_wrap( ~ MSM)
    }   
    #-------------------------------------------------------------
    # DEFINE STATIC INTERVENTION indexed by switch time tswitch (swithc A1 from 0 to 1)
    act_A1_tswitch <- node("A1",t=0:t_end, distr="rbern", prob=ifelse(t >= tswitch,1,0))
    act_NoCens <- node("A2",t=0:t_end, distr="rbern", prob=0)
    actionnodes <- c(act_A1_tswitch, act_NoCens)
    #-------------------------------------------------------------
    # tswitch_vec <- (0:(t_end+1)) # vector of switch time (t_end+1 means never switch)
    tswitch_vec <- c(0, 3, 6, 10, 13, (t_end+1))
    #-------------------------------------------------------------
    # S() formula MSM with static regimens (indexed by time switch)
    #-------------------------------------------------------------
    tvec <- 0:t_end
    D_wS <- lDAG3
    for (tswitch_i in tswitch_vec){
      print(tswitch_i)
      abar <- rep(0, length(tvec))
      abar[which(tvec >= tswitch_i)] <- 1
      print("abar"); print(abar)
      D_wS <- D_wS + action("A1_ts"%+%tswitch_i, nodes=actionnodes, tswitch=tswitch_i, abar=abar)
    }

    #-------------------------------------------------------------
    # Equivalent MSM with static regimens without S() formula (indexed by time switch) by adding mean exposure vector as an action-specific time-varying attribute
    #-------------------------------------------------------------
    tvec <- 0:t_end
    D_noS <- lDAG3
    for (tswitch_i in tswitch_vec){
      abar <- rep(0, length(tvec))
      abar[which(tvec >= tswitch_i)] <- 1 
      meanExposureVec <- cumsum(abar)/(1:length(abar))
      print("tswitch_i");print(tswitch_i)
      print("meanExposureVec"); print(round(meanExposureVec, 3))
      D_noS <- D_noS + action("A1_ts"%+%tswitch_i, nodes=actionnodes, tswitch=tswitch_i, meanExposure=meanExposureVec)
    }

    #-------------------------------------------------------------
    # two ways of doing hazard MSM w/ summary measure
    #-------------------------------------------------------------
    msm.form_1 <- "Y ~  t + S(mean(A1[0:t])) + I(t*S(mean(A1[0:t])))"
    msm.form_2 <- "Y ~  t + S(mean(abar[0:t])) + I(t*S(mean(abar[0:t])))"

    # subset of time points only (was a bug and wasn't working before)
    D_wS_subs <- set.targetMSM(D_wS, outcome="Y", t=10:16, form=msm.form_1, family="binomial", hazard=TRUE)
    hMSMres_wS1_subs <- eval.target(D_wS_subs, n=500, rndseed = 123)
    D_wS_subs <- set.targetMSM(D_wS, outcome="Y", t=10:16, form=msm.form_2, family="binomial", hazard=TRUE)
    hMSMres_wS1_subs <- eval.target(D_wS_subs, n=500, rndseed = 123)

    D_wS <- set.targetMSM(D_wS, outcome="Y", t=0:16, form=msm.form_1, family="binomial", hazard=TRUE)
    hMSMres_wS1 <- eval.target(D_wS, n=500, rndseed = 123)

    # hMSMres_wS1 <- eval.target(D_wS, n=5000, rndseed = 123)
    # X_dat_ts <- simfull(A(D_wS), n=5000, rndseed = 123)    
    # hMSMres_wS <- eval.target(D_wS, data=X_dat_ts)
    hMSMres_wS1$hazard
    hMSMres_wS1$coef
      # (Intercept)                       t        S(mean(A1[0:t])) I(t * S(mean(A1[0:t]))) 
      # -3.46447105              0.09666309             -1.07802371             -0.13721364 

    msm.form_2 <- "Y ~  t + S(mean(abar[0:t])) + I(t*S(mean(abar[0:t])))"
    D_wS <- set.targetMSM(D_wS, outcome="Y", t=0:16, form=msm.form_2, family="binomial", hazard=TRUE)
    hMSMres_wS2 <- eval.target(D_wS, n=500, rndseed = 123)
    # hMSMres_wS2 <- eval.target(D_wS, n=5000, rndseed = 123)
    hMSMres_wS2$hazard
    hMSMres_wS2$coef
     # (Intercept)                         t        S(mean(abar[0:t])) I(t * S(mean(abar[0:t]))) 
     #  -3.46447105                0.09666309               -1.07802371               -0.13721364 

    msm.form_3 <- "Y ~  t + meanExposure + I(t*meanExposure)"
    D_noS <- set.targetMSM(D_noS, outcome="Y", t=0:16, form=msm.form_3, family="binomial", hazard=TRUE)
    hMSMres_noS <- eval.target(D_noS, n=500, rndseed = 123)
    # hMSMres_noS <- eval.target(D_noS, n=5000, rndseed = 123)
    hMSMres_noS$hazard
    hMSMres_noS$coef
    # (Intercept)                   t        meanExposure I(t * meanExposure) 
    # -3.46447105          0.09666309         -1.07802371         -0.13721364 

    # plot survival functions for this hazard MSMs
    survMSMh_wS <- .f_getsurv_byMSMterm(MSMres=hMSMres_wS1, t_vec=0:16, MSMtermName="mean(A1[0:t])")
    survMSMh_noS <- .f_getsurv_byMSMterm(MSMres=hMSMres_noS, t_vec=0:16, MSMtermName="meanExposure")
    # print(.f_plotsurv_byMSMterm(survMSMh_wS))
    # print(.f_plotsurv_byMSMterm(survMSMh_noS))
    # print(.f_plotsurv_byMSMterm_facet(survMSMh_wS, survMSMh_noS))

    #-------------------------------------------------------------
    # two ways of doing survival MSM w/ summary measure
    #-------------------------------------------------------------
    msm.form_1 <- "Y ~  t + S(mean(A1[0:t])) + I(t*S(mean(A1[0:t])))"
    D_wS <- set.targetMSM(D_wS, outcome="Y", t=0:16, form=msm.form_1, family="binomial", hazard=FALSE)
    survMSMres_wS1 <- eval.target(D_wS, n=500, rndseed = 123)
    # survMSMres_wS1 <- eval.target(D_wS, n=5000, rndseed = 123)
    # survMSMres_wS <- eval.target(D_wS, n=20000, rndseed = 123)
    survMSMres_wS1$hazard
    survMSMres_wS1$coef
        # NEW (no carry foward imputation)
        # (Intercept)                       t        S(mean(A1[0:t])) I(t * S(mean(A1[0:t]))) 
        #  -2.8912232               0.2907723              -0.5532854              -0.2422200 

        # OLD (with carry forward imputation for the summary measures)
        #  (Intercept)                       t        S(mean(A1[0:t])) I(t * S(mean(A1[0:t]))) 
        # -2.9302268               0.3046414              -0.6750779              -0.3060546 

    msm.form_2 <- "Y ~  t + S(mean(abar[0:t])) + I(t*S(mean(abar[0:t])))"
    D_wS <- set.targetMSM(D_wS, outcome="Y", t=0:16, form=msm.form_2, family="binomial", hazard=FALSE)
    survMSMres_wS2 <- eval.target(D_wS, n=500, rndseed = 123)
    # survMSMres_wS2 <- eval.target(D_wS, n=5000, rndseed = 123)
    survMSMres_wS2$hazard
    survMSMres_wS2$coef
        # (Intercept)                         t        S(mean(abar[0:t])) I(t * S(mean(abar[0:t]))) 
        # -2.91170715                0.28002205               -0.03412886               -0.18715498 

    msm.form_noS <- "Y ~  t + meanExposure + I(t*meanExposure)"
    D_noS <- set.targetMSM(D_noS, outcome="Y", t=0:16, form=msm.form_noS, family="binomial", hazard=FALSE)
    survMSMres_noS <- eval.target(D_noS, n=500, rndseed = 123)
    # survMSMres_noS <- eval.target(D_noS, n=5000, rndseed = 123)
    # survMSMres_noS <- eval.target(D_noS, n=20000, rndseed = 123)
    survMSMres_noS$hazard
    survMSMres_noS$coef
        # (Intercept)                   t        meanExposure I(t * meanExposure) 
        # -2.91170715          0.28002205         -0.03412886         -0.18715498 

        # OLD (with carry forward imputation for meanExposure attributes)
        # N=5K
        # (Intercept)                   t        meanExposure I(t * meanExposure) 
        #  -2.9302268           0.3046414          -0.6750779          -0.3060546 

    # plot survival functions for this survival MSMs
    survMSM_Surv_wS1 <- .f_getsurv_byMSMterm(MSMres=survMSMres_wS1, t_vec=0:16, MSMtermName="mean(A1[0:t])")
    survMSM_Surv_wS2 <- .f_getsurv_byMSMterm(MSMres=survMSMres_wS2, t_vec=0:16, MSMtermName="mean(abar[0:t])")
    survMSM_Surv_noS <- .f_getsurv_byMSMterm(MSMres=survMSMres_noS, t_vec=0:16, MSMtermName="meanExposure")

    # print(.f_plotsurv_byMSMterm(survMSM_Surv_wS1))
    # print(.f_plotsurv_byMSMterm(survMSM_Surv_wS2))
    # print(.f_plotsurv_byMSMterm_facet(survMSM_Surv_wS1, survMSM_Surv_wS2))

    # #-------------------------------------------------------------
    # # comparing data output of two survival MSMs
    # i <- 3
    # nrow(survMSMres_wS$df_long[[i]]); nrow(survMSMres_noS$df_long[[i]])
    # setkey(survMSMres_wS$df_long[[i]], ID, t)
    # setkey(survMSMres_noS$df_long[[i]], ID, t)
    # neq_indx <- which(abs(survMSMres_wS$df_long[[i]][,XMSMterm.1]-survMSMres_noS$df_long[[i]][,meanExposure])>0.0000001)
    # survMSMres_wS$df_long[[i]][c(neq_indx[1]-2, neq_indx[1]-1,neq_indx),]
    # survMSMres_noS$df_long[[i]][c(neq_indx[1]-2, neq_indx[1]-1,neq_indx),]
    # sapply(seq(survMSMres_wS$df_long), function(i)
    #         all(abs(survMSMres_wS$df_long[[i]][,XMSMterm.1]-survMSMres_noS$df_long[[i]][,meanExposure]) < 0.0000001)
    #         )

    #-------------------------------------------------------------
    # Estimation of MSM params with ltmleMSM for several of the static regimens above (but using observational simulated data)
    #-------------------------------------------------------------
    # tswitch_vec <- (0:(t_end+1)) # vector of switch time (t_end+1 means never switch)
    tswitch_vec <- c(0, 5, 10, 17)
    tvec <- 0:t_end
    D_noS_ltmle <- lDAG3
    for (tswitch_i in tswitch_vec){
      abar <- rep(0, length(tvec))
      abar[which(tvec >= tswitch_i)] <- 1 
      meanExposureVec <- cumsum(abar)/(1:length(abar))
      print(tswitch_i)
      print(meanExposureVec)
      D_noS_ltmle <- D_noS_ltmle + action("A1_ts"%+%tswitch_i, nodes=actionnodes, tswitch=tswitch_i, meanExposure=meanExposureVec)
    }

    msm.form_noS <- "Y ~  t + meanExposure + I(t*meanExposure)"
    D_noS_ltmle <- set.targetMSM(D_noS_ltmle, outcome="Y", t=0:16, form=msm.form_noS, family="binomial", hazard=FALSE)
    MSMres_noS <- eval.target(D_noS_ltmle, n=200, rndseed = 123) 
    # MSMres_noS <- eval.target(D_noS_ltmle, n=30000, rndseed = 123)
    surv_MSM_noS <- .f_getsurv_byMSMterm(MSMres=MSMres_noS, t_vec=0:16, MSMtermName="meanExposure")
    # print(.f_plotsurv_byMSMterm(surv_MSM_noS))    
    MSMres_noS$coef
    # NEW (same meanExposure for all observations in one action) N=30K per action
       # (Intercept)                   t        meanExposure I(t * meanExposure) 
       #   -2.7780835           0.2643111          -0.4583592          -0.1388532 

    # OLD (with forward imputation):  N=20K per action
        # (Intercept)                   t        meanExposure I(t * meanExposure) 
        #  -2.8118299           0.2800626          -0.5447148          -0.2204364

    # O_dat_N20K <- simobs(D_noS_ltmle, n=200, rndseed = 123)
    # O_dat_N20K <- simobs(D_noS_ltmle, n=20000, rndseed = 123)
    # ltmleMSMres_N20K <- est.targetMSM(D_noS_ltmle, O_dat_N20K, Aname="A1", Cname="A2", Lnames="L2")
    # # save("ltmleMSMres_N20K", file="ltmleMSMres_N20K.Rda")
    # # names(ltmleMSMres_N20K)
    # ltmleMSMres_N20K

    # $tmleres
    # Estimator:  tmle 
    #                      Estimate Std. Error   CI 2.5% CI 97.5%  p-value    
    # (Intercept)         -2.620069   0.049570 -2.717224   -2.523  < 2e-16 ***
    # t                    0.247684   0.005628  0.236653    0.259  < 2e-16 ***
    # meanExposure        -0.744853   0.113594 -0.967495   -0.522 5.49e-11 ***
    # I(t * meanExposure) -0.125106   0.009434 -0.143596   -0.107  < 2e-16 ***
    # ---
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    # $iptwres
    # Estimator:  iptw 
    #                      Estimate Std. Error   CI 2.5% CI 97.5%  p-value    
    # (Intercept)         -2.637566   0.057350 -2.749969   -2.525  < 2e-16 ***
    # t                    0.249119   0.007795  0.233841    0.264  < 2e-16 ***
    # meanExposure        -0.729060   0.116857 -0.958095   -0.500 4.41e-10 ***
    # I(t * meanExposure) -0.126376   0.010837 -0.147616   -0.105  < 2e-16 ***
    # ---
    # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    # surv_ltmle_N20K <- .f_getsurv_byMSMterm(MSMres=MSMres_noS, t_vec=0:16, MSMtermName="meanExposure", est.msm=ltmleMSMres_N20K$lTMLEobj$msm)
    # print(.f_plotsurv_byMSMterm(surv_ltmle_N20K))
    # plot S()-based true MSM and ltmle MSM face to face
    # print(.f_plotsurv_byMSMterm_facet(surv_MSM_noS,surv_ltmle_N20K, c("trueMSM_noS", "ltmleMSM N20K")))

}


test.set.DAG_DAG_informcens <- function() {
    library(simcausal)
    #-------------------------------------------------------------
    # EXAMPLE 3: longitudinal data with informative censoring
    #-------------------------------------------------------------
    n <- 100
    t_end <- 16
    library(simcausal)

    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05, order=1)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    D <- D + node("A2", t=0,        distr="rbern", prob=0, order=4, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), order=8+4*(0:(t_end-1)), EFU=TRUE) # informative censoring
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    lDAG3 <- set.DAG(D)

    # example of subsetting DAG nodes by specific time points:
    # node_ts <- sapply(N(lDAG3), '[[', "t")
    # N(lDAG3)[node_ts <= 10]

    # node_ts <- sapply(N(lDAG3), '[[', "t")
    # vars_sel <- names(node_ts[node_ts <= 10])
    # all_names <- sapply(N(lDAG3), '[[', "name")    

    #-------------------------------------------------------------
    # Simulating observed data
    #-------------------------------------------------------------
    O_dat <- simobs(lDAG3, n=n, rndseed = 123)

    O_dat_LTCFY <- doLTCF(data=O_dat, LTCF="Y")  # carry forward Y
    O_dat_LTCFA2 <- doLTCF(data=O_dat, LTCF="A2")  # carry forward A2 (censoring indicator)
    # carry forward both Y and A2 (censoring indicator) by first calling doLTCF with "Y" then with "A2" or vice-versa
    O_dat_LTCFY <- doLTCF(data=O_dat, LTCF="Y")
    O_dat_LTCFA2Y <- doLTCF(data=O_dat_LTCFY, LTCF="A2")

    O_dat_long <- simobs(lDAG3, n=n, wide=FALSE, rndseed = 123)
    t_tolong <- system.time(O_dat_long_2 <- DF.to.long(O_dat))

    O_dat_long_LTCF <- simobs(lDAG3, n=n, wide=FALSE, LTCF="Y", rndseed = 123)
    #-------------------------------------------------------------
    # Adding dynamic actions (indexed by a real-valued parameter)
    #-------------------------------------------------------------
    # act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    act_NoCens <- node("A2",t=0:t_end, distr="rbern", prob=0)
    actionnodes <- c(act_t0_theta, act_tp_theta, act_NoCens)

    D <- lDAG3 + action("A1_th0", nodes=actionnodes, theta=0)
    D <- D + action("A1_th1", nodes=actionnodes, theta=1)

    #-------------------------------------------------------------
    # Simulating full data
    #-------------------------------------------------------------
    X_dat <- simfull(A(D), n=n, rndseed = 123)
    X_dat_long <- simfull(A(D), n=n, wide=FALSE, rndseed = 123)
    #-------------------------------------------------------------
    # Target parameter: counterfactual means
    #-------------------------------------------------------------
    # Vector of counterfactual mean survival over time
    D <- set.targetE(D, outcome="Y", t=0:16, param="A1_th1")
    eval.target(D, n=n, rndseed = 123)

    D <- set.targetE(D, outcome="Y", t=0:4, param="A1_th1")
    eval.target(D, n=n, rndseed = 123)

    X_dat <- simfull(A(D), n=n, rndseed = 123)
    X_dat_LTCF <- simfull(A(D), n=n, LTCF="Y", rndseed = 123) 

    eval.target(D, data=X_dat)
    eval.target(D, data=X_dat_LTCF)

    #-------------------------------------------------------------
    # Target parameter: modelling survival with MSM
    #-------------------------------------------------------------
    msm.form <- "Y ~ theta + t + I(theta*t)"
    # D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=FALSE)
    D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=FALSE)    
    # option one (have the function simulate full data itself)
    MSMres <- eval.target(D, n=500, rndseed = 123)
    MSMres$coef

    # option two (supply simulated full data)
    X_dat_long_LTCF <- simfull(A(D), n=500, wide=FALSE, LTCF="Y", rndseed = 123) # simulate some long format full data
    MSMres <- eval.target(D, data=X_dat_long_LTCF)
    MSMres$coef

    # plot survival
    S_th0 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(0,17), t=0:16), type="response")
    S_th1 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(1,17), t=0:16), type="response")
    plotSurvEst(surv=list(MSM_theta1 = S_th1, MSM_theta0 = S_th0), xindx=1:17, ylab="MSM Survival, P(T>t)", ylim=c(0.75,1.0))

    #-------------------------------------------------------------
    # Target parameter: modelling the descrete hazard with MSM
    #-------------------------------------------------------------
    msm.form <- "Y ~ theta + t + I(theta*t)"
    D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form, family="binomial", hazard=TRUE)

    MSMres <- eval.target(D, n=500, rndseed = 123) # option two (have the function simulate full data itself)
    MSMres$coef

    X_dat_long <- simfull(A(D), n=500, wide=FALSE, rndseed = 123) # simulate some long format full data
    MSMres <- eval.target(D, data=X_dat_long) # option one (supply simulated full data)
    MSMres$coef

    # plot the hazard
    h_th0 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(0,17), t=1:17), type="response")
    h_th1 <- 1-predict(MSMres$m, newdata=data.frame(theta=rep(1,17), t=1:17), type="response")
    plotSurvEst(surv=list(MSM_theta1 = h_th1, MSM_theta0 = h_th0), xindx=1:17, ylab="1 - MSM predicted hazard, P(T>t)", ylim=c(0.95,1.0))

    # Converting hazard to survival
    Surv_h_th0 <- cumprod(h_th0)
    Surv_h_th1 <- cumprod(h_th1)
    plotSurvEst(surv=list(MSM_theta1 = Surv_h_th1, MSM_theta0 = Surv_h_th0), xindx=1:17, ylab="P(T>t) from hazard", ylim=c(0.75,1.0))

    #-------------------------------------------------------------
    # Suppose now we want to model Y_t by adding a summary measure covariate that is defined as the mean of exposure A1 from time 0 to t    
    #-------------------------------------------------------------
    msm.form_sum <- "Y ~ theta + t + I(theta*t) + S(mean(A1[0:t]))"
    D <- set.targetMSM(D, outcome="Y", t=0:16, form=msm.form_sum, family="binomial", hazard=FALSE)
    MSMres <- eval.target(D, n=500, rndseed = 123)
    MSMres$coef
    # coef
     # (Intercept)            theta                t     I(theta * t) S(mean(A1[0:t])) 
     # -4.45396383       1.26001884       0.15817903      -0.05731396       0.73631725

    #-------------------------------------------------------------
    # Estimation with lTMLE package (with informative censoring)
    #-------------------------------------------------------------
    # Suppose we want to now evalute some estimator of the MSM target parameter, based on the simulated observed data
    msm.form <- "Y ~ theta + t + I(theta*t)"
    D <- set.targetMSM(D, outcome="Y", t=0:10, form=msm.form, family="binomial", hazard=FALSE)    
    O_dat <- simobs(D, n=100, rndseed = 123)

    O_dat_origDAG <- simobs(lDAG3, n=100, rndseed = 123)
    checkTrue(all(sapply(colnames(O_dat), function(colnm) identical(O_dat[,colnm], O_dat_origDAG[,colnm]))))

    # est.targetMSM(D, O_dat, Aname="A1", Cname="A2", Lnames="L2", package="ltmle")
    #   # $tmleres
    #   # Estimator:  tmle 
    #   #              Estimate Std. Error CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)   -3.6883     0.9869 -5.6226   -1.754 0.000186 ***
    #   # theta         -1.1894     1.4597 -4.0504    1.672 0.415174    
    #   # t              0.1991     0.0141  0.1715    0.227  < 2e-16 ***
    #   # I(theta * t)   0.1665     0.1362 -0.1005    0.434 0.221655    
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

    #   # $iptwres
    #   # Estimator:  iptw 
    #   #              Estimate Std. Error  CI 2.5% CI 97.5%  p-value    
    #   # (Intercept)  -3.66637    0.94693 -5.52231   -1.810 0.000108 ***
    #   # theta        -0.83628    1.38848 -3.55765    1.885 0.546977    
    #   # t             0.19774    0.02157  0.15546    0.240  < 2e-16 ***
    #   # I(theta * t) -0.14505    0.07006 -0.28236   -0.008 0.038418 *  
    #   # ---
    #   # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
}


test.set.DAG_general <- function() {
    #------------------------------------
    # TEST FOR ERRORS IN ARGUMENT NAMES
    #------------------------------------
    # test input is of correct type
    testDAG_listobj1 <- numeric(2)
    checkException(set.DAG(testDAG_listobj1))
    testDAG_listobj2 <- list(a=5, b=list(name="test"))
    checkException(set.DAG(testDAG_listobj2))

    # test that undefined names are not allowed
    testDAG_names1 <- list(a=list(name="nm", time=5), b=list(name="nm", timept =5), c=list(name="nm", timept =5))
    checkException(set.DAG(testDAG_names1))

    # test all required names present
    testDAG_names2 <- list(a=list(name="nm", time=0, distr="rnorm", order=1), 
                            b=list(name="nm", time=1, distr="rnorm"), 
                            c=list(name="nm", time=1, distr="rnorm"))
    checkException(set.DAG(testDAG_names2))

    # test for uniquness of name x time combos
    # testDAG_names3 <- list(a=list(name="nm", time=0, distr="rnorm", order=1), 
    #                         a=list(name="nm", time=0, distr="rnorm", order=2))
    # datgendist_out <- set.DAG(testDAG_names3)

    # test for correct distribution names (rbern, cat, rnorm)
    testDAG_dist <- list(a=list(name="node1", time=0, distr="rcategor", order=1), 
                            a=list(name="node", time=0, distr="rnorm", order=2))
    checkException(set.DAG(testDAG_dist))

}

# manually defining the distribution node
fmakeBern <- function(name, order, meanform, EFU=NULL, logit=TRUE, t = NULL) {
    meanform <- as.character(meanform)
    if (logit) meanform <-  paste0("plogis(", meanform, ")")
    # node()
    return(node(name = name, distr = "rbern", order = order, params = list(prob = meanform), EFU = .(EFU)))
    # return(list(name = name, distr = "rbern", dist_params = list(prob = meanform), EFU = EFU, order = order, node.env = environment()))
}

test.set.DAG_DAG1 <- function() {
    #--------------------------------------------------------
    # DAG 1: set.DAG & simobs
    #--------------------------------------------------------
    # real DAG 1
    n <- 500
    # n <- 100000
    n <- n  # faster run time
    W1 <- fmakeBern("W1", 1, -0.5)
    W2 <- fmakeBern("W2", 2, "-0.5 + 0.5*W1")
    W3 <- fmakeBern("W3", 3, "-0.5 + 0.7*W1 + 0.3*W2")
    A <- fmakeBern("A", 4, "-0.5 - 0.3*W1 - 0.3*W2 - 0.2*W3")
    Y <- fmakeBern("Y", 5, "-0.1 + 1.2*A + 0.3*W1 + 0.3*W2 + 0.2*W3", TRUE)
    # testDAG_1 <- list(W1 = W1, W2 = W2, W3 = W3, A = A, Y = Y)
    testDAG_1 <- c(W1, W2, W3, A, Y)
    datgendist_DAG1 <- set.DAG(testDAG_1)
    simODAG_1 <- simobs(datgendist_DAG1, n = n, rndseed = 123)
    # attributes(datgendist_DAG1)
    # str(attributes(datgendist_DAG1))
    # head(simODAG_1)
    # nrow(simODAG_1)

    #--------------------------------------------------------
    # DAG 1 - action 1: simcausal:::setAction & simfull
    #--------------------------------------------------------
    # testing one intervention (named nested list with each item = one intervention)
    # newactions1 <-  list(AW2set1 = list(W2=list(name="W2", distr="rbern", prob=1), 
    #                                         A=list(name="A", distr="rbern", prob=1)))
    newaction1 <-  c(node("W2", distr="rbern", prob=1), node("A", distr="rbern", prob=1))


    # returns a named list of DAGs, where each item = one counterfactual DAG

    action1_DAG_1 <- list(simcausal:::setAction(actname="action1_DAG_1", inputDAG=datgendist_DAG1, actnodes=newaction1))
    checkTrue(class(action1_DAG_1)%in%"list")
    checkTrue(length(action1_DAG_1)==1)
    checkEquals(action1_DAG_1[[1]]$W2$dist_params$prob,"1")
    checkEquals(action1_DAG_1[[1]]$A$dist_params$prob,"1")

    fulldf_ac1 <- simfull(action1_DAG_1, n=n, rndseed = 123)
    # nrow(fulldf_ac1[[1]])
    checkTrue(class(fulldf_ac1)=="list")
    checkTrue(length(fulldf_ac1)==1)
    # head(fulldf_ac1[[1]])

    checkTrue(all(fulldf_ac1[[1]]$W2==1))
    checkTrue(all(fulldf_ac1[[1]]$A==1))

    #--------------------------------------------------------
    # DAG 1 - action 2: simcausal:::setAction & simfull
    #--------------------------------------------------------
    # testing two interventions - each intervention results in a separate DAG
    newaction1 <-  c(node("W2", distr="rbern", prob=1), node("A", distr="rbern", prob=1))
    newaction2 <-  node(name="A", distr="rbern", prob=0)
    # returns a named list of DAGs, where each item = one counterfactual DAG
    action1_DAG_1 <- list(simcausal:::setAction(actname="AW2set1", inputDAG=datgendist_DAG1, actnodes=newaction1))
    action2_DAG_1 <- list(simcausal:::setAction(actname="Aset0", inputDAG=datgendist_DAG1, actnodes=newaction2))

    actions_DAG_1 <- c(AW2set1 = action1_DAG_1, Aset0=action2_DAG_1)
    names(actions_DAG_1)

    checkTrue(class(actions_DAG_1)=="list")
    checkTrue(length(actions_DAG_1)==2)
    checkEquals(actions_DAG_1[[1]]$W2$dist_params$prob,"1")
    checkEquals(actions_DAG_1[[1]]$A$dist_params$prob,"1")
    checkEquals(actions_DAG_1[[2]]$A$dist_params$prob,"0")

    fulldf_ac2 <- simfull(actions_DAG_1, n=n, rndseed = 123)
    checkTrue(class(fulldf_ac2)=="list")
    checkTrue(length(fulldf_ac2)==2)
    # checkTrue(all(names(fulldf_ac2)==names(newactions2)))
    # head(fulldf_ac2$AW2set1)
    # attributes(fulldf_ac2$AW2set1)
    # head(fulldf_ac2$Aset0)
    # attributes(fulldf_ac2$Aset0)
    checkTrue(all(sapply(fulldf_ac2, nrow)==n))

    #--------------------------------------------------------
    # DAG 1: Calculate the true parameter
    #--------------------------------------------------------
    # test for naming errors, argument errors
    # test for mean of one intervention (one node) and two ways of specifying full data (action DAG or simulated full data)

    # THIS OLD INTERFACE DOESN"T WORK ANYMORE:
    # outnodes <- list(gen_name="Y", t=NULL)
    # (psi_fuldf <- getTrueParam(outnodes=outnodes, param="Aset0", df_full=fulldf_ac2))
    # (psi_actionDAG <- getTrueParam(outnodes=outnodes, param="Aset0", actions_DAG=actions2_DAG_1, n=n))

    # NEW INTERFACE:
    D_test <- datgendist_DAG1 + action("AW2set1", nodes=newaction1)
    D_test <- D_test + action("Aset0", nodes=newaction2)
    D_test <- set.targetE(D_test, outcome="Y", param="Aset0")
    (psi_fuldf <- eval.target(D_test, data=fulldf_ac2)$res)
    (psi_actionDAG <- eval.target(D_test, n=n, rndseed = 123)$res)
    checkTrue(psi_fuldf==psi_actionDAG)

    # (psi_fuldf <- getTrueParam(outnodes=outnodes, param="AW2set1", df_full=fulldf_ac2))
    # (psi_actionDAG <- getTrueParam(outnodes=outnodes, param="AW2set1", actions_DAG=actions2_DAG_1, n=n))

    D_test <- set.targetE(D_test, outcome="Y", param="AW2set1")
    (psi_fuldf <- eval.target(D_test, data=fulldf_ac2)$res)
    (psi_actionDAG <- eval.target(D_test, n=n, rndseed = 123)$res)
    checkTrue(psi_fuldf==psi_actionDAG)

    # test for contrasts (one node) & passing actionDAG isntead of full data:

    # outnodes <- list(gen_name="Y", t=NULL)
    # (psi_fuldf <- getTrueParam(outnodes=outnodes, param="Aset0-AW2set1", df_full=fulldf_ac2))
    # (psi_actionDAG <- getTrueParam(outnodes=outnodes, param="Aset0-AW2set1", actions_DAG=actions2_DAG_1, n=n))

    D_test <- set.targetE(D_test, outcome="Y", param="Aset0-AW2set1")
    (psi_fuldf <- eval.target(D_test, data=fulldf_ac2)$res)
    (psi_actionDAG <- eval.target(D_test, n=n, rndseed = 123)$res)
    checkTrue(psi_fuldf==psi_actionDAG)

    # (psi_fuldf2 <- getTrueParam(outnodes=outnodes, param="AW2set1-Aset0", df_full=fulldf_ac2))
    # (psi_fuldf2 <- getTrueParam(outnodes=outnodes, param="AW2set1 - Aset0", df_full=fulldf_ac2))

    D_test <- set.targetE(D_test, outcome="Y", param="AW2set1-Aset0")
    (psi_fuldf2 <- eval.target(D_test, data=fulldf_ac2)$res)
    checkTrue(psi_fuldf==-psi_fuldf2)
    D_test <- set.targetE(D_test, outcome="Y", param="AW2set1 - Aset0")
    (psi_fuldf2 <- eval.target(D_test, data=fulldf_ac2)$res)
    checkTrue(psi_fuldf==-psi_fuldf2)

    # test for ratios (one node) & passing actionDAG isntead of full data:
    # outnodes <- list(gen_name="Y", t=NULL)
    # (psi_fuldf <- getTrueParam(outnodes=outnodes, param="AW2set1 / Aset0", df_full=fulldf_ac2))
    # (psi_actionDAG <- getTrueParam(outnodes=outnodes, param="AW2set1 / Aset0", actions_DAG=actions2_DAG_1, n=n))

    D_test <- set.targetE(D_test, outcome="Y", param="AW2set1 / Aset0")
    (psi_fuldf <- eval.target(D_test, data=fulldf_ac2)$res)
    (psi_actionDAG <- eval.target(D_test, n=n, rndseed = 123)$res)
    checkTrue(round(psi_fuldf, 4) == round(psi_actionDAG, 4))

    # (psi_fuldf2 <- getTrueParam(outnodes=outnodes, param="AW2set1/Aset0", df_full=fulldf_ac2))
    
    D_test <- set.targetE(D_test, outcome="Y", param="AW2set1/Aset0")
    (psi_fuldf2 <- eval.target(D_test, data=fulldf_ac2)$res)
    checkTrue(round(psi_fuldf2, 4) == round(psi_fuldf, 4))

    # (psi_fuldf2 <- getTrueParam(outnodes=outnodes, param="Aset0/AW2set1", df_full=fulldf_ac2))
    # (psi_fuldf2 <- getTrueParam(outnodes=outnodes, param="Aset0 / AW2set1", df_full=fulldf_ac2)    )

    D_test <- set.targetE(D_test, outcome="Y", param="Aset0/AW2set1")
    (psi_fuldf2 <- eval.target(D_test, data=fulldf_ac2)$res)
    checkTrue(round(1/psi_fuldf2, 4) == round(psi_fuldf, 4))

    D_test <- set.targetE(D_test, outcome="Y", param="Aset0 / AW2set1")
    (psi_fuldf2 <- eval.target(D_test, data=fulldf_ac2)$res)
    checkTrue(round(1/psi_fuldf2, 4) == round(psi_fuldf, 4))
}

test.set.DAG_DAG2_errors <- function() {
    # test for no underscore char "_" in node names
    checkException(node("A_2", distr="rbern", prob=0.5, order=1))

    # RETURNS AN ERROR (AS IT SHOULD) - FIXED TO CORRECTLY THROW AN EXCEPTION: VAR L2[0] NOT DEFINED
    L2_0 <- node(name="L1", t=0, distr="rbern", prob=0.05, order=1)
    L1_0 <- node(name="L2", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    checkException(datgendist_DAG2_error <- set.DAG(c(L2_0,L1_0)))
    # <simpleError in L2[0]: undefined time-dependent variable(s): L2_0>
    rm(list=c("L2_0","L1_0"))

    #--------------------------------------------------------
    # long DAG 2: set.DAG - catching a node naming error
    #--------------------------------------------------------
    n <- 500
    L1_0 <- fmakeBern("L1", 1, 0.05, NULL, FALSE, t=0)
    L2_0 <- fmakeBern("L2", 2, "ifelse(L1_0==1,0.5,0.1)", NULL, FALSE, t=0)
    A1_0 <- fmakeBern("A1", 3, "ifelse(all(c(L1_0,L2_0)==c(1,0)), 0.5, ifelse(all(c(L1_0,L2_0)==c(0,0)) , 0.1, ifelse(all(c(L1_0,L2_0)==c(1,1)) , 0.9, 0.5)))", NULL, FALSE, t=0)
    A2_0 <- fmakeBern("A1", 4, "0", NULL, FALSE, t=0)
    Y_0 <- fmakeBern("Y", 5, "0", TRUE, FALSE, t=0)
    
    #*********************************************
    # GIVES AN ERROR AS IT SHOULD, SINCE THE NODE NAMES OF THE OBJECT AND LIST DON'T MATCH (A1_0!=A2_0)
    #*********************************************
    testDAG_2_err1a <- list(L1_0=L1_0, L2_0=L2_0, A1_0=A1_0, A2_0=A2_0)
    checkException(datgendist_DAG2_err1a <- set.DAG(testDAG_2_err1a))

    #*********************************************    
    # GIVES AN ERROR AS IT SHOULD, SINCE LAST TWO NODES HAVE THE SAME NAME (without error would proceed to overwrite the column A1_0 twice)
    #*********************************************    
    testDAG_2_err1b <- list(L1_0=L1_0, L2_0=L2_0, A1_0=A1_0, A1_0=A2_0, Y_0=Y_0)
    checkException(datgendist_DAG2_err1b <- set.DAG(testDAG_2_err1b))
}

# DAG3: testing wide/long format conversion with missingness
test.set.DAG_DAG3_wlong <- function() {
    # #-------------------------------------------------------------
    # # RESHAPING TO LONG FORMAT:
    # *) keeps all covariates that appear only once and at the first time-point constant (carry-forward)
    # *) all covariates that appear fewer than range(t) times are imputed with NA for missing time-points
    # *) observations with all NA's for all time-varying covars are removed
    # #-------------------------------------------------------------
    n <- 50
    t_end <- 16

    W1 <- node(name="W1", t=0, distr="rbern", prob=0.05, order=1)
    W2 <- node(name="W2", t=0, distr="rbern", prob=0.05, order=2)
    
    L2_0 <- node(name="L2", t=0, distr="rbern", prob=0.05, order=3)
    L1_0 <- node(name="L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=4)
    A1_0 <- node(name="A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=5)
    A2_0 <- node(name="A2", t=0, distr="rbern", prob=0, order=6)
    Y_0 <-  node(name="Y",  t=0, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=7, EFU=TRUE)

    L2_t <- node(name="L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=8+5*(0:(t_end-1)))
    L1_t <- node(name="L1", t=c(1:3, 10, 11), distr="rbern", prob=0, order=9+5*c(0:2, 9, 10))
    A1_t <- node(name="A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=10+5*(0:(t_end-1)))
    A2_t <- node(name="A2", t=1:t_end, distr="rbern", prob=0, order=11+5*(0:(t_end-1)))
    Y_t <- node(name="Y", t=1:t_end, distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=12+5*(0:(t_end-1)), EFU=TRUE)
    # (ord_vec_L2_t <- sapply(L2_t, "[[", "order"))
    # (ord_vec_L1_t <- sapply(L1_t, "[[", "order"))
    # (ord_vec_A1_t <- sapply(A1_t, "[[", "order"))
    # (ord_vec_A2_t <- sapply(A2_t, "[[", "order"))
    # (ord_vec_Y_t <- sapply(Y_t, "[[", "order"))

    datgendist_DAG2_longtest <- set.DAG(c(W1,W2,L2_0,L1_0,A1_0,A2_0,Y_0,L2_t,L1_t,A1_t,A2_t,Y_t))

    simODAG_wtest <- simobs(datgendist_DAG2_longtest, n=n, wide=TRUE, rndseed = 123)
    simODAG_ltest <- simobs(datgendist_DAG2_longtest, n=n, wide=FALSE, rndseed = 123)
    simODAG_ltest_2 <- DF.to.long(simODAG_wtest)
    simODAG_ltest_2DT <- DF.to.longDT(simODAG_wtest)

    # head(cbind(simODAG_ltest,simODAG_ltest_2), 100)
    # head(cbind(simODAG_ltest_2DT,simODAG_ltest_2), 100)

    checkIdentical(simODAG_ltest[,"ID"], simODAG_ltest_2DT[["ID"]])
    checkIdentical(simODAG_ltest[,"W1"], simODAG_ltest_2DT[["W1"]])
    checkIdentical(simODAG_ltest[,"W2"], simODAG_ltest_2DT[["W2"]])
    checkIdentical(simODAG_ltest[,"t"], simODAG_ltest_2DT[["t"]])
    checkIdentical(simODAG_ltest[,"L2"], simODAG_ltest_2DT[["L2"]])
    checkIdentical(simODAG_ltest[,"L1"], simODAG_ltest_2DT[["L1"]])
    checkIdentical(simODAG_ltest[,"A1"], simODAG_ltest_2DT[["A1"]])
    checkIdentical(simODAG_ltest[,"A2"], simODAG_ltest_2DT[["A2"]])
    checkIdentical(simODAG_ltest[,"Y"], simODAG_ltest_2DT[["Y"]])

}    


test.faster_tolongdata <- function() {
    #-------------------------------------------------------------
    # Testing the converter to long format that is based on package data.table
    #-------------------------------------------------------------
    t_end <- 16
    library(simcausal)

    D <- DAG.empty()
    D <- D + node("L2", t=0,        distr="rbern", prob=0.05, order=1)
    D <- D + node("L1", t=0,        distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1), order=2)
    D <- D + node("A1", t=0,        distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))), order=3)
    D <- D + node("A2", t=0,        distr="rbern", prob=0, order=4, EFU=TRUE)
    D <- D + node("Y",  t=0,        distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), order=5, EFU=TRUE)
    D <- D + node("L2", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, ifelse(L2[t-1]==1, 0.9, min(1,0.1 + t/16))), order=6+4*(0:(t_end-1)))
    D <- D + node("A1", t=1:t_end,  distr="rbern", prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, ifelse(L1[0]==0 & L2[0]==0, 0.1, ifelse(L1[0]==1 & L2[0]==1, 0.7, 0.5)))), order=7+4*(0:(t_end-1)))
    D <- D + node("A2", t=1:t_end,  distr="rbern", prob=plogis(-3.5 + 0.5*A1[t]+0.5*L2[t]), order=8+4*(0:(t_end-1)), EFU=TRUE) # informative censoring

    # this takes longer (6 sec longer for 1Mil obs)
    # D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), order=9+4*(0:(t_end-1)), EFU=TRUE)
    D <- D + node("Y",  t=1:t_end,  distr="rbern", prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*(sum(L2[0:t])==0)), order=9+4*(0:(t_end-1)), EFU=TRUE)
    lDAG3 <- set.DAG(D)

    #-------------------------------------------------------------
    # Adding dynamic actions (indexed by a real-valued parameter)
    #-------------------------------------------------------------
    # act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
    act_tp_theta <- node("A1",t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
    act_NoCens <- node("A2",t=0:t_end, distr="rbern", prob=0)
    actionnodes <- c(act_t0_theta, act_tp_theta, act_NoCens)
    D <- lDAG3 + action("A1_th0", nodes=actionnodes, theta=0)
    D <- D + action("A1_th1", nodes=actionnodes, theta=1)

    #-------------------------------------------------------------
    # Testing conversion of observed data to long format 
    #-------------------------------------------------------------
    # NO carry forward imputation:
    O_dat_df <- simobs(D, n=500, rndseed = 123)
    system.time(O_dat_long <- DF.to.long(O_dat_df))
    system.time(O_dat_long_DT <- DF.to.longDT(O_dat_df))

    checkIdentical(O_dat_long$ID, O_dat_long_DT$ID)
    checkIdentical(O_dat_long$L1_0, O_dat_long_DT$L1_0)
    checkIdentical(O_dat_long$t, O_dat_long_DT$t)
    checkIdentical(O_dat_long$L2, O_dat_long_DT$L2)
    checkIdentical(O_dat_long$A1, O_dat_long_DT$A1)
    checkIdentical(O_dat_long$A2, O_dat_long_DT$A2)
    checkIdentical(O_dat_long$Y, O_dat_long_DT$Y)

    # With carry forward imputation of Y (vs 1):
    O_dat_df <- simobs(D, n=500, rndseed = 123)
    O_dat_LTCF <- doLTCF(data=O_dat_df, LTCF="Y")
    system.time(O_dat_long_LTCF_v1 <- DF.to.long(O_dat_LTCF))
    system.time(O_dat_long_DT_LTCF_v1 <- DF.to.longDT(O_dat_LTCF))

    checkIdentical(O_dat_long_LTCF_v1$ID, O_dat_long_DT_LTCF_v1$ID)
    checkIdentical(O_dat_long_LTCF_v1$L1_0, O_dat_long_DT_LTCF_v1$L1_0)
    checkIdentical(O_dat_long_LTCF_v1$t, O_dat_long_DT_LTCF_v1$t)
    checkIdentical(O_dat_long_LTCF_v1$L2, O_dat_long_DT_LTCF_v1$L2)
    checkIdentical(O_dat_long_LTCF_v1$A1, O_dat_long_DT_LTCF_v1$A1)
    checkIdentical(O_dat_long_LTCF_v1$A2, O_dat_long_DT_LTCF_v1$A2)
    checkIdentical(O_dat_long_LTCF_v1$Y, O_dat_long_DT_LTCF_v1$Y)

    # With carry forward imputation of Y (vs 2):
    O_dat_df_LTCF <- simobs(D, n=500, LTCF="Y", rndseed = 123)
    system.time(O_dat_long_LTCF <- DF.to.long(O_dat_df_LTCF))
    system.time(O_dat_long_DT_LTCF <- DF.to.longDT(O_dat_df_LTCF))

    checkIdentical(O_dat_long_LTCF$ID, O_dat_long_DT_LTCF$ID)
    checkIdentical(O_dat_long_LTCF$L1_0, O_dat_long_DT_LTCF$L1_0)
    checkIdentical(O_dat_long_LTCF$t, O_dat_long_DT_LTCF$t)
    checkIdentical(O_dat_long_LTCF$L2, O_dat_long_DT_LTCF$L2)
    checkIdentical(O_dat_long_LTCF$A1, O_dat_long_DT_LTCF$A1)
    checkIdentical(O_dat_long_LTCF$A2, O_dat_long_DT_LTCF$A2)
    checkIdentical(O_dat_long_LTCF$Y, O_dat_long_DT_LTCF$Y)

    #-------------------------------------------------------------
    # Testing conversion of full data to long format (with carry forward imputation)
    #-------------------------------------------------------------
    X_dat <- simfull(A(D), n=500, rndseed = 123)
    attributes(X_dat[[1]])$node_nms

    system.time(X_dat_l <- lapply(X_dat, DF.to.long))
    system.time(X_dat_lDT <- lapply(X_dat, DF.to.longDT))

    checkIdentical(X_dat_l[["A1_th0"]]$ID, X_dat_lDT[["A1_th0"]]$ID)
    checkIdentical(X_dat_l[["A1_th0"]]$L1_0, X_dat_lDT[["A1_th0"]]$L1_0)
    checkIdentical(X_dat_l[["A1_th0"]]$t, X_dat_lDT[["A1_th0"]]$t)
    checkIdentical(X_dat_l[["A1_th0"]]$L2, X_dat_lDT[["A1_th0"]]$L2)
    checkIdentical(X_dat_l[["A1_th0"]]$A1, X_dat_lDT[["A1_th0"]]$A1)
    checkIdentical(X_dat_l[["A1_th0"]]$A2, X_dat_lDT[["A1_th0"]]$A2)
    checkIdentical(X_dat_l[["A1_th0"]]$Y, X_dat_lDT[["A1_th0"]]$Y)

    checkIdentical(X_dat_l[["A1_th1"]]$ID, X_dat_lDT[["A1_th1"]]$ID)
    checkIdentical(X_dat_l[["A1_th1"]]$L1_0, X_dat_lDT[["A1_th1"]]$L1_0)
    checkIdentical(X_dat_l[["A1_th1"]]$t, X_dat_lDT[["A1_th1"]]$t)
    checkIdentical(X_dat_l[["A1_th1"]]$L2, X_dat_lDT[["A1_th1"]]$L2)
    checkIdentical(X_dat_l[["A1_th1"]]$A1, X_dat_lDT[["A1_th1"]]$A1)
    checkIdentical(X_dat_l[["A1_th1"]]$A2, X_dat_lDT[["A1_th1"]]$A2)
    checkIdentical(X_dat_l[["A1_th1"]]$Y, X_dat_lDT[["A1_th1"]]$Y)

    # BENCHMARKING for 50K: gain of x5.4 factor
    # old convert to long
    # user  system elapsed 
    # 13.943   1.783  15.766 
    # new DF.to.longDT
    # user  system elapsed 
    # 2.564   0.370   2.935 

    # BENCHMARKING for 500K: gain of x5.4 factor
    # old convert to long
    # user  system elapsed 
    # 140.378  18.398 158.853     
    # new DF.to.longDT    
    # user  system elapsed 
    # 28.753   4.092  32.844 

    # CONVERTING BACK TO WIDE FORMAT:
    # ## convert long to wide using dcast from data.table
    # dcast.data.table(data, formula, fun.aggregate = NULL, 
    #     ..., margins = NULL, subset = NULL, fill = NULL, 
    #     drop = TRUE, value.var = guess(data),
    #     verbose = getOption("datatable.verbose"))


    #-------------------------------------------------------------
    # BENCHMARKING current package rowSums with data.table version - abandoned for now
    #-------------------------------------------------------------
    # t_pts <- 0:16
    # t_vec <- "_"%+%(t_pts)
    # (L2_names <- "L2"%+%t_vec)
    # # library(data.table)
    # system.time( X_dat_1 <- simfull(A(D)[1], n=1000000, LTCF="Y", rndseed = 123)[[1]])
    # X_dat_1 <- X_dat_1[,-1]
    # nrow(X_dat_1)
    # colnames(X_dat_1)
    # head(X_dat_1)
    # X_dat_1_DT = data.table(X_dat_1)
    # setkey(X_dat_1_DT,ID)
    # L2_idx <- which(names(X_dat_1_DT)%in%L2_names)
    # ID_idx <- which(names(X_dat_1_DT)%in%"ID")
    # ncol(X_dat_1_DT)
    # # COMPARING data.table to current data.rame rowSums
    # # fast version 1  of row sums 1
    # system.time(
    # X_dat_1_DT[, SumRow := rowSums(.SD), .SDcols = L2_idx]
    # )
    # user  system elapsed 
    # 0.139   0.030   0.181     

    # # faster version 2 of row sums
    # system.time(
    # X_dat_1_DT[, SumRow2 := Reduce(`+`, .SD), .SDcol = L2_idx]
    # )
    #  user  system elapsed 
    # 0.075   0.027   0.120

    # # version 3 using set
    # # i  In set(), integer row numbers to be assigned value.
    # # NULL represents all rows more efficiently than creating a vector such as 1:nrow(x).
    # # j In set(), integer column number to be assigned value.    
    # # x - DT, i - row indx, j - col indx, value - new val
    # set(x, i=NULL, j, value)
    # system.time(for (i in 1:nrow(X_dat_1_DT)) set(X_dat_1_DT,as.integer(i),"SumRow3",i))

    # # current version of row sums (slowest)
    # system.time(newVar_vold <- rowSums(X_dat_1[,L2_names]))
    # # COMPARING data.table to current data.rame rowSums with some column operations
    # system.time(X_dat_1_DT[, SumRow := rowSums(.SD==c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), .SDcols = L2_idx])
    # system.time(newVar_vold <- rowSums(I(X_dat_1[,L2_names] == cbind(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))))

}

