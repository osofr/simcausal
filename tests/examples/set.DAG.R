#---------------------------------------------------------------------------------------
# EXAMPLE 1A: Define some Bernoulli nodes, survival outcome Y and put it together in a
# DAG object
#---------------------------------------------------------------------------------------
W1 <- node(name = "W1", distr = "rbern",
	prob = plogis(-0.5), order = 1)
W2 <- node(name = "W2", distr = "rbern",
	prob = plogis(-0.5 + 0.5 * W1), order = 2)
A <- node(name = "A", distr = "rbern",
	prob = plogis(-0.5 - 0.3 * W1 - 0.3 * W2), order = 3)
Y <- node(name = "Y", distr = "rbern",
	prob = plogis(-0.1 + 1.2 * A + 0.3 * W1 + 0.3 * W2), order = 4)
D1A <- set.DAG(c(W1,W2,A,Y))

#---------------------------------------------------------------------------------------
# EXAMPLE 1B: Same as 1A using +node interface and no order argument
#---------------------------------------------------------------------------------------
D1B <- DAG.empty()
D1B <- D1B + node(name = "W1", distr = "rbern",
	prob = plogis(-0.5))
D1B <- D1B + node(name = "W2", distr = "rbern",
	prob = plogis(-0.5 + 0.5 * W1))
D1B <- D1B + node(name = "A", distr = "rbern",
	prob = plogis(-0.5 - 0.3 * W1 - 0.3 * W2))
D1B <- D1B + node(name = "Y", distr = "rbern",
	prob = plogis(-0.1 + 1.2 * A + 0.3 * W1 + 0.3 * W2))
D1B <- set.DAG(D1B)

#---------------------------------------------------------------------------------------
# EXAMPLE 1C: Same as 1A and 1B using add.nodes interface and no order argument
#---------------------------------------------------------------------------------------
D1C <- DAG.empty()
D1C <- add.nodes(D1C, node(name = "W1", distr = "rbern",
	prob = plogis(-0.5)))
D1C <- add.nodes(D1C, node(name = "W2", distr = "rbern",
	prob = plogis(-0.5 + 0.5 * W1)))
D1C <- add.nodes(D1C, node(name = "A", distr = "rbern",
	prob = plogis(-0.5 - 0.3 * W1 - 0.3 * W2)))
D1C <- add.nodes(D1C, node(name = "Y", distr = "rbern",
	prob = plogis(-0.1 + 1.2 * A + 0.3 * W1 + 0.3 * W2)))
D1C <- set.DAG(D1C)

#---------------------------------------------------------------------------------------
# EXAMPLE 1D: Add a uniformly distributed node and redefine outcome Y as categorical
#---------------------------------------------------------------------------------------
D_unif <- DAG.empty()
D_unif <- D_unif +
node("W1", distr = "rbern", prob = plogis(-0.5)) +
node("W2", distr = "rbern", prob = plogis(-0.5 + 0.5 * W1)) +
node("W3", distr = "runif", min = plogis(-0.5 + 0.7 * W1 + 0.3 * W2), max = 10) +
node("An", distr = "rbern", prob = plogis(-0.5 - 0.3 * W1 - 0.3 * W2 - 0.2 * sin(W3)))
# Categorical syntax 1 (probabilities as values):
D_cat_1 <- D_unif + node("Y", distr = "rcategor", probs = {0.3; 0.4})
D_cat_1 <- set.DAG(D_cat_1)
# Categorical syntax 2 (probabilities as formulas):
D_cat_2 <- D_unif +
node("Y", distr = "rcategor",
	probs={plogis(-0.1 + 1.2 * An + 0.3 * W1 + 0.3 * W2 + 0.2 * cos(W3));
			plogis(-0.5 + 0.7 * W1)})
D_cat_2 <- set.DAG(D_cat_2)

#---------------------------------------------------------------------------------------
# EXAMPLE 2A: Define Bernoulli nodes using R rbinom() function, defining prob argument
# for L2 as a function of node L1
#---------------------------------------------------------------------------------------
D <- DAG.empty()
D <- D +
node("L1", t = 0, distr = "rbinom",
	prob = 0.05, size = 1) +
node("L2", t = 0, distr = "rbinom",
	prob = ifelse(L1[0] == 1, 0.5, 0.1), size = 1)
D <- set.DAG(D)

#---------------------------------------------------------------------------------------
# EXAMPLE 2B: Equivalent to 2A, passing argument size to rbinom inside a named list
# params
#---------------------------------------------------------------------------------------
D <- DAG.empty()
D <- D +
node("L1", t = 0, distr = "rbinom",
	prob = 0.05, params = list(size = 1)) +
node("L2", t = 0, distr = "rbinom",
	prob = ifelse(L1[0] == 1,0.5,0.1), params = list(size = 1))
D <- set.DAG(D)

#---------------------------------------------------------------------------------------
# EXAMPLE 2C: Equivalent to 2A and 2B, define Bernoulli nodes using a wrapper "rbern"
#---------------------------------------------------------------------------------------
D <- DAG.empty()
D <- D +
node("L1", t = 0, distr = "rbern", prob = 0.05) +
node("L2", t = 0, distr = "rbern", prob = ifelse(L1[0] == 1, 0.5, 0.1))
D <- set.DAG(D)

#---------------------------------------------------------------------------------------
# EXAMPLE 3: Define node with normal distribution using rnorm() R function
#---------------------------------------------------------------------------------------
D <- DAG.empty()
D <- D + node("L2", t = 0, distr = "rnorm", mean = 10, sd = 5)
D <- set.DAG(D)

#---------------------------------------------------------------------------------------
# EXAMPLE 4: Define 34 Bernoulli nodes, or 2 Bernoulli nodes over 17 time points,
#---------------------------------------------------------------------------------------
t_end <- 16
D <- DAG.empty()
D <- D +
node("L2", t = 0:t_end, distr = "rbinom",
	prob = ifelse(t == t_end, 0.5, 0.1), size = 1) +
node("L1", t = 0:t_end, distr = "rbinom",
	prob = ifelse(L2[0] == 1, 0.5, 0.1), size = 1)
D <- set.DAG(D)
sim(D, n=10)

#---------------------------------------------------------------------------------------
# EXAMPLE 5: Defining new distribution function 'rbern', defining and passing a custom
# vectorized node function 'customfun'
#---------------------------------------------------------------------------------------
rbern <- function(n, prob) { # defining a bernoulli wrapper based on R rbinom function
  rbinom(n = n, prob = prob, size = 1)
}
customfun <- function(arg, lambda) {
  res <- ifelse(arg == 1, lambda, 0.1)
  res
}
D <- DAG.empty()
D <- D +
node("W1", distr = "rbern", prob = 0.05) +
node("W2", distr = "rbern", prob = customfun(W1, 0.5)) +
node("W3", distr = "rbern", prob = ifelse(W1 == 1, 0.5, 0.1))
D1d <- set.DAG(D, vecfun = c("customfun"))
sim(D1d, n = 10, rndseed = 1)


#---------------------------------------------------------------------------------------
# EXAMPLE 6: Defining latent variables I and U.Y (will be hidden from simulated data)
#---------------------------------------------------------------------------------------
D <- DAG.empty()
D <- D +
  node("I",
    distr = "rcategor.int",
    probs = c(0.1, 0.2, 0.2, 0.2, 0.1, 0.1, 0.1)) +
  node("W1",
    distr = "rnorm",
    mean = ifelse(I == 1, 0, ifelse(I == 2, 3, 10)) + 0.6 * I,
    sd = 1) +
  node("W2",
    distr = "runif",
    min = 0.025*I, max = 0.7*I) +
  node("W3",
    distr = "rbern",
    prob = plogis(-0.5 + 0.7*W1 + 0.3*W2 - 0.2*I)) +
  node("A",
    distr = "rbern",
    prob = plogis(+4.2 - 0.5*W1 + 0.2*W2/2 + 0.2*W3)) +
	node("U.Y", distr = "rnorm", mean = 0, sd = 1) +
  node("Y",
    distr = "rconst",
    const = -0.5 + 1.2*A + 0.1*W1 + 0.3*W2 + 0.2*W3 + 0.2*I + U.Y)
Dset1 <- set.DAG(D, latent.v = c("I", "U.Y"))
sim(Dset1, n = 10, rndseed = 1)

#---------------------------------------------------------------------------------------
# EXAMPLE 7: Multivariate random variables
#---------------------------------------------------------------------------------------
require("mvtnorm")
D <- DAG.empty()
# 2 dimensional normal (uncorrelated), using rmvnorm function from rmvnorm package:
D <- D +
  node(c("X1","X2"), distr = "rmvnorm", asis.params = list(mean = "c(0,1)")) +
# Equivalent, but more natural syntax .() to define the same distribution as above:
  node(c("Y1","Y2"), distr = "rmvnorm", mean = .(c(0,1))) +
# Equivalent to .() is wrapping the expression in eval():
  node(c("Z1","Z2"), distr = "rmvnorm", mean = eval(c(0,1))) +
# Can define a degenerate (rconst) multivariate node:
  node(c("X1.copy", "X2.copy"), distr = "rconst", const = c(X1, X2)) +
# Same as above, but using .() to force direct evaluation:
  node(c("Y1.copy", "Y2.copy"), distr="rconst", const=.(cbind(Y1, Y2)))
Dset1 <- set.DAG(D, verbose = TRUE)
sim(Dset1, n = 10)

D <- DAG.empty()
# Bivariate normal (correlation coef 0.75):
D <- D +
  node(c("X1","X2"), distr = "rmvnorm",
    asis.params = list(mean = "c(0,1)", sigma = "matrix(c(1,0.75,0.75,1), ncol=2)")) +
# Same binariate normal with a more natural syntax .():
  node(c("Y1","Y2"), distr = "rmvnorm",
    mean = .(c(0,1)), sigma = .(matrix(c(1,0.75,0.75,1), ncol=2))) +
# Can use any component of such multivariate nodes when defining future nodes:
  node("A", distr = "rconst", const = 1 - X1)
Dset2 <- set.DAG(D, verbose = TRUE)
sim(Dset2, n = 10)

# Two ways to define the same bivariate uniform copula:
require("copula")
D <- DAG.empty()
D <- D +
  node(c("X1","X2"), distr = "rCopula", copula = .(normalCopula(0.75, dim = 2)))
vecfun.add("qbinom")
# Bivariate binomial derived from previous copula, with same correlation:
D <- D +
  node(c("A.Bin1", "A.Bin2"), distr = "rconst",
    const = c(qbinom(X1, 10, 0.5), qbinom(X2, 15, 0.7)))
Dset3 <- set.DAG(D)
sim(Dset3, n = 10)

# Same as "A.Bin1" and "A.Bin2", but directly using rmvbin function in bindata package:
require("bindata")
D <- DAG.empty()
D <- D +
  node(c("B.Bin1","B.Bin2"), distr = "rmvbin",
    margprob = .(c(0.5, 0.5)),
    bincorr = .(matrix(c(1,0.75,0.75,1), ncol=2)))
Dset4b <- set.DAG(D)
sim(Dset4b, n = 10)

# Time-varying multivar node (3 time-points, 2 dimensional normal)
# plus changing the mean over time:
D <- DAG.empty()
D <- D +
  node(c("X1", "X2"), t = 0:2, distr = "rmvnorm",
    mean = .(c(0,1)) + t,
    sigma = .(matrix(rep(0.75,4), ncol=2)))
Dset5b <- set.DAG(D)
sim(Dset5b, n = 10)

#---------------------------------------------------------------------------------------
# EXAMPLE 8: Combining simcausal non-standard evaluation with .() forced evaluation
#---------------------------------------------------------------------------------------
coefAi <- 1:10
D <- DAG.empty()
D <- D +
  node("A", t = 1, distr = "rbern", prob = 0.5) +
  node("A", t = 2:10, distr = "rconst", const = .(coefAi[t]) * A[t-1])
Dset8 <- set.DAG(D)
sim(Dset8, n = 10)
