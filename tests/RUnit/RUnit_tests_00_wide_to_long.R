
test.long.wide.simobs <- function() {
    library("simcausal"); options(simcausal.verbose=FALSE)
    t_end <- 5
    D <- DAG.empty() +
      node("W", distr="rbern", prob=0.05) +
      node("L1", t=0:t_end, distr="rbern", prob=0.25) +
      node("L2", t=0:t_end, distr="rconst", const=L1[t] + W) +
      node("Y",  t=0:t_end,  distr="rbern", prob=plogis(-6.5 + L1[t] + 2*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))), EFU=TRUE)
    Dset <- set.DAG(D)

    Odat1 <- sim(Dset, n=500, wide = TRUE, rndseed = 123)
    checkTrue(all.equal(
      names(Odat1),
      c("ID", "W", "L1_0", "L2_0", "Y_0", "L1_1", "L2_1", "Y_1", "L1_2", "L2_2", "Y_2", "L1_3", "L2_3", "Y_3", "L1_4", "L2_4", "Y_4", "L1_5", "L2_5", "Y_5")
      ))

    Odat1b <- simobs(Dset, n=500, wide = TRUE, rndseed = 123)
    checkTrue(all.equal(Odat1, Odat1b))

    Odat1 <- sim(Dset, n=500, wide = FALSE, rndseed = 123)
    checkTrue(all.equal(
      names(Odat1),
      c("ID", "W", "t", "L1", "L2", "Y")
      ))

    Odat1b <- simobs(Dset, n=500, wide = FALSE, rndseed = 123)
    checkTrue(all.equal(Odat1, Odat1b))
}