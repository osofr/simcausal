t_end <- 10
lDAG <- DAG.empty()
lDAG <- lDAG + 
	node(name = "L2", t = 0, distr = "rconst", const = 0) + 
	node(name = "A1", t = 0, distr = "rconst", const = 0) + 
	node(name = "L2", t = 1:t_end, distr = "rbern", 
 	prob = ifelse(A1[t - 1]  ==  1, 0.1, 
 			ifelse(L2[t-1] == 1, 0.9, 
 			  min(1,0.1 + t/.(t_end))))) + 
	node(name = "A1", t = 1:t_end, distr = "rbern", 
 	prob = ifelse(A1[t - 1]  ==  1, 1, 
 			 ifelse(L2[0] == 0, 0.3, 
			  ifelse(L2[0] == 0, 0.1, 
			   ifelse(L2[0] == 1, 0.7, 0.5))))) + 
	node(name = "Y", t = 1:t_end, distr = "rbern", 
 	prob = plogis(-6.5 + 4 * L2[t] + 0.05 * sum(I(L2[0:t] == rep(0,(t + 1))))), 
 	EFU = TRUE)
lDAG <- set.DAG(lDAG)
#---------------------------------------------------------------------------------------
# EXAMPLE 1. No forward imputation.
#---------------------------------------------------------------------------------------
Odat.wide <- sim(DAG = lDAG, n = 1000, rndseed = 123)
Odat.wide[c(21,47), 1:18]
Odat.wideLTCF <- sim(DAG = lDAG, n = 1000, LTCF = "Y", rndseed = 123)
Odat.wideLTCF[c(21,47), 1:18]
#---------------------------------------------------------------------------------------
# EXAMPLE 2. With forward imputation.
#---------------------------------------------------------------------------------------
Odat.wideLTCF2 <- doLTCF(data = Odat.wide, LTCF = "Y")
Odat.wideLTCF2[c(21,47), 1:18]
all.equal(Odat.wideLTCF, Odat.wideLTCF2)
