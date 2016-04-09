#---------------------------------------------------------------------------------------
# DAG with time-varying outcomes (survival outcome)
#---------------------------------------------------------------------------------------
# Define longitudinal data structure over 6 time-points t=(0:5)
t_end <- 5
D <- DAG.empty()
D <- D + node("L2", t=0, distr="rbern", prob=0.05)
D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1 & L2[0]==0, 0.5,
ifelse(L1[0]==0 & L2[0]==0, 0.1,
ifelse(L1[0]==1 & L2[0]==1, 0.9, 0.5))))
D <- D + node("A2", t=0, distr="rbern", prob=0, order=4, EFU=TRUE)
D <- D + node("Y",  t=0, distr="rbern",
prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)),
EFU=TRUE)
D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1,
ifelse(L2[t-1]==1, 0.9,
min(1,0.1 + t/16))))
D <- D + node("A1", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 1,
ifelse(L1[0]==1 & L2[0]==0, 0.3,
ifelse(L1[0]==0 & L2[0]==0, 0.1,
ifelse(L1[0]==1 & L2[0]==1, 0.7,
0.5)))))
D <- D + node("A2", t=1:t_end, distr="rbern", prob=0, EFU=TRUE)
D <- D + node( "Y",  t=1:t_end, distr="rbern",
prob=plogis(-6.5 + L1[0] + 4*L2[t] + 0.05*sum(I(L2[0:t]==rep(0,(t+1))))),
EFU=TRUE)
D <- set.DAG(D)

# Add two dynamic actions (indexed by values of the parameter theta={0,1})
# Define intervention nodes
act_t0_theta <- node("A1",t=0, distr="rbern", prob=ifelse(L2[0] >= theta,1,0))
act_tp_theta <- node("A1",t=1:t_end, distr="rbern",
prob=ifelse(A1[t-1]==1,1,ifelse(L2[t] >= theta,1,0)))
# Add two actions to current DAG object
D <- D + action("A1_th0", nodes=c(act_t0_theta, act_tp_theta), theta=0)
D <- D + action("A1_th1", nodes=c(act_t0_theta, act_tp_theta), theta=1)

#---------------------------------------------------------------------------------------
# MSM EXAMPLE 1: Modeling survival over time
#---------------------------------------------------------------------------------------
# Modeling pooled survival Y_t over time as a projection on the following working
# logistic model:
msm.form <- "Y ~ theta + t + I(theta*t)"
D <- set.targetMSM(D, outcome="Y", t=0:5, formula=msm.form, family="binomial",
hazard=FALSE)
MSMres <- eval.target(D, n=1000)
MSMres$coef

#---------------------------------------------------------------------------------------
# MSM EXAMPLE 2: Modeling survival over time with exposure-based summary measures
#---------------------------------------------------------------------------------------
# Now we want to model Y_t by adding a summary measure covariate defined as mean
# exposure A1 from time 0 to t;
# Enclosing any term inside S() forces its evaluation in the environment
# of the full (counterfactual) data.
msm.form_sum <- "Y ~ theta + t + I(theta*t) + S(mean(A1[0:t]))"
D <- set.targetMSM(D, outcome="Y", t=0:5, formula=msm.form_sum, family="binomial",
hazard=FALSE)
MSMres <- eval.target(D, n=1000)
MSMres$coef
