#---------------------------------------------------------------------------------------
# EXAMPLE 1: Showing two equivalent ways of defining an action for a simple DAG
#---------------------------------------------------------------------------------------

D <- DAG.empty()
D <- D + node(name="W1", distr="rbern", prob=plogis(-0.5))
D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 + 0.5*W1+ 0.5*W2))
Dset <- set.DAG(D)

# Syntax '+ action': define two actions, intervening on node "A", imputing order
Dset <- Dset + action("A0", nodes=node("A", distr="rbern", prob=0))
Dset <- Dset + action("A1", nodes=node("A", distr="rbern", prob=1))

# Equivalent syntax 'add.action': define two actions, intervening on node "A"
Dset <- add.action(Dset, "A0", nodes=node("A", distr="rbern", prob=0))
Dset <- add.action(Dset, "A1", nodes=node("A", distr="rbern", prob=1))

#---------------------------------------------------------------------------------------
# EXAMPLE 2: Adding named attributes that define (index) the action.
# Define intervention on A that is conditional on W1 crossing some threshold theta
#---------------------------------------------------------------------------------------

# Redefining node W1 as uniform [0,1]
D <- DAG.empty()
D <- D + node(name="W1", distr="runif", min=0, max=1)
D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 + 0.5*W1+ 0.5*W2))
Dset <- set.DAG(D)

# Define a node that is indexed by unknown variable theta
actN <- node("A",distr="rbern",prob=ifelse(W1 >= theta,1,0))
# Define 3 actions for theta=0.1, 0.5, 0.9
Dset <- Dset + action("A1th0.1", nodes = actN, theta = 0.1)
Dset <- Dset + action("A1th0.5", nodes = actN, theta = 0.5)
Dset <- Dset + action("A1th0.9", nodes = actN, theta = 0.9)

# Simulate 50 observations per each action above
simfull(A(Dset), n=50)

#---------------------------------------------------------------------------------------
# EXAMPLE 3: Time-varying action attributes for longitudinal DAG
#---------------------------------------------------------------------------------------
# Define longitudinal data structure over 6 time-points t=(0:5) with survival outcome "Y"
t_end <- 5
D <- DAG.empty()
D <- D + node("L2", t=0, distr="rbern", prob=0.05)
D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1, 0.5, 0.1))
D <- D + node("Y",  t=0, distr="rbern",
                prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), EFU=TRUE)
D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, 0.9))
D <- D + node("A1", t=1:t_end, distr="rbern",
              prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, 0.5)))
D <- D + node("Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5+L1[0]+4*L2[t]), EFU=TRUE)
D <- set.DAG(D)

#---------------------------------------------------------------------------------------
# Dynamic actions indexed by constant value of parameter theta={0,1})
#---------------------------------------------------------------------------------------
# Define time-varying node A1: sets A1 to 1 if L2 at t is >= theta
actN_A1 <- node("A1",t=0:t_end, distr="rbern", prob=ifelse(L2[t] >= theta,1,0))

# Define two actions, indexed by fixed values of theta={0,1}
D_act <- D + action("A1_th0", nodes=actN_A1, theta=0)
D_act <- D_act + action("A1_th1", nodes=actN_A1, theta=1)

# Simulate 50 observations for per each action above
simfull(simcausal::A(D_act), n=50)

#---------------------------------------------------------------------------------------
# Dynamic actions indexed by time-varying parameter theta[t]
#---------------------------------------------------------------------------------------
# This defines an action node with threshold theta varying in time (note syntax theta[t])
actN_A1 <- node("A1",t=0:t_end, distr="rbern", prob=ifelse(L2[t] >= theta[t],1,0))

# Now define 3 actions that are indexed by various values of theta over time
D_act <- D + action("A1_th_const0", nodes=actN_A1, theta=rep(0,(t_end+1)))
D_act <- D_act + action("A1_th_var1", nodes=actN_A1, theta=c(0,0,0,1,1,1))
D_act <- D_act + action("A1_th_var2", nodes=actN_A1, theta=c(0,1,1,1,1,1))

# Simulate 50 observations for per each action above
simfull(simcausal::A(D_act), n=50)
