

# (EXPERIMENTAL) Estimate node mean target parameters from the simulated observed data
#
# Performs estimation of the mean target parameter using the simulated observational data. Currently only \pkg{ltmle} package can be used for estimation, which has to be installed before calling this function. Please note, this is an experimental feature that hasn't been fully tested yet, \strong{use at your own risk}.
# @param DAG Object specifying the directed acyclic graph for the observed data, must have a well-defined MSM target parameter (\code{set.target.MSM()})
# @param obs_df Simulated observational data
# @param Aname Generic names of the treatment nodes (can be time-varying)
# @param Cname Generic names of the censoring nodes (can be time-varying)
# @param Lnames Generic names of the time-varying covariates (can be time-varying)
# @param package Character vector for R package name to use for estimation. Currenlty only "ltmle" is implemented.
# @param fun Character name for R function name to employ for estimation. Currenlty only "ltmle" is implemented.
# @param ... Additional named arguments that will be passed on to ltmle function
# est.targetE <- function(DAG, obs_df, Anodes="A", Cnodes="C", Lnodes, package="ltmle", fun="ltmle", ...) {
est.targetE <- function(DAG, obs_df, Aname="A", Cname="C", Lnames, package="ltmle", fun="ltmle", ...) {
	outnodes <- attr(DAG, "target")$outnodes
	param_name <- attr(DAG, "target")$param_name
	if (is.null(outnodes$t)) stop("estimation is only implemented for longitudinal data with t defined")
	if (!param_name%in%"E") stop("target parameter has to be defined with set.targetE()...")

	#------------------------------------------------------------
	# getting E params
	#------------------------------------------------------------
	params.E <- attr(DAG, "target")$params.E
	if (length(params.E$interv_nms)>1) stop("Currently cannot run estimation for more than one action at a time")
	actions <- A(DAG)[params.E$interv_nms]
	# print("actions"); print(actions)

	t_all <- attr(obs_df, "tvals")  # all time points actually used in the observed data
	tvec <- outnodes$t
	t_sel <- t_all[1:which(t_all%in%max(tvec))] # time points we only care about (from first to max time-point in target param)

	Ynodes <- outnodes$gen_name%+%"_"%+%t_sel		
	Anodes <- Aname%+%"_"%+%t_sel
	Cnodes <- Cname%+%"_"%+%t_sel
	Lnodes <- t(sapply(Lnames, function(Lname) Lname%+%"_"%+%t_sel[-1]))
	Lnodes <- as.vector(matrix(Lnodes, nrow=1, ncol=ncol(Lnodes)*length(Lnames), byrow=FALSE))
	Nobs <- nrow(obs_df)

	#------------------------------------------------------------
	# evaluating ltmle
	#------------------------------------------------------------
	if (!requireNamespace("ltmle", quietly = TRUE)) {
		stop("ltmle package needed for this function to work. Please install it.", call. = FALSE)
	}

	# VERY IMPORTANT: # the number of attributes and their dimensionality have to match between different actions	
	n_attrs <- length(attr(actions[[1]], "attnames"))
	#------------------------------------------------------------
	# define final ltmle arrays
	regimens_arr <- array(dim = c(Nobs, length(t_sel), length(actions)))
	# loop over actions (regimes) creating counterfactual mtx of A's for each action:
	for (action_idx in seq(actions)) {
	    # I) CREATE COUNTERFACTUAL TREATMENTS & II) CREATE summary.measure that describes each attribute by time + regimen
	    #------------------------------------------------------------    
	    # VERY IMPORTANT:
	    A_mtx_act <- as.matrix(obs_df[,Anodes]) # needs to assign observed treatments and replace the action timepoints with counterfactuals
	    #------------------------------------------------------------    
	    action <- actions[[action_idx]]
	    t_act <- as.integer(attr(action, "acttimes"))    # action-spec. time-points
	    attnames <- attr(action, "attnames")  # action-spec. attribute names
	    attrs <- attr(action, "attrs")        # list of action-spec. attributes

	    #------------------------------------------------------------    
	    # "cbind" action-specific attributes inside the observed data
	    #------------------------------------------------------------    
	    t_act_idx <- which(t_sel%in%t_act)
	    t_chg <- t_sel[t_act_idx]
	    As_chg <- Anodes[t_act_idx] # modify only A's which are defined in this action out of all Anodes
	    t_vec_idx <- which(t_act%in%tvec) # which t's are in the final pooled MSM => need to save the summary measures only for these ts

	    obs_df_attr <- obs_df # add all action attributes to the observed data
	    for (attr_idx in seq(attnames)) { # self-contained loop # grab values of the attributes, # loop over all attributes
	      if (length(attrs[[attnames[attr_idx]]])>1) {
	        attr_i <- attnames[attr_idx]%+%"_"%+%t_chg
	        val_attr_i <- attrs[[attnames[attr_idx]]][t_act_idx]
	      } else {
	        attr_i <- attnames[attr_idx]
	        val_attr_i <- attrs[[attnames[attr_idx]]]
	      }
	      df_attr_i <- matrix(val_attr_i, nrow=Nobs, ncol=length(val_attr_i), byrow=TRUE) # observed data values of the attribute
	      # create the combined data.frame (attrs + O.dat)
	      colnames(df_attr_i) <- attr_i;
	      obs_df_attr <- cbind(data.frame(df_attr_i), obs_df_attr)
	    } # end of loop
	    #------------------------------------------------------------
	    # GENERATING A MATRIX OF COUNTERFACTUAL TREATMENTS
	    for (Achange in As_chg) { # for each A defined in the action, evaluate its value applied to the observed data
	      cur.node <- action[As_chg][[Achange]]
	      t <- cur.node$t
	      newAval <- with(obs_df_attr, { # for static no need to sample from a distr
	        	ANCHOR_VARS_OBSDF <- TRUE
	       		eval_nodeform(as.character(cur.node$dist_params$prob), cur.node)$evaled_expr
	      })
	      if (length(newAval)==1) {
	        newA <- rep(newAval, Nobs)
	      } else {
	        newA <- newAval
	      }
	      A_mtx_act[,which(Anodes%in%Achange)] <- newA
	    } 
	    # resulting matrix A_mtx_act is has all treatments that were defined in that action replaced with their counterfactual values the rest of the treatments are as observed
	    #------------------------------------------------------------
	    regimens_arr[, , action_idx] <- A_mtx_act # add action specific summary measures to the full array
	    #------------------------------------------------------------
	}

	#---------------------------------------------------------------------------------------
	# ALL CENSROING NODES NEED TO BE RECODED FROM 0 to 1 and vice versa for ltmle required censoring coding (0 - censored, 1 - uncensored)
	#---------------------------------------------------------------------------------------
    obs_df[,Cnodes] <- 1-obs_df[,Cnodes]
	#---------------------------------------------------------------------------------------
	# ltmle settings
	#---------------------------------------------------------------------------------------
	stratify_Qg <- FALSE
	survivalOutcome <- is.EFUP(N(DAG)[[Ynodes[length(Ynodes)]]])
	mhte.iptw <- TRUE
	weight.msm <- FALSE

	#---------------------------------------------------------------------------------------
	# If the outcome is survival need to carry forward last observation with doLTCF function
	#---------------------------------------------------------------------------------------
	obs_df <- doLTCF(data=obs_df, outcome=outnodes$gen_name)
	#---------------------------------------------------------------------------------------

	#---------------------------------------------------------------------------------------
	# subset data by time points of interest and remove ID col
	#---------------------------------------------------------------------------------------
	index <- match(Ynodes, names(obs_df))
	obs_df_tsel <- obs_df[,1:index[length(index)]]
	obs_df_tsel <- obs_df_tsel[,!(names(obs_df_tsel)%in%"ID")]

	dprint("head(obs_df_tsel)"); dprint(head(obs_df_tsel))
	dprint("regimens_arr)"); dprint(regimens_arr)

	#---------------------------------------------------------------------------------------
	# run ltmleMSM function
	#---------------------------------------------------------------------------------------
	abar_mat <- regimens_arr[, , 1]
	
	dprint("abar_mat"); dprint(dim(abar_mat)); dprint(head(abar_mat))

	lTMLEres <- ltmle::ltmle(data = obs_df_tsel, Anodes = Anodes, Cnodes = Cnodes, Lnodes = Lnodes,
	    					Ynodes = Ynodes, abar = abar_mat, survivalOutcome = survivalOutcome,
	    					stratify = stratify_Qg, mhte.iptw = mhte.iptw, ...)
	dprint(lTMLEres)

	list(tmleres=summary(lTMLEres, estimator = "tmle"), iptwres=summary(lTMLEres, estimator = "iptw"), lTMLEobj=lTMLEres)
}

# (EXPERIMENTAL) Estimate MSM parameters from simulated observed data
#
# Performs estimation of the MSM target parameters using the simulated observational data. Currently only \pkg{ltmle} package can be used for estimation, which has to be installed before calling this function. Please note, this is an experimental feature that hasn't been fully tested yet, \strong{use at your own risk}.
# @param DAG Object specifying the directed acyclic graph for the observed data, must have a well-defined MSM target parameter (\code{set.target.MSM()})
# @param obs_df Simulated observational data
# @param Aname Generic names of the treatment nodes (can be time-varying)
# @param Cname Generic names of the censoring nodes (can be time-varying)
# @param Lnames Generic names of the time-varying covariates (can be time-varying)
# @param tvec Vector of time points for Y nodes
# @param actions Which actions (regimens) should be used in estimation from the observed simulated data. If NULL then all actions that were defined in DAG will be considered.
# @param package Character vector for R package name to use for estimation. Currently only "ltmle" is implemented.
# @param fun Character name for R function name to employ for estimation. Currently only "ltmleMSM" is implemented.
# @param ... Additional named arguments that will be passed on to ltmleMSM function
est.targetMSM <- function(DAG, obs_df, Aname="A", Cname="C", Lnames, Ytvec, ACLtvec, actions=NULL, package="ltmle", fun="ltmleMSM", ...) {
# est.targetMSM <- function(DAG, obs_df, Anodes="A", Cnodes="C", Lnodes, actions=NULL, package="ltmle", fun="ltmleMSM", ...) {
	outnodes <- attr(DAG, "target")$outnodes
	param_name <- attr(DAG, "target")$param_name
	if (is.null(outnodes$t)) stop("estimation is only implemented for longitudinal data with t defined")
	if (!param_name%in%"MSM") stop("estimation is only implemented for MSM target parameters")

	if (is.null(actions)) {
		message("actions argument underfined, using all available actions")
		actions <- A(DAG)
	}

	t_all <- attr(obs_df, "tvals")  # all time points actually used in the observed data
	# print("t_all"); print(t_all)

	tvec <- outnodes$t
	
	# OS: REPLACED WITH
	t_sel <- ACLtvec
	# t_sel <- t_all[1:which(t_all%in%max(tvec))] # time points we only care about (from first to max time-point in target param)

	# ltmle allows for pooling Y's over smaller subset of t's (for example t=(2:8))
	# in this case summary measures HAVE TO MATCH the dimension of finYnodes, not t_sel
	# currently this is not supported, thus, if tvec is a subset of t_sel this will cause an error
	# finYnodes <- outnodes$gen_name%+%"_"%+%tvec
	finYnodes <- outnodes$gen_name%+%"_"%+%Ytvec

	# NOTE: Fixing the issue when Y_0 doesn't exist. Note, this is probably not the best approach to do this
	# Ynodes <- outnodes$gen_name%+%"_"%+%t_sel
	Ynodes <- outnodes$gen_name%+%"_"%+%Ytvec

	Anodes <- Aname%+%"_"%+%ACLtvec
	Cnodes <- Cname%+%"_"%+%ACLtvec
	Lnodes <- t(sapply(Lnames, function(Lname) Lname%+%"_"%+%ACLtvec[-1]))
	Lnodes <- as.vector(matrix(Lnodes, nrow=1, ncol=ncol(Lnodes)*length(Lnames), byrow=FALSE))

	# Anodes <- Aname%+%"_"%+%t_sel
	# Cnodes <- Cname%+%"_"%+%t_sel
	# Lnodes <- t(sapply(Lnames, function(Lname) Lname%+%"_"%+%t_sel[-1]))
	# Lnodes <- as.vector(matrix(Lnodes, nrow=1, ncol=ncol(Lnodes)*length(Lnames), byrow=FALSE))

	Nobs <- nrow(obs_df)

	#------------------------------------------------------------
	# getting MSM params
	#------------------------------------------------------------
  	params.MSM <- attr(DAG, "target")$params.MSM
	working.msm <- params.MSM$form
	msm.family <- params.MSM$family
	if (params.MSM$hazard) stop("ltmleMSM cannot estimate hazard MSMs...")

	#------------------------------------------------------------
	# evaluating ltmle
	#------------------------------------------------------------
	# require('ltmle')
	if (!requireNamespace("ltmle", quietly = TRUE)) {
		stop("ltmle package needed for this function to work. Please install it.", call. = FALSE)
	}

	# VERY IMPORTANT: # the number of attributes and their dimensionality have to match between different actions	
	n_attrs <- length(attr(actions[[1]], "attnames"))
	#------------------------------------------------------------
	# define final ltmle arrays
	#OS: REPLACED WITH
	regimens_arr <- array(dim = c(Nobs, length(ACLtvec), length(actions)))
	# regimens_arr <- array(dim = c(Nobs, length(t_sel), length(actions)))

	#OS: REPLACED WITH
	summeas_arr <- array(dim = c(length(actions), (n_attrs+1), length(Ytvec)))
	# summeas_arr <- array(dim = c(length(actions), (n_attrs+1), length(t_sel)))
	# summeas_arr <- array(dim = c(length(actions), (n_attrs+1), length(tvec)))

	# loop over actions (regimes) creating counterfactual mtx of A's for each action:
	for (action_idx in seq(actions)) {
	    # I) CREATE COUNTERFACTUAL TREATMENTS & II) CREATE summary.measure that describes each attribute by time + regimen
	    #------------------------------------------------------------    
	    # VERY IMPORTANT:
	    A_mtx_act <- as.matrix(obs_df[,Anodes]) # needs to assign observed treatments and replace the action timepoints with counterfactuals
	    #------------------------------------------------------------    
	    action <- actions[[action_idx]]
	    t_act <- as.integer(attr(action, "acttimes"))    # action-spec. time-points
	    attnames <- attr(action, "attnames")  # action-spec. attribute names
	    attrs <- attr(action, "attrs")        # list of action-spec. attributes

	    # time points for which we need to evaluate the counterfactual treatment assignment as determined by action:
	    #------------------------------------------------------------    
	    # VERY IMPORTANT:
	    # Action t's need always be the same subset of t_sel (outcome-based times), otherwise we are in big trouble
	    t_act_idx <- which(t_sel%in%t_act)
	    t_chg <- t_sel[t_act_idx]
	    As_chg <- Anodes[t_act_idx] # modify only A's which are defined in this action out of all Anodes

	    #------------------------------------------------------------
	    # creates summary measure array that is of dimension (length(t_chg)) - time-points only defined for this action
	    # Q: is this correct???? DO we instead need to make it of dimension t_sel???

	    # summeas_attr <- matrix(nrow=length(attnames), ncol=length(t_chg))
	    t_vec_idx <- which(t_act%in%tvec) # which t's are in the final pooled MSM => need to save the summary measures only for these ts
	    
	    # print("t_act"); print(t_act)
	    # print("t_act_idx"); print(t_act_idx)

	    summeas_attr <- matrix(nrow=length(attnames), ncol=length(tvec))
	    # summeas_attr <- matrix(nrow=length(attnames), ncol=t_vec_idx)

	    #------------------------------------------------------------
	    # extract values of terms in MSM formula: get all attribute values from +action(...,attrs)
	    #------------------------------------------------------------
	    obs_df_attr <- obs_df # add all action attributes to the observed data
	    for (attr_idx in seq(attnames)) { # self-contained loop # grab values of the attributes, # loop over all attributes
	      if (length(attrs[[attnames[attr_idx]]])>1) {
	        attr_i <- attnames[attr_idx]%+%"_"%+%t_chg
	        val_attr_i <- attrs[[attnames[attr_idx]]][t_act_idx]
	      } else {
	        attr_i <- attnames[attr_idx]
	        val_attr_i <- attrs[[attnames[attr_idx]]]
	      } 
	      # summary measures, for each action/measure
	      # print(dim(summeas_attr))

	      summeas_attr[attr_idx,] <- matrix(val_attr_i, nrow=1, ncol=length(t_chg))[,t_vec_idx]

	      # observed data values of the attribute
	      df_attr_i <- matrix(val_attr_i, nrow=Nobs, ncol=length(val_attr_i), byrow=TRUE)
	      # create the combined data.frame (attrs + O.dat)
	      colnames(df_attr_i) <- attr_i; obs_df_attr <- cbind(data.frame(df_attr_i), obs_df_attr)
	    } # end of loop
	    summeas_attr <- rbind(summeas_attr, t_chg[t_vec_idx])
	    rownames(summeas_attr) <- c(attnames, "t")

	    #------------------------------------------------------------
	    summeas_arr[action_idx, , ] <- summeas_attr # add action specific summary measures to the full array
	    dimnames(summeas_arr)[[2]] <- rownames(summeas_attr)
	    #------------------------------------------------------------
	    # GENERATING A MATRIX OF COUNTERFACTUAL TREATMENTS
	    for (Achange in As_chg) { # for each A defined in the action, evaluate its value applied to the observed data
	      cur.node <- action[As_chg][[Achange]]
	      t <- cur.node$t

	      # print("cur.node"); print(cur.node$name)
	      # print("cur.node$dist_params"); print(cur.node$dist_params)
	      # print("t"); print(t)

	      newAval <- with(obs_df_attr, { # for static no need to sample from a distr
	        ANCHOR_VARS_OBSDF <- TRUE
	        # print("cur.node"); print(cur.node)
	        # print("cur.node$dist_params$prob"); print(cur.node$dist_params$prob)
	        eval_nodeform(as.character(cur.node$dist_params$prob), cur.node)$evaled_expr
	      })
	      # print("newAval"); print(newAval)

	      if (length(newAval)==1) {
	        newA <- rep(newAval, Nobs)
	      } else {
	        newA <- newAval
	      }
	      A_mtx_act[,which(Anodes%in%Achange)] <- newA
	    }

	    # print("summeas_arr[action_idx, , ]"); print(summeas_arr[action_idx, , ])
	    # print("A_mtx_act"); print(A_mtx_act)
	    
	    # A_mtx_act[is.na(A_mtx_act)] <- 1
	    # print(A_mtx_act)

	    # resulting matrix A_mtx_act is has all treatments that were defined in that action replaced with their counterfactual values the rest of the treatments are as observed
	    #------------------------------------------------------------
	    regimens_arr[, , action_idx] <- A_mtx_act # add action specific summary measures to the full array
	    #------------------------------------------------------------
	}

	#---------------------------------------------------------------------------------------
	# ALL CENSROING NODES NEED TO BE RECODED FROM 0 to 1 and vice versa for ltmle required censoring coding (0 - censored, 1 - uncensored)
	#---------------------------------------------------------------------------------------
    obs_df[,Cnodes] <- 1-obs_df[,Cnodes]

	#---------------------------------------------------------------------------------------
	# ltmle settings
	#---------------------------------------------------------------------------------------
	pooledMSM <- FALSE
	stratify_Qg <- FALSE
	survivalOutcome <- is.EFUP(N(DAG)[[Ynodes[length(Ynodes)]]])
	iptw.only <- FALSE
	mhte.iptw <- TRUE
	weight.msm <- FALSE

	#---------------------------------------------------------------------------------------
	# If the outcome is survival need to carry forward last observation with doLTCF function
	#---------------------------------------------------------------------------------------
	obs_df <- doLTCF(data=obs_df, outcome=outnodes$gen_name)
	#---------------------------------------------------------------------------------------

	#---------------------------------------------------------------------------------------
	# subset data by time points of interest and remove ID col
	#---------------------------------------------------------------------------------------
	index <- match(Ynodes, names(obs_df))
	maxindx <- index[length(index)]

	# print("maxindx"); print(maxindx)
	
	obs_df_tsel <- obs_df[,1:maxindx]
	# obs_df_tsel <- obs_df
	# removeNames <- names(obs_df)[(maxindx+1):length(names(obs_df))]
	# # print("removeNames"); print(removeNames)
	# Anodes <- Anodes[!(Anodes%in%removeNames)]
	# Cnodes <- Cnodes[!(Cnodes%in%removeNames)]
	# Lnodes <- Lnodes[!(Lnodes%in%removeNames)]

	obs_df_tsel <- obs_df_tsel[,!(names(obs_df_tsel)%in%"ID")]

	# print("head(obs_df_tsel)"); print(head(obs_df_tsel))
	# print("Anodes"); print(Anodes)
	# print("Cnodes"); print(Cnodes)
	# print("Lnodes"); print(Lnodes)
	# print("Ynodes"); print(Ynodes)

	# # print("summeas_arr"); print(summeas_arr)
	# print("regimens_arr)"); print(regimens_arr[,,1])

	# print("regimens_arr"); print(regimens_arr)
	# print("summeas_arr"); print(summeas_arr)

	#---------------------------------------------------------------------------------------
	# run ltmleMSM function
	#---------------------------------------------------------------------------------------	
	# lTMLEres <- ltmle::ltmleMSM(data = obs_df_tsel, Anodes = Anodes, Cnodes = Cnodes, Lnodes = Lnodes,
	#     Ynodes = Ynodes, final.Ynodes = finYnodes, regimes = regimens_arr, working.msm = working.msm, 
	#     summary.measures = summeas_arr, pooledMSM = pooledMSM, survivalOutcome = survivalOutcome,
	#     stratify = stratify_Qg, mhte.iptw = mhte.iptw, iptw.only = iptw.only, weight.msm=weight.msm, ...)
	# dprint(lTMLEres)
	# list(tmleres=summary(lTMLEres, estimator = "tmle"), iptwres=summary(lTMLEres, estimator = "iptw"), lTMLEobj=lTMLEres, 
		# regimens_arr=regimens_arr, summeas_arr=summeas_arr)

	list(regimens_arr=regimens_arr, summeas_arr=summeas_arr)

}

