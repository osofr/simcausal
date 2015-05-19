

###################################################################
# DAG Constructors
###################################################################
#' Initialize an empty DAG object
#' @export
DAG.empty <- function() {
  emptyDAG <- list()
  class(emptyDAG) <- "DAG"
  emptyDAG
}

#' @export
`+.DAG` <- function(obj1, obj2) {
  if (!is.DAG(obj1) && is.DAG(obj2)) {
    tmp <- obj1
    obj1 <- obj2
    obj2 <- tmp
  }
  if ("DAG.action" %in% class(obj2)) {
    ## Adding action, possibly with attributes
    ## Option 1: Non-named argument defines the action name
    ## Option 2: Non-named argument defines the nodes
    # if (is.null(names(obj2))) {
    #   toadd <- unlist(obj2, recursive=FALSE)
    #   attr <- list()
    # } else {
      # toadd <- unlist(obj2[names(obj2)==""])
    name <- unlist(obj2[names(obj2)==""], recursive=FALSE)
    if (length(name)>1) stop("only one unnamed argument can be specified")
    if (length(name)==0) name <- obj2[[which(names(obj2)%in%"name")]]
    if (length(name)==0) stop("name argument for action must be specified")
    # nodes <- unlist(obj2[names(obj2)%in%"nodes"])
    # nodes <- unlist(obj2[names(obj2)%in%"nodes"], recursive=FALSE)
    nodes <- obj2[names(obj2)%in%"nodes"][[1]]
    attr <- obj2[(names(obj2)!="") & (!(names(obj2) %in% c("name", "nodes")))]
    dprint("name"); dprint(name)
    dprint("nodes"); dprint(nodes)
    dprint("attr"); dprint(attr)
    res <- add.action(DAG=obj1, name=name, nodes=nodes, attr=attr)
  } else if ("DAG.nodelist" %in% class(obj2)) {
    # res <- c(obj1, obj2)
    # class(res) <- "DAG"
    res <- add.nodes(DAG=obj1, nodes=obj2)
  } else {
    stop("Cannot add unknown type to DAG")
  }
  res
}

# @export
# `+.DAG.nodelist` <- function(obj1, obj2) {
#   if (!is.DAG(obj1) && is.DAG(obj2)) {
#     return(`+.DAG`(obj2,obj1))
#   }
#   if (("DAG.nodelist" %in% class(obj1)) && ("DAG.nodelist" %in% class(obj2))) {
#     return(append(obj1, obj2))
#   } else {
#     stop("Cannot add unknown type to DAG")
#   }
# }

###################################################################
# Node constructor
# 1) If some of the nodes in "nodes" already exist in DAG then replace them instead of adding
# 2) If order is not defined in "nodes" then infer where to add the node and calculate the orders at set.DAG()
# if DAG.nodelist object nodes consists of just one node, increment the order by 1
###################################################################
#' Adding Nodes to DAG
#'
#' Adding nodes to a growing DAG object, as in \code{DAG + node(...)}. Use either syntax \code{DAG + node} or \code{add.action(DAG, node)}, both give identical results.
#' 
#' @param DAG DAG object
#' @param nodes A node or several nodes returned from a call to \code{node} function. If the node(s) under same name(s) already exist, the old node(s) get overwritten.
#' @return An updated DAG object with new nodes
#'
#' @export
add.nodes <- function(DAG, nodes) {
  if (!is.DAG(DAG)) {
    stop("Not a DAG object")
  }
  if (is.DAGlocked(DAG)) {
    stop("DAG object is locked: nodes in this DAG cannot be modified or added after set.DAG()")
  }

  # DAG_names <- unlist(Nattr(DAG, "name"))  # get DAG node names
  modDAG <- DAG
  # print("modDAG"); print(modDAG)
  # print("nodes"); print(nodes)
  for (node_idx in seq(nodes)) { # loop over each node in DAG.nodelist and add it or overwrite existing node
    DAG_names <- unlist(Nattr(modDAG, "name"))  # get DAG node names
    node_nm <- nodes[[node_idx]]$name
    gnode_nm <- as.character(unlist(strsplit(node_nm, "_"))[1]) # generic node name (without t)

    checkexist <- (node_nm%in%DAG_names) # check if the node under the same name already exists
    checkexistgen <- (gnode_nm%in%DAG_names) # check if the generic node under the same name already exists

    if ((!checkexist) & checkexistgen) { # the TV node doesn't exist yet but the generic (nonTV) already does
    # give warning and delete old node, add new ones
      gnode_idx <- which(DAG_names%in%gnode_nm)
      modDAG <- modDAG[-gnode_idx]
      class(modDAG) <- "DAG"
      warning("existing non-time-varying node "%+% gnode_nm %+% " was overwritten with a time-varying node")
    }

    if (!checkexist) {  # this node doesn't exist yet in the DAG, new node has to be added
      node_insert <- nodes[node_idx]
      t_insert <- nodes[[node_idx]]$t
      t_idx_all <- Nattr(modDAG, "t") # a list of t values from current DAG (including NULLs)
      t_idx_miss <- sapply(t_idx_all, is.null) # finding all nodes where t is undefined (null)
      t_idx_miss <- which(t_idx_miss%in%TRUE)
      if (is.null(t_insert)) {
        # t is undefined in the new node, hence insert it after last position where is.null(t)
        t_idx_poslast <- t_idx_miss[length(t_idx_miss)]
        # IMPORTANT: FORCE ALL NODES TO HAVE t defined (!is.null(node$t)) AFTER t has been defined for ANY prior node 
        t_idx_nomiss <- sapply(t_idx_all, function(tnode) !is.null(tnode))
        n_tnomiss <- sum(t_idx_nomiss%in%TRUE)
        if (n_tnomiss>0) stop("cannot define nodes with missing t after nodes with t non-missing were already defined")
      } else { # find the last occurence of a node with t=t_insert in modDAG and insert node after
        t_idx_poslast <- which(t_idx_all%in%t_insert)
        t_idx_poslast <- t_idx_poslast[length(t_idx_poslast)]
        t_idx_more <- sapply(t_idx_all, function(t) !is.null(t)&&(t > t_insert))
        if (length(t_idx_more)>0 && length(t_idx_poslast)<1) {
          t_idx_poslast <- which(t_idx_more)[1]-1
        }
      }
      if ((length(t_idx_poslast)!=1) || (t_idx_poslast < 0) || (is.na(t_idx_poslast))) {
          t_idx_poslast <- length(t_idx_all)
      }
      modDAG <- append(modDAG, node_insert, t_idx_poslast)
      class(modDAG) <- "DAG"
    } else { # this node name already exist in the DAG, existing node is overwritten
      modDAG <- modDAGnode(modDAG, nodes[[node_idx]])
      warning("existing node "%+% nodes[[node_idx]]$name %+% " was modified")
    } 
  }
  return(modDAG)
}

###################################################################
# Action constructor
###################################################################
#' Define Actions
#'
#' Define and add new action (intervention) to the existing DAG object. Use either syntax \code{DAG + action} or \code{add.action}, both give identical results, see examples and details.
#' 
#' In addition to the action name and list of action nodes, both of these functions accept arbitrary named attributes (as additional arguments which must be given a name).
#' This additional attributes can be used to simplify specification of dynamic regimes (actions that depend on the past observed covariates).
#' 
#' The formula of the intervention node is allowed to contain undefined variables, as long as those are later defined as a named argument to \code{action}. 
#' 
#' In Example 2 below, \code{node("A",..., mean=ifelse(W1 >= theta,1,0))}, 
#' defines the mean of the node "A" as a function of some undefined variable \code{theta}, setting \code{A} to 1 if the baseline node \code{W1} is above or equal to \code{theta} and 0 vice versa.
#' One specifies actual values of \code{theta} while defining a new action, possible creating a series of actions, each indexed by a different value of \code{theta}.
#' A new action can be defined with \code{D<-D+action("A1th0.1", nodes=actN, theta=0.1)}.
#' 
#' Note that any name can be used in place of \code{theta}. This attribute variable can appear anywhere inside the node distribution formula.
#' Finally, the attribute variable can also be time varying and, just like with DAG nodes, can be indexed by square bracket notation, \code{theta[t]}. See Example 3 for defining time-varying attributes.
#'
#' @param DAG DAG object
#' @param name Unique name of the action
#' @param nodes A list of node objects that defines the action on the DAG (replaces the distributions of the corresponding nodes in DAG)
#' @param ... Additional named attributes defining / indexing the action
#' @param attr Additional named attributes defining / indexing the action
#' @return A DAG object with saved action
#' @examples
#'
#'#---------------------------------------------------------------------------------------
#'# EXAMPLE 1: Showing two equivalent ways of defining an action for a simple DAG
#'#---------------------------------------------------------------------------------------
#'
#'D <- DAG.empty()
#'D <- D + node(name="W1", distr="rbern", prob=plogis(-0.5))
#'D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 + 0.5*W1+ 0.5*W2))
#'D <- set.DAG(D)
#'
#'# Syntax '+ action': define two actions, intervening on node "A", imputing order
#'D <- D + action("A0", nodes=node("A", distr="rbern", prob=0))
#'D <- D + action("A1", nodes=node("A", distr="rbern", prob=1))
#'
#'# Equivalent syntax 'add.action': define two actions, intervening on node "A"
#'D <- add.action(D, "A0", nodes=node("A", distr="rbern", prob=0))
#'D <- add.action(D, "A1", nodes=node("A", distr="rbern", prob=1))
#'
#'#---------------------------------------------------------------------------------------
#'# EXAMPLE 2: Adding named attributes that define (index) the action.
#'# Define intervention on A that is conditional on W1 crossing some threshold theta
#'#---------------------------------------------------------------------------------------
#'
#'# Redefining node W1 as uniform [0,1]
#'D <- DAG.empty()
#'D <- D + node(name="W1", distr="runif", min=0, max=1)
#'D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 + 0.5*W1+ 0.5*W2))
#'D <- set.DAG(D)
#'
#'# Define a node that is indexed by unknown variable theta
#'actN<-node("A",distr="rbern",prob=ifelse(W1 >= theta,1,0))
#'# Define 3 actions for theta=0.1, 0.5, 0.9
#'D <- D + action("A1th0.1", nodes=actN, theta=0.1)
#'D <- D + action("A1th0.5", nodes=actN, theta=0.5)
#'D <- D + action("A1th0.9", nodes=actN, theta=0.9)
#'
#'# Simulate 50 observations per each action above
#'simfull(A(D), n=50)
#'
#'#---------------------------------------------------------------------------------------
#'# EXAMPLE 3: Time-varying action attributes for longitudinal DAG
#'#---------------------------------------------------------------------------------------
#'# Define longitudinal data structure over 6 time-points t=(0:5) with survival outcome "Y"
#'t_end <- 5
#'D <- DAG.empty()
#'D <- D + node("L2", t=0, distr="rbern", prob=0.05)
#'D <- D + node("L1", t=0, distr="rbern", prob=ifelse(L2[0]==1,0.5,0.1))
#'D <- D + node("A1", t=0, distr="rbern", prob=ifelse(L1[0]==1, 0.5, 0.1))
#'D <- D + node("Y",  t=0, distr="rbern", 
#'                prob=plogis(-6.5 + L1[0] + 4*L2[0] + 0.05*I(L2[0]==0)), EFU=TRUE)
#'D <- D + node("L2", t=1:t_end, distr="rbern", prob=ifelse(A1[t-1]==1, 0.1, 0.9))
#'D <- D + node("A1", t=1:t_end, distr="rbern", 
#'              prob=ifelse(A1[t-1]==1, 1, ifelse(L1[0]==1 & L2[0]==0, 0.3, 0.5)))
#'D <- D + node("Y",  t=1:t_end, distr="rbern", prob=plogis(-6.5+L1[0]+4*L2[t]), EFU=TRUE)
#'D <- set.DAG(D)
#'
#'#---------------------------------------------------------------------------------------
#'# Dynamic actions indexed by constant value of parameter theta={0,1})
#'#---------------------------------------------------------------------------------------
#'# Define time-varying node A1: sets A1 to 1 if L2 at t is >= theta
#'actN_A1 <- node("A1",t=0:t_end, distr="rbern", prob=ifelse(L2[t] >= theta,1,0))
#'
#'# Define two actions, indexed by fixed values of theta={0,1}
#'D_act <- D + action("A1_th0", nodes=actN_A1, theta=0)
#'D_act <- D_act + action("A1_th1", nodes=actN_A1, theta=1)
#'
#'# Simulate 50 observations for per each action above
#'simfull(A(D_act), n=50)
#'
#'#---------------------------------------------------------------------------------------
#'# Dynamic actions indexed by time-varying parameter theta[t]
#'#---------------------------------------------------------------------------------------
#'# This defines an action node with threshold theta varying in time (note syntax theta[t])
#'actN_A1 <- node("A1",t=0:t_end, distr="rbern", prob=ifelse(L2[t] >= theta[t],1,0))
#'
#'# Now define 3 actions that are indexed by various values of theta over time
#'D_act <- D + action("A1_th_const0", nodes=actN_A1, theta=rep(0,(t_end+1)))
#'D_act <- D_act + action("A1_th_var1", nodes=actN_A1, theta=c(0,0,0,1,1,1))
#'D_act <- D_act + action("A1_th_var2", nodes=actN_A1, theta=c(0,1,1,1,1,1))
#'
#'# Simulate 50 observations for per each action above
#'simfull(A(D_act), n=50)
#'
#' @export
add.action <- function(DAG, name, nodes, ..., attr=list()) {
  if (!is.DAG(DAG)) {
    stop("Not a DAG object")
  }
  # collect all attributes (must be named)
  attrs <- list(...)
  attrs <- append(attrs, attr)
  attnames <- names(attrs)
  if (length(attrs) != 0 && (is.null(attnames) || any(attnames==""))) {
    stop("please specify name for each attribute")
  }
  if (missing(name)) stop("please specify an action name")
  if (!is.character(name) || name=="") stop("action name must be a non-empty string of characters")
  if (missing(nodes)) stop("please specify action node(s)")
  curr.names <- names(attr(DAG, "actions"))
  if (name %in% curr.names) {	# modify currently existing DAG
  	attr(DAG, "actions")[[name]] <- setAction(actname=name, inputDAG=attr(DAG, "actions")[[name]], actnodes=nodes, attr=attrs)
  } else { # create a new action
    new.action <- list(setAction(actname=name, inputDAG=DAG, actnodes=nodes, attr=attrs))
  	names(new.action) <- name
  	attr(DAG, "actions") <- c(attr(DAG, "actions"), new.action)
  }
  return(DAG)
}
## Action wrapper
## 
## Constructor for a new action object
## @param ... Arguments defining action
##' @rdname add.action
##' @export
action <- function(...) {
  structure(list(...), class="DAG.action")
}


