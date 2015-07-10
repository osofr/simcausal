
`%+%` <- function(a, b) paste0(a, b)	# custom cat function
`%/%` <- function(a, b) paste0(a, paste0(b, collapse=","))  # custom collapse with "," function
`.` <- function(...) return(unlist(list(...)))	# defining function . to avoid warnings from the R check

opts <- new.env(parent = emptyenv())
opts$vecfun <- NULL
opts$debug <- FALSE

#' Print Names of Custom Vectorized Functions
#'
#' Print current user-defined vectorized function names.
#' @return A vector of vectorized function names
#' @export
vecfun.print <- function() {
  new <- opts$vecfun
  if (length(new)>1) new <- paste0(new, collapse=",")
  print("current list of user-defined vectorized functions: "%+%new)
  invisible(opts$vecfun)
}
#' Print Names of All Vectorized Functions
#'
#' Print all vectorized function names (build-in and user-defined).
#' @return A vector of build-in and user-defined vectorized function names
#' @export
vecfun.all.print <- function() {
  new <- opts$vecfun
  if (length(new)>1) new <- paste0(new, collapse=",")
  print("build-in vectorized functions:"); print(c(vector_ops_fcns, vector_math_fcns))
  print("user-defined vectorized functions: "%+%new)
  invisible(c(vector_ops_fcns,vector_math_fcns,new))
}
#' Add Custom Vectorized Functions
#'
#' Add user-defined function names to a global list of custom vectorized functions. The functions in \code{vecfun_names} are intended for use inside the node formulas. Adding functions to this list will generally greatly expedite the simulation run time. Any node formula calling a function on this list will be evaluated "as is", the function should be written to accept arguments as either vectors of length \code{n} or as matrices with \code{n} rows. Adding function to this list will effects simulation from all DAG objects that call this function. See vignette for more details.
#' @param vecfun_names A character vector of function names that will be treated as "vectorized" by the node formula R parser
#' @return An old vector of user-defined vectorized function names
#' @export
vecfun.add <- function(vecfun_names) { # Add vectorized function to global custom vectorized function list and return the old version
  old <- opts$vecfun
  opts$vecfun <- unique(c(opts$vecfun, vecfun_names))
  new <- opts$vecfun
  if (length(new)>1) new <- paste0(new, collapse=",")
  print("current list of user-defined vectorized functions: "%+%new)
  invisible(old)
}
#' Remove Custom Vectorized Functions
#'
#' Remove user-defined function names from a global list of custom vectorized functions. See vignette for more details.
#' @param vecfun_names A character vector of function names that will be removed from the custom list
#' @return An old vector of user-defined vectorized function names
#' @export
vecfun.remove <- function(vecfun_names) { # Remove vectorized functions to global custom vectorized function list and return the old version
  old <- opts$vecfun
  idx_remove <- old%in%vecfun_names

  if (sum(idx_remove) < length(vecfun_names)) {
    fun_notfound <- vecfun_names[!(vecfun_names%in%old)]
    if (length(fun_notfound)>1) fun_notfound <- paste0(fun_notfound, collapse=",")
    warning("some of the function names in 'vecfun_names' were not found and cannot be removed: "%+%fun_notfound)
  }
  if (sum(idx_remove)>0) {
    opts$vecfun <- opts$vecfun[-(which(idx_remove))]
  }
  new <- opts$vecfun
  if (length(new)>1) new <- paste0(new, collapse=",")
  print("current list of user-defined vectorized functions: "%+%new)
  invisible(old)
}
#' Reset Custom Vectorized Function List
#'
#' Reset a listing of user-defined vectorized functions.
#' @return An old vector of user-defined vectorized function names
#' @export
vecfun.reset <- function() {
  old <- opts$vecfun
  opts$vecfun <- NULL
  invisible(old)
}
vecfun.get <- function() opts$vecfun
get_opts <- function() opts$debug # Return Current Debug Mode Setting
debug_set <- function() { # Set to Debug Mode
  mode <- TRUE
  old <- opts$debug
  opts$debug <- mode
  invisible(old)
}
debug_off <- function() { # Turn Off Debug Mode
  mode <- FALSE
  old <- opts$debug
  opts$debug <- mode
  invisible(old)
}
get_localvecfun <- function(DAG) attr(DAG, "vecfun")
dprint <- function(...) if (opts$debug) print(...) # debug-only version of print
is.DAG <- function(DAG) (("DAG" %in% class(DAG)) || ("DAG.action" %in% class(DAG))) # check its a DAG object
is.DAG.action <- function(DAG) ("DAG.action" %in% class(DAG)) # check its a DAG.action object
is.DAGnodelist <- function(DAG) "DAG.nodelist" %in% class(DAG) # check its a DAG object
is.node <- function(node) "DAG.node" %in% class(node) # check its a DAG.node object
is.EFUP <- function(node) (!is.null(node$EFU))&&(node$EFU) # if the node is end follow-up when value = 1 (EFUP)
is.Bern <- function(node) {
  (node$distr%in%"Bern"||node$distr%in%"rbern") 		# if the node is Bernoulli random variable
}
is.attrnode <- function(node, DAG) {	# check if the node is an attribute (constant and cannot be intervened upon)
	attnames <- attr(DAG, "attnames")
	if (is.null(attnames)) {
	    FALSE
	} else {
		gnodename <- as.character(unlist(strsplit(node$name, "_"))[1])
	    gnodename%in%attr(DAG, "attnames")
	}
}
is.longfmt <- function(simdat) (attr(simdat, "dataform")%in%c("long")) # check data is in long format
is.LTCF <- function(simdat, outcome) {
	if (is.null(attr(simdat, "LTCF"))) {
		FALSE
	} else {
		attr(simdat, "LTCF")%in%outcome # check last time-point is carried forward (after failure)	
	}
}
is.DAGlocked <- function(DAG) (!is.null(attr(DAG, "locked"))&&attr(DAG, "locked"))
get.actname <- function(DAG) attr(DAG, "actname")

#' Print DAG Object
#' @param x A DAG object.
#' @param ... Other arguments to generic print.
#' @export
print.DAG <- function(x, ...) {
	idx_nodes <- (!sapply(x, is.attrnode, x))
	x_notnull <- lapply(x, function(node) node[!sapply(node, is.null)])
	print(str(x_notnull[idx_nodes]))
  invisible(x)
}

#' Print Action Object
#' @param x An object.
#' @param ... Other arguments to generic print.
#' @export
print.DAG.action <- function(x, ...) {
  actnodes <- unique(attr(x, "actnodes"))
  lenact <- length(actnodes)
  if (lenact>1) {
    if (lenact>5) {
      actnodes <- actnodes[1]%+%", "%+%actnodes[2]%+%", ... , "%+%actnodes[lenact-1]%+%", "%+%actnodes[lenact]
    } else {
      actnodes <- paste0(actnodes, collapse=",")
    }
  }
  print("Action: "%+%attr(x, "actname"))
  print("ActionNodes: "%+%actnodes)
  print("ActionAttributes: "); print(attr(x, "attrs"))
  res <- list(
      Action=attr(x, "actname"),
      ActionNodes=attr(x, "actnodes"), 
      ActionAttributes=attr(x, "attrs"))
  invisible(res)
}


#' Print DAG.node Object
#' @param x A Node object.
#' @param ... Other arguments to generic print.
#' @export
print.DAG.node <- function(x, ...) str(x)

#' (EXPERIMENTAL) Plot Discrete Survival Function(s)
#'
#' Plot discrete survival curves from a list of discrete survival probabilities by calling \code{plot} with \code{type='b'}.
#' @param surv A list of vectors, each containing action-specific discrete survival probabilities over time.
#' @param xindx A vector of indices for subsetting the survival vectors in \code{surv}, if omitted all survival probabilities in each \code{surv[[i]]} are plotted.
#' @param ylab An optional title for y axis, passed to \code{plot}.
#' @param xlab An optional title for x axis, passed to \code{plot}.
#' @param ylim Optional y limits for the plot, passed to \code{plot}.
#' @param ... Additional arguments passed to \code{plot}.
#' @export
plotSurvEst <- function(surv = list(), xindx = NULL, ylab = '', xlab = 't', ylim = c(0.0, 1.0), ...) {
  ptsize <- 0.7
  counter <- 0
  for(d.j in names(surv)){
    counter <- counter+1
    if (is.null(xindx)) xindx <- seq(surv[[d.j]])
    plot(x=xindx, y=surv[[d.j]][xindx], col=counter, type='b', cex=ptsize, ylab=ylab, xlab=xlab, ylim=ylim)
    par(new=TRUE)
  }
  legend(12,0.96, legend=names(surv), col=c(1:length(names(surv))), cex=ptsize, pch=1)
}

#' Plot DAG
#'
#' Plot a DAG object using functions from \code{igraph} package.
#' @param DAG A DAG object that was specified by calling \code{\link{set.DAG}}
#' @param tmax Maximum time-point to plot for time-varying DAG objects
#' @param xjitter Amount of random jitter for node x-axis plotting coordinates
#' @param yjitter Amount of random jitter for node y-axis plotting coordinates
#' @param node.action.color Color of the action node labels (only for action DAG of class DAG.action). If missing, defaults to red.
#' @param vertex_attrs A named list of \code{igraph} graphical parameters for plotting DAG vertices. These parameters are passed on to \code{add.vertices} \code{igraph} function.
#' @param edge_attrs A named list of \code{igraph} graphical parameters for plotting DAG edges. These parameters are passed on to \code{add.edges} \code{igraph} function.
#' @param customvlabs A named vector of custom DAG node labels (replaces node names from the DAG object).
#' @param excludeattrs A character vector of attribute DAG nodes that shouldn't be plotted
#' @export
plotDAG <- function(DAG, tmax=NULL, xjitter, yjitter, node.action.color, vertex_attrs=list(), edge_attrs=list(), excludeattrs, customvlabs) {
      if (!requireNamespace("igraph", quietly = TRUE)) {
        stop("igraph package is required for this function to work. Please install igraph.",
          call. = FALSE)
      }
      set.seed(12345)
      DAG_orig <- DAG      
      # Get a list of parent nodes
      par_nodes <- attr(DAG, "parents")
      # Get a list of attributes
      # attrs <- attr(DAG, "attrs")
      # print("par_nodes"); print(par_nodes)

      # ID attribute DAG nodes (those with missing order) and have them plotted separately (so as not to mess up the grid)
      ordervec <- sapply(DAG, '[[', "order")    
      nullorder_idx <- which(sapply(ordervec, is.null))
      if (length(nullorder_idx)>0) {
        DAG <- DAG[-nullorder_idx]
        class(DAG) <- "DAG"

        par_nodes_nullorder <- par_nodes[nullorder_idx]
        par_nodes <- par_nodes[-nullorder_idx]
      }

      # SUBSET A DAG by tmax
      if (!is.null(tmax)) {
        t_idx_all <- Nattr(DAG, "t") # a list of t values from current DAG (including NULLs)
        idx_tmax <- which(t_idx_all%in%tmax)
        if (length(idx_tmax)==0) {
          warning("tmax argument could not be matched, using all available time points")
        } else {
         DAG <- DAG[1:max(idx_tmax)]
         par_nodes <- par_nodes[1:max(idx_tmax)]
         class(DAG) <- "DAG"
        }
      }

      # Define x-axis coordinates
      x_layout <- c(0:(length(par_nodes)-1))
      if (!missing(xjitter)) {
        # x_layout <- x_layout + rnorm(length(x_layout))/5
        x_layout <- x_layout + rnorm(n=length(x_layout), mean=0, sd=xjitter)
      }
      
      # custom layout where x_axis is arranged by order ranking and y_axis is random only for a type of variable, but not by time
      # x_layout <- 3*c(0:(length(par_nodes)-1))

      # Plot when no t is defined in the DAG:
      if (is.null(DAG[[length(DAG)]]$t)) { 
        arrow.width <- 0.6
        arrow.size <- 0.5
        vertsize <- 10

      	tmax <- 0
        num_bsl <- length(DAG)
        num_tv <- 0
        y_bsl <- rep(c(0,1), length.out = num_bsl) + rnorm(num_bsl)     # y placement for bsl nodes
        y_layout <- y_bsl

      # Plot when t nodes are present:
      } else { 
        arrow.width <- 0.3
        arrow.size <- 0.2
        vertsize <- 7
        y_vec <- NULL # y coordinates for each DAG node
        # get node t values in a DAG as a list (including NULLs)
        t_idx_all <- Nattr(DAG, "t") # a list of t values from current DAG (including NULLs)
        tunique <- unique(unlist(t_idx_all))
        t_idx_miss <- sapply(t_idx_all, is.null) # finding all nodes where t is undefined (null)
        t_idx_miss <- which(t_idx_miss%in%TRUE)
        if (length(t_idx_miss)>0) {
          y_vec <- c(y_vec, seq(length(t_idx_miss)))
        }
        for (t in tunique) { # finding node indices for each t value in the DAG
          idx_t <- which(t_idx_all%in%t)
          if (length(idx_t)>0) {
            y_vec <- c(y_vec, seq(length(idx_t)))
          }
        }
        y_layout <- y_vec
      }

      if (!missing(yjitter)) {
        y_layout <- y_layout + rnorm(n=length(y_layout), mean=0, sd=yjitter)
      }
      layoutcustom_2 <- cbind(x_layout, y_layout)

      # create a layout for attribute nodes
      # check if any of the attributes are on the exclude list, if so, do not plot those nodes
      if (length(nullorder_idx)>0) {
        gennames <- sapply(strsplit(names(par_nodes_nullorder), "_"), '[[', 1)
        excl <- rep(FALSE, length(par_nodes_nullorder))
        if (!missing(excludeattrs)) {
          excl <- gennames%in%excludeattrs
          if (sum(excl)>0) {
            par_nodes_nullorder <- par_nodes_nullorder[!excl]
          }
        }
        x_layout_nullorder <- rep(-2, length(par_nodes_nullorder))
        maxy <- max(layoutcustom_2[,2])
        y_layout_nullorder <- seq(from=1, to=maxy, length.out=length(par_nodes_nullorder))
        layout_nullorder <- cbind(x_layout_nullorder, y_layout_nullorder)
        layoutcustom_2 <- rbind(layout_nullorder, layoutcustom_2)
        par_nodes <- c(par_nodes_nullorder, par_nodes)
      }

      # VERTEX AND EDGE plotting ATTRIBUTES
      # collect all attributes (must be named)
      attnames_ver <- names(vertex_attrs)
      attnames_edge <- names(edge_attrs)
      if (length(vertex_attrs) != 0 && (is.null(attnames_ver) || any(attnames_ver==""))) {
        stop("please specify name for each attribute in vertex_attrs")
      }
      if (length(edge_attrs) != 0 && (is.null(attnames_edge) || any(attnames_edge==""))) {
        stop("please specify name for each attribute in edge_attrs")
      }
      vertex_attrs_default <- list(color=NA, shape="circle", size=vertsize, label.cex=0.5, label.dist=0)
      edge_attrs_default <- list(color="black", width=0.2, arrow.width=arrow.width, arrow.size=arrow.size)
      vertex_attrs <- append(vertex_attrs, vertex_attrs_default[!(names(vertex_attrs_default)%in%attnames_ver)])
      edge_attrs <- append(edge_attrs, edge_attrs_default[!(names(edge_attrs_default)%in%attnames_edge)])
      message("using the following vertex attributes: "); message(vertex_attrs)
      message("using the following edge attributes: "); message(edge_attrs)

      g <- igraph::graph.empty()
      # g <- igraph::add.vertices(g, nv=length(names(par_nodes)), color=NA, shape="circle", size=vertsize, label.cex=0.5, label.dist=0)
      
      vlabs <- names(par_nodes)
      g <- igraph::add.vertices(g, nv=length(vlabs), attr=vertex_attrs)
      igraph::V(g)$name <- vlabs
      for (i in c(1:length(names(par_nodes)))) {
        if (length(par_nodes[[i]])>0) {
          # check that parent nodes actually exist and already have been defined:
          parents_i <- par_nodes[[i]]
          ind_parexist <- parents_i%in%(names(par_nodes)[1:i])
          if (!all(ind_parexist)) {
            par_outvec <- parents_i[!ind_parexist]
            if (length(par_outvec)>1) par_outvec <- paste0(par_outvec, collapse=",")
            warning("some of the extracted parent nodes aren't defined and are being omitted: "%+%par_outvec)
            parents_i <- parents_i[ind_parexist]
          }
          if (length(parents_i)>0) {
            # g <- igraph::add.edges(g, t(cbind(parents_i, names(par_nodes)[i])), color="black", width=0.2, arrow.width=arrow.width, arrow.size=arrow.size)
            g <- igraph::add.edges(g, t(cbind(parents_i, names(par_nodes)[i])), attr=edge_attrs)
          }
        }
      }
      # mark intervention node labels with another color
      if ("DAG.action"%in%class(DAG_orig)) {
        actnodenames <- attr(DAG_orig, "actnodes")
        if (missing(node.action.color)) node.action.color <- "red"
        # V(g)[name%in%actnodenames]$color <-"red"
        igraph::V(g)[igraph::V(g)$name%in%actnodenames]$label.color <- node.action.color
      }
      if (!missing(customvlabs)) {  # use user-supplied custom labels for DAG nodes
        igraph::V(g)$name <- customvlabs
      }
      g <- igraph::set.graph.attribute(g,'layout',layoutcustom_2)
      igraph::plot.igraph(g, asp=0.5)
}

#' Show Node Parents Given DAG Object
#'
#' Given a vector of node names, this function provides the name(s) of node parents that were obtained by parsing the node formulas.
#' @param DAG A DAG object that was specified by calling \code{\link{set.DAG}}
#' @param nodesChr A vector of node names that are already defined in DAG
#' @return A list with parent names for each node name in nodesChr
#' @examples
#' 
#'D <- DAG.empty()
#'D <- D + node(name="W1", distr="rbern", prob=plogis(-0.5))
#'D <- D + node(name="W2", distr="rbern", prob=plogis(-0.5 + 0.5*W1))
#'D <- D + node(name="A", distr="rbern", prob=plogis(-0.5 - 0.3*W1 - 0.3*W2))
#'D <- D + node(name="Y", distr="rbern", prob=plogis(-0.1 + 1.2*A + 0.3*W1 + 0.3*W2), EFU=TRUE)
#'D <- set.DAG(D)
#'parents(D, c("W2", "A", "Y"))
#' @export
parents <- function(DAG, nodesChr) {
	parents_list <- attr(DAG, "parents")
	node_names <- names(parents_list)
	# print("node_names"); print(node_names)
	parents <- lapply(nodesChr, function(nodeChr) as.character(parents_list[[which(node_names%in%nodeChr)]]))
	names(parents) <- nodesChr
	parents
}

# Internal function that checks all DAG node objects have unique name attributes
check_namesunique <- function(inputDAG) {
	node_names <- sapply(inputDAG, "[[", "name")
	(length(unique(node_names))==length(inputDAG)) 
}
# Internal function that checks all DAG node objects are already in expanded format ((length(t)==1, length(order)==1))
check_expanded <- function(inputDAG) {
	for (node in inputDAG) {
		if (length(node$t)>1 | length(node$order)>1) {
			stop("node "%+% node$name %+% " is not expanded. Call constructor function node() first: node(name, t, distr, mean, probs, var, EFU, order)")
		}
	}
}

#' Create and Lock DAG Object
#'
#' Check current DAG (created with \code{node}) for errors and consistency of its node distributions, set the observed data generating distribution. Attempts to simulates several observations to catch any errors in DAG definition. New nodes cannot be added after function set.DAG has been applied.
#' @param DAG Named list of node objects that together will form a DAG. Temporal ordering of nodes is either determined by the order in which the nodes were added to the DAG (using \code{+node(...)} interface) or with an optional "order" argument to \code{node()}.
#' @param vecfun A character vector with names of the vectorized user-defined node formula functions. See examples and the vignette for more information.
#' @return A DAG (S3) object, which is a list consisting of node object(s) sorted by their temporal order.
#' @example tests/RUnit/set.DAG.R
#' @export
set.DAG <- function(DAG, vecfun) {

  # ************
  # Adding parent env. for future evaluation of node formulas. 
  # Will be saved as a DAG attribute and then passed to form parser for evaluation as: eval(form, envir = df, enclos = env)
  env <- parent.frame() 
  # ************

  rndseed <- NULL
	# set of allowed named arguments
	node_args_all <- c("name", "t", "distr", "dist_params", "EFU", "order")
	# set of required named arguments
	node_args_req <- c("name", "distr", "order")
	# set of optional named arguments
	node_args_opt <- c("t", "dist_params", "EFU")

	#---------------------------------------------------------------------------------
	# DAG specification errors checks
	#---------------------------------------------------------------------------------

	# *) check DAG is a list and all of its items are also lists
	if (!(is.list(DAG))) stop("DAG must be a list")
  # *) if DAG is a empty create a wanring and return empty DAG
  if (length(DAG)==0) {
    warning("trying to lock an empty DAG, add nodes before calling set.DAG()")
    # attr(DAG, "locked") <- TRUE
    return(DAG)
  }
	if (!(all(sapply(DAG, is.list)))) stop("each of DAG items must be a list specifying DAG node(s)")
  class(DAG) <- "DAG"
  # *) check all DAG have order attribute defined, if not, assign the order based on the node location in the list
  check.order <- sapply(Nattr(DAG, "order"), is.null)
  # print("check.order"); print(check.order)
  if (any(check.order)) {
    message("...automatically assigning order attribute to some nodes...")
    for (inode in which(check.order)) {
      if (inode==1) {
        DAG[[inode]]$order <- 1
      } else {
        DAG[[inode]]$order <- DAG[[inode-1]]$order+1
      }
      message("node "%+%DAG[[inode]]$name%+%", order:"%+%DAG[[inode]]$order)
    }
  }
	# *) check each node contains required named arguments
	checknames.req <- unlist(lapply(DAG, function(node) all(node_args_req%in%names(node))))
	if (!all(checknames.req)) stop(paste0("some nodes are missing required named arguments, check node(s): ",
									paste0(which(!checknames.req), collapse=",")))
	# *) check all DAG names are unique
	if (!check_namesunique(DAG)) stop("All DAG nodes must have unique name attributes")
	# *) check the DAG nodes are already in expanded format (length(t)==1, length(order)==1), if not, 
	# in the future give warning and call node() constructor on not expanded nodes
	# for now just throws an exception and quits
	check_expanded(DAG)

	#---------------------------------------------------------------------------------
	# Sort DAG (by order) - giving order argument supersedes the order in which the nodes were added to the DAG
	#---------------------------------------------------------------------------------
	Nsublists <- length(DAG)
	Nnodes <- Nsublists	# Actual number of nodes that will appear in DAG (Nnodes) (for now equal to number of sublists)
	inputDAG <- sortbyorder(DAG)	# Sort sublists by order value
	for (node_idx in c(1:length(inputDAG))) {	# add all optional missing named arguments as 'argname'=NULL (when DAG is entered without node() constructor)
		inputDAG[[node_idx]] <- createNodeObj(inputDAG[[node_idx]], node_args_all)
	}
	#---------------------------------------------------------------------------------
	# Returning the full DAG object (expanded + nodes renamed + sorted by order)
	# Saved each node as an object node that has a fixed named items (=NULL when not used)
	#---------------------------------------------------------------------------------
  class(inputDAG) <- "DAG"
  #---------------------------------------------------------------------------------
  # Add vectorized functions to vectorized function list and DAG attribute "vect_fun"
  #---------------------------------------------------------------------------------
  if (!missing(vecfun)) {
    oldvecfun <- vecfun.add(vecfun)
    # oldvecfun <- vecfun_add(vecfun)
    # print("get_vecfun"); print(get_vecfun()); newvecfun <- vecfun_reset(oldvecfun); print("get_vecfun"); print(get_vecfun())
    attr(inputDAG, "vecfun") <- vecfun
  }
  #---------------------------------------------------------------------------------
  # Checking for correct DAG specification by simulating one observation
  #---------------------------------------------------------------------------------  
  obs.df <- try(simobs(inputDAG, n=10, rndseed=rndseed))
  if(inherits(obs.df, "try-error")) {
    stop("\n...attempt to simulate data from DAG failed...")
  }
	attr(inputDAG, "parents") <- attr(obs.df, "parents")
  attr(inputDAG, "locked") <- TRUE
  attr(inputDAG, "parent.env") <- env # Adding parent env. for future evaluation of node formulas. 
	return(inputDAG)
}

# given a DAG and a newnode, find the DAG node by name and change its values to those in newnode (that aren't equal to NULL)
modDAGnode <- function(oldDAG, newnode) {
  mod_name <- newnode$name
  oldDAG_names <- sapply(oldDAG, function(node) node$name)
  idxDAG <- which(oldDAG_names%in%mod_name)
  if (length(idxDAG)>1) stop("non-unique intervention node name found in DAG")
  oldnode <- oldDAG[[idxDAG]]
  if (as.character(oldnode$name)!=as.character(newnode$name)) stop("names of action node and DAG node do not match")
  oldnodenames <- names(oldnode) # only change node values for items that already exist in DAG node (NOT TO OVERWRITE THE ENTIRE NODE)
  newnodenames <- names(newnode)
  common_items <- intersect(newnodenames, oldnodenames)
  common_items <- common_items[!sapply(newnode[common_items], is.null)] # only replace DAG values from newnode (action node) values that are not NULL
  oldnode[common_items] <- newnode[common_items]
  oldDAG[[idxDAG]] <- oldnode
  return(oldDAG)
}
###################################################################
# An internal function that processes an action by finding intervention nodes and returning modified DAG object
###################################################################
# Define intervention DAG(s) consisting of action node(s)
# inputDAG - Previously defined DAG object
# actnodes - List of action nodes, describing actions for any number of nodes
# attr -  Named list with intervention-specific real-valued vectors that will be added as constant-value or time-dependent nodes to the modified DAG
setAction <- function(actname, inputDAG, actnodes, attr=list()) {
  rndseed <- NULL
    # given attribute name, returns several "constant" TV nodes (attname_t) if attvals is a vector, otherwise returns one baseline node (attname)
	expandattr <- function(attname, attvals, tvals) {
		if (!is.numeric(attvals)) stop("action attribute values must be numeric: "%+%attname)
    # ***ASSUMPTION***: We are assuming if the attribute vector is of length 1 than it is TIME-INVARIANT (e.g., theta=0.5)
		if (length(attvals)>1) { 
			if (length(attvals)!=length(tvals)) stop("vector-valued attributes of length > 1 must be equal to the number of action node time points: "%+%attname)
			new_nodes <- sapply(seq(attvals), function(i) node(name=attname, t=tvals[i], distr="rconst", const=.(attvals[i])))
		} else {
      # ***NOTE***: If action nodes are defined over time points, make time-invariant node t=0, other t=NULL/Missing
      # above doesn't work for formula references, alternative is define the time t=0 but leave name unchanged, i.e., theta, not theta_0
      # if (!is.null(tvals)) {
      #   new_nodes <- node(name=attname, t=0, distr="rconst", const=.(attvals))
      # } else {
        new_nodes <- node(name=attname, distr="rconst", const=.(attvals))
      # }
		}
    class(new_nodes) <- "DAG.nodelist"
		return(new_nodes)
	}

	if (!is.DAG(inputDAG)) stop("inputDAG argument is not of class DAG, run set.DAG function first")

	modDAG.full <- inputDAG # This is either observed data DAG (locked) or already existing DAG.action
  dagattrs_saved <- attributes(inputDAG) # save existing attributes
  dagattrs_saved$parents <- NULL  # remove all actions
  dagattrs_saved$actions <- NULL  # and parents

  class(actnodes) <- "DAG.nodelist"
  mod_names <- unlist(Nattr(actnodes, "name")) # get intervention node names
  DAG_names <- unlist(Nattr(inputDAG, "name"))  # get DAG node names

	checkintnames <- (mod_names%in%DAG_names)
	if (!all(checkintnames)) stop(paste0("setActions(): some intervention node names do not exist in the original DAG: ", paste0(mod_names[!checkintnames], collapse=",")))
	DAG_chgnodes <- match(mod_names, DAG_names)	# nodes indices in DAG that need to be modified
	if (length(DAG_chgnodes)!=length(mod_names)) stop("setActions(): action node and DAG node names do not make a unique pair match")
	for (inode in (1:length(mod_names))) {	# MODIFY DAG NODES w/ action nodes
		modDAG.full <- modDAGnode(modDAG.full, actnodes[[inode]]) # launch a function for each pair (DAG_node <-> Intervention Node) that modifies DAG_node
	}
  dprint("actnodes"); dprint(actnodes)

	if (!is.null(actnodes[[1]]$t)) {
		# mod_tvals <- sort(unique(sapply(actnodes, function(node) node$t)))	# get unique time points for this intervention	
    mod_tvals <- sort(unique(unlist(Nattr(actnodes, "t"))))  # get unique time points for this intervention  
	} else {
		mod_tvals <- NULL
	}
	dprint("mod_tvals"); dprint(mod_tvals)

	if (length(attr)>0) {	# either add attribute as a new DAG node or modify existing attribute node
		attnames <- names(attr)
		for (i in seq(attr)) {
      DAG_names <- unlist(Nattr(modDAG.full, "name"))  # get DAG node names
      gennamesall <- as.character(sapply(strsplit(DAG_names, "_"), '[', 1))
      gattr_nm <- attnames[i] # generic attribute name (without t)

			attr_nodes <- expandattr(attnames[i], attr[[attnames[i]]], mod_tvals)
			# mod_names_attr <- sapply(attr_nodes, function(node) node$name)
      mod_names_attr <- unlist(Nattr(attr_nodes, "name"))
  
			checkattrexist <- (mod_names_attr%in%DAG_names)
      checkgenexist <- (gattr_nm%in%DAG_names) # check if generic attribute under the same name already exists

      if ((!any(checkattrexist)) & checkgenexist) { # no TV attribute values were defined yet but the generic (nonTV) already exists
      # give warning and delete non-TV attribute, add TV attribute
        gnode_idx <- which(DAG_names%in%gattr_nm)
        modDAG.full <- modDAG.full[-gnode_idx]
        class(modDAG.full) <- "DAG"
        warning("\nAction: " %+% actname %+% "; Attribute: " %+% gattr_nm %+% ".\nScalar attribute value is overwritten by a vector of time-varying values.")
        # warning("\nNon-time-varying attribute ("%+% gattr_nm %+% ") was overwritten by time-varying attribute values")
      }
      # for generic node being added (theta), need to check that no TV nodes under the same name already exist (theta_i)
      if (!checkgenexist) { # the generic (nonTV) node doesn't exist yet but the TV node already does
        if ((length(attr_nodes)==1) && (mod_names_attr==gattr_nm) && any(gennamesall%in%gattr_nm)) { # check its a non-TV attribute that is being added:
          # give warning and delete old node, add new ones
          gnode_idx <- which(gennamesall%in%gattr_nm)
          modDAG.full <- modDAG.full[-gnode_idx]
          class(modDAG.full) <- "DAG"
          warning("\nAction: " %+% actname %+% "; Attribute: " %+% gattr_nm %+% ".\nVector of time-varying attribute values is overwritten by a single scalar value.")
          # warning("\nTime-varying attribute ("%+% gattr_nm %+% ") was overwritten by non-time-varying attribute values")
        }
      }
      # either add or overwrite existing attribute values
      exist_attr <- which(names(dagattrs_saved$attrs)%in%gattr_nm)
      if (length(exist_attr)>0) {
        dagattrs_saved$attrs[exist_attr] <- attr[i]
      } else {
        dagattrs_saved$attrs <- c(dagattrs_saved$attrs, attr[i])
      }

      attr_insert <- attr_nodes[which(!checkattrexist)]
      # 12/23/2014 OS: attributes no longer inserted at their corresponding time slots in the DAG, inserted at the beginning of the DAG as was originally designed
      modDAG.full <- append(modDAG.full, attr_insert, 0) # append new attributes as a first item(s) of the DAG
      dprint("modDAG.full"); dprint(modDAG.full[1:5])
      class(modDAG.full) <- "DAG"
      # for attributes that already exists in the DAG, just modify the current node:
			for (inode in which(checkattrexist)) modDAG.full <- modDAGnode(modDAG.full, attr_nodes[[inode]])
		}
	} else {
		attnames <- NULL
	}

  # check data can be simulated from modified DAG
  full.df <- try(simobs(modDAG.full, n=10, rndseed=rndseed))
  if(inherits(full.df, "try-error")) {
    stop("\n...attempt to simulate data from action DAG failed...")
  }
  dagattrs_saved$names <- attr(modDAG.full,"names")
  attributes(modDAG.full) <- dagattrs_saved
  # attributes(modDAG.full) <- c(attributes(modDAG.full)["names"], dagattrs_saved)
	attr(modDAG.full, "parents") <- attr(full.df, "parents")
  attr(modDAG.full, "actname") <- actname
	attr(modDAG.full, "actnodes") <- unique(c(attr(modDAG.full, "actnodes"), mod_names))
	attr(modDAG.full, "acttimes") <- unique(c(attr(modDAG.full, "acttimes"), mod_tvals))
	attr(modDAG.full, "attnames") <- unique(c(attr(modDAG.full, "attnames"), attnames))
  class(modDAG.full) <- "DAG.action"

	return(modDAG.full)
}

# Internal function creates an object of class node/actionode 
# Deprecated & is used for backward compatibility when specifying DAG directly through lists (not using node() constructor)
# Define and set to NULL all missing node arguments
# input.node - node content (names+values)
# node_args_all - all node arguments
createNodeObj <- function(input.node, node_args_all) {
	indx_missargs <- which(!(node_args_all %in% names(input.node)))
	temp <- list()	# temp list with NULL args that weren't defined yet
	length(temp) <- length(indx_missargs)
	names(temp) <- node_args_all[indx_missargs]
	input.node <- c(input.node, temp)	# add NULL named items to action node
  order_nms <- order(match(names(input.node),node_args_all)) # sort node names according to name order in newnode_args_all
	node_obj <- input.node[order_nms] # reassign old node vals to reordered + NULL node vals
	if (!identical(class(node_obj), "DAG.node")) class(node_obj) <- "DAG.node"
	# return(input.node[order_nms])
	return(node_obj)
}

# Sort a named list of node objects by node$order value -> get a DAG object
sortbyorder <- function(DAG) {
	node_names_L <- names(DAG)
	node_names_attr <- sapply(DAG, '[[', "name")
	if (length(node_names_L)!=length(node_names_attr)) stop("DAG list names of node objects and node name arguments do not match in length...")
	if (!all(node_names_L==node_names_attr)) stop("DAG list names of node objects and node name arguments don't match")
	order_vals <- sapply(DAG, '[[', "order")
	if ((length(order_vals)!=length(DAG)) | any(is.null(order_vals))) stop("node order argument is either incorrectly specified or missing")
	if (length(unique(order_vals))!=length(order_vals)) stop("some nodes have identical order values - all order values must be unique")
	if (min(order_vals)<1) stop("node order values must start at 1")
	# order_indx <- c(1:length(order_vals))	# the ordering that we want to sort by
	order_indx <- sort(order_vals)
	new_order_indx <- order(match(order_vals,order_indx)) # sort observed order values in increasing order and get new index vector for Input node list
	DAG[new_order_indx]
}


