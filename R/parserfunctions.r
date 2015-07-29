# nocov start
# #
# # FUNCTIONS FOR PARSING NODE EXPRESSIONS
# #
# # FUNCTION NAMES THAT PRODUCE A VECTOR WHEN APPLIED TO A VECTOR (WILL NOT BE REPLACED BY apply(df,1,func)):
# vector_fcns <- c("cbind_mod","vecapply","apply","rowSums","rowMeans", "(", "[", "{", ":", "rep", "length", "if")
# # vectorized operators:
# vector_ops_fcns <- c("ifelse", "+", "-", "*","^", "/", "==", "!=", "!", "<", ">", "<=", ">=", "|", "&")
# # vectorized math funcs
# vector_math_fcns <- c("I","abs","sign","sqrt","round","signif","floor","ceil","ceiling","trunc",
#                       "sin","tan","cos","acos","asin","atan","cosh","sinh","tanh",
#                       "log","log10","log1p","exp","expm1","plogis",
#                       "beta","lbeta","gamma","lgamma","psigamma","digamma","trigamma",
#                       "choose","lchoose","factorial","lfactorial")

# a) find TD var calls;
# b) find baseline var calls;
# c) parse the tree at most 10 times and evaluate all atomic expressions
# d) modify calls to summary (non-vectorized) function to apply(DF, 1, func_name), adding cbind to calls with more than 1 arg
nodeform_parsers.depr <- function(node_form_call) {
  # combine all default vectorized funs + what the user-specified:
  vector_fcns_all <- c(vector_fcns, vector_ops_fcns, vector_math_fcns, vecfun.get())
  # print("vector_fcns_all"); print(vector_fcns_all)

  # (not USED) SUMMARY FCNS (non-vectorized): these will be always turned into apply(arg, 1, func) 
  # summary_fcns <- c("c","all","any","sum","mean","prod","min","max","range")
  # (not USED) FOR FUTURE IMPLEMENTATION: FUNCTION NAMES THAT AREN'T ALLOWED IN FORMULA EXPRESSIONS:
  # banned_fcns <- c( "apply", "cbind", "&&", "||")

  # * recursively parse the call tree structure of a given expression, find calls to '[', output the first argument (TDVar name that is being called as TDVar[])
  find_FormVars <- function(x, vartype="TD") {
    if (is.name(x) & vartype=="non_TD") {
      if (!is.function(try(get(as.character(x)),  silent = TRUE))) varnames <- as.character(x)
    } else if (is.atomic(x) || is.name(x)) {
      character()
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`[`)) && is.name(x[[2]])) {
        if (vartype=="TD") {
          varnames <- as.character(x[[2]])
        } else if (vartype=="TD_t") {
          varnames <- as.character(as.character(x[[2]])%+%"_"%+%eval(x[[3]]))
        } else if (vartype=="non_TD") {
          varnames <- character()
          x[[2]] <- NULL
        } else {
          stop("unrecognized variable type")
        }
      } else {
        varnames <- character()
      }
      if (length(x)>1 & vartype=="non_TD") x[[1]] <- NULL
      unique(c(varnames, unlist(lapply(x, find_FormVars, vartype))))
    } else if (is.pairlist(x)) {
      unique(unlist(lapply(x, find_FormVars, vartype)))
    } else {
      stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }
  # * iteratively parse the call tree and evaluate all functions with atomic args until identical tree is returned
  eval_all_atomic <- function(expr) {
    eval_atomic <- function (x, where = parent.frame()) {
      if (is.atomic(x) || is.name(x)) {
        x	# Leave unchanged
      } else if (is.call(x)) {
        # reached '[' or 'c' functions, don't need to parse any deeper, return this subtree intact
        if ((identical(x[[1]], quote(`[`)) && is.name(x[[2]])) | identical(x[[1]], quote(c))) {
          x # Leave unchanged
        } else {
          atomargs_test <- sapply(2:length(x), function(i) is.atomic(x[[i]]))
          # dprint("call: "%+%x[[1]]); for (i in (2:length(x))) dprint(x[[i]]); dprint("all atomic?: "%+%all(atomargs_test))
          if (!all(atomargs_test) | identical(x[[1]], quote(`{`))) { # 1) either one or more args are non-atomic, then continue parsing
            as.call(lapply(x, eval_atomic, where = where))
          } else {
            # or 2) all args are atomic - then evalute and return result
            # dprint("all args atomic, evaluated: "); dprint(eval(x))
            eval(x)
          }
        }
      } else if (is.pairlist(x)) {
        as.pairlist(lapply(x, eval_atomic, where = where))
      } else { # User supplied incorrect input
        stop("Don't know how to handle type ", typeof(x), call. = FALSE)
      }
    } # end of eval_atomic()

    # dprint("expression before atomic pre-eval: "); dprint(expr)
    preveval_atom_call <- expr; i=1; samecall <- FALSE
    while ((i <= 10)&(!samecall)) { # loop in while, max 10 iterations or when tree is no longer changing
      eval_atom_call <- eval_atomic(preveval_atom_call)
      samecall <- identical(eval_atom_call, preveval_atom_call)
      # dprint("-------------"); dprint(samecall); dprint(eval_atom_call); dprint("-------------")
      preveval_atom_call <- eval_atom_call; i <- i + 1
    }
    # dprint("expression after atomic pre-eval: "); dprint(eval_atom_call)
    eval_atom_call
  }
  # * modify the call tree with apply for non-vectorized (summary) functions, also adding cbind_mod() for calls with more than one arg
  # * TO ADD: if call tree starts with '{' need to process each argument as a separate call and return a list of calls instead
  modify_call <- function (x, where = parent.frame()) {
    if (is.atomic(x) & length(x)>1) {
      x <- parse(text = deparse(x, width.cutoff = 500))[[1]]
      modify_call(x, where = where)	# continue parsing recursively, turning result back into call
    }
    if (is.atomic(x) || is.name(x)) {
      # if (is.atomic(x)) dprint("atomic: "%+%x)
      # if (is.name(x)) dprint("name: "%+%x)
      x	# Leave unchanged
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`[`)) && is.name(x[[2]])) {	# reached '[' function, don't need to parse any deeper, return this subtree intact
        x
      } else if (as.character(x[[1]])%in%vector_fcns_all)  {  # these functions are already vectorized (if given a vector, will return a vector)
        # dprint(paste0("vectorized func: ",as.character(x[[1]])))
        as.call(lapply(x, modify_call, where = where))	# continue parsing recursively, turning result back into call
      } else {	# non-vectorized fun needs to be wrapped in vecapply, with args combined as cbind(arg1,arg2,...) for more than one arg
        # dprint(paste0("non-vectorized func: ",as.character(x[[1]])))
        if (identical(x[[1]], quote(c))) {
          x[[1]] <- quote(cbind_mod)	# check if the function is 'c', in which case replace call with 'cbind_mod'
          as.call(lapply(x, modify_call, where = where))	# continue parsing recursively, turning result back into call
        } else if (identical(x[[1]], quote(sum))) {
          x[[1]] <- quote(rowSums)	# check if the function is 'sum', in which case replace call with 'colSums'
          as.call(lapply(x, modify_call, where = where))	# continue parsing recursively, turning result back into call
        } else if (identical(x[[1]], quote(mean))) {
          x[[1]] <- quote(rowMeans)	# check if the function is 'mean', in which case replace call with 'colMeans'
          as.call(lapply(x, modify_call, where = where))	# continue parsing recursively, turning result back into call
        } else {
          nargs <- length(x)-1
          if (nargs > 1) { # IF NON-VECTORIZED func has more than one argument, combine all args into one with cbind_mod
          # dprint("several args: "%+%x[[1]])
            newargs <- "cbind_mod("%+%deparse(x[[2]], width.cutoff=500)
            for (i in (3:length(x))) newargs <- newargs%+%","%+%deparse(x[[i]], width.cutoff=500)
            for (i in (length(x)):3) x[[i]] <- NULL
            newargs <- newargs%+%")"
            newexp <- parse(text=newargs)[[1]]
            x[[2]] <- newexp
          }
          reparsed_chr <- "vecapply("%+%deparse(x[[2]], width.cutoff=500) %+% ", 1, " %+% deparse(x[[1]], width.cutoff=500) %+% ")"
          reparsed_call <- parse(text=reparsed_chr)[[1]]
          print(x)
          x[[1]] <- reparsed_call
          x[[2]] <- NULL
          modify_call(x[[1]], where = where)	# continue parsing recursively, turning result back into call
        }
      }
    } else if (is.pairlist(x)) {
      as.pairlist(lapply(x, modify_call, where = where))
    } else { # User supplied incorrect input
      stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    }
  }
  # eval_atom_call <- node_form_call						# don't evaluate any atomic expressions
  eval_atom_call <- eval_all_atomic(node_form_call)		# pre-evaluate all atomic expressions

  Vnames <- find_FormVars(eval_atom_call, vartype="non_TD")	# returns unique names of none TD vars that were called as VarName
  TD_vnames <- find_FormVars(eval_atom_call, vartype="TD")	# returns unique names TDVar that were called as TDVar[indx]	
  TD_t_vnames <- find_FormVars(eval_atom_call, vartype="TD_t") # returns unique names TDVar_t that were called as TDVar[indx]	

  # dprint("Vnames"); dprint(Vnames)
  # dprint("TD_vnames"); dprint(TD_vnames)
  # dprint("TD_t_vnames: "); dprint(TD_t_vnames)

  modified_call <- modify_call(eval_atom_call) 			# parse current call and replace any non-vectorized function with apply call (adding cbind_mod if more than one arg)
  return(list(Vnames=Vnames, TD_vnames=TD_vnames, TD_t_vnames=TD_t_vnames, modified_call=modified_call))
}

# function takes a string node formula, current node and current observed data environment
# 1) processes expression into R call, replaces t to its current value
# 2) finds all time-dep var names (Var[]) and non-time dep var names (Var)
# 3) replaces all summary function calls, s.a., func(Var) with apply(Var, 1, func)
# 4) replaces all calls to functions with several vectors, s.a., func(X1,X2,X3) with func(cbind(X1,X2,X3))
# 5) evaluates final expression in a special environment where: 
	# -) variables that have been simulated so far in obs.df are accessible
	# -) the subset vector function '[' is replaces with its specialized version, with syntax TDVar[t_range] for subsetting columns of the observed data by time
	# -) vecapply() function that is a wrapper for apply, converts vector to a 1 col matrix
# 6) standardizes the final expression to be a vector (NEED TO CHANGE FOR CATEGORICAL NODES - sapply over each prob formula in expression?)
eval_nodeform.depr <- function(expr_str, cur.node, env=parent.frame()) {
	where <- function(name, env = parent.frame()) {	# recursively return on nested environments until desired variable name is located
    if (identical(env, emptyenv())) {
      stop("Can't find ", name, call. = FALSE) 	# Base case
    } else if (exists(name, envir = env, inherits = FALSE)) {
      env # Success case
    } else {
      where(name, parent.env(env))	# Recursive case
    }
  }
  node_func <- list(
    # custom function for vector look up '['
    # function takes the name of the TD var and index vector => creates a vector of time-varying column names in df
    # returns matrix TD_var[indx]
    # ***NOTE: current '[' cannot evalute subsetting that is based on values of other covariates such as A1C[ifelse(BMI<5, 1, 2)]
    `[` = function(var, indx, env = parent.frame()) {
      if (identical(class(indx),"logical")) indx <- which(indx)
      t <- get("t", envir = env); # dprint("t: "%+%t)
      if (is.null(t)) stop("references references Var[t] are not allowed when t is undefined")
      if (max(indx)>t) stop(paste0(var, "[",max(indx),"] cannot be referenced in node formulas at t = ", t))	# check indx<= t
      TDvars <- var%+%"_"%+%indx
      # check the variables paste0(var, "_", indx) exist in simulated data.frame environment:
      # NEED ANOTHER METHOD FOR CHECKING TDvar existance...
      # e.g., try(eval(TDvar)), then is.vector() and length == n?
      # another approach is to modify node_func to include ANCHOR_ALLVARNMS_LIST that contains a list of character names of all vars...
      anchor_evn <- where("ANCHOR_VARS_OBSDF", env)
      check_exist <- sapply(TDvars, function(TDvar) exists(TDvar, where = anchor_evn, inherits = FALSE))
      # anchor_evn$ANCHOR_VARS_OBSDF <- append(anchor_evn$ANCHOR_VARS_OBSDF, list(TDvars)) # only needed for debugging
      if (!all(check_exist)) stop("undefined time-dependent variable(s): "%+%TDvars[which(!check_exist)])
      # dprint("TDvars in '[': "); dprint(TDvars)
      # dprint("exprs text in '[': "); 
      # dprint(paste0("cbind(",paste0(TDvars, collapse=","),")"))
      TDvars_eval <- eval(parse(text=paste0("cbind(",paste0(TDvars, collapse=","),")")), envir = env)
      return(TDvars_eval)
    },
    vecapply = function(X, idx, func) {	# custom wrapper for apply that turns a vector X into one column matrix
      if (is.vector(X)) dim(X) <- c(length(X), 1)	# returns TRUE only if the object is a vector with no attributes apart from names
      # if (is.atomic(x) || is.list(x)) dim(X) <- c(length(X), 1)	# alternative way to test for vectors
        x <- parse(text = deparse(func))[[1]]
        nargs <- length(x[[2]])
        if (nargs>1) {
          funline <- deparse(func)[1]
          stop(funline%+%". Node formulas cannot call non-vectorized functions with more than one named argument. If this is a vectorized function, pass its name to set.DAG(, vecfun=).")
        }
      apply(X, idx, func)
    },
    cbind_mod = function(...) { # wrapper for cbind that expands one row matrix to nrows=Nsamp
      env <- parent.frame()
      cbind_res <- do.call("cbind", eval(substitute(alist(...)), envir = env) , envir = env)
      if (nrow(cbind_res)==1) {
        Nsamp <- get("Nsamp", envir =env)
        cbind_res <- matrix(cbind_res, nrow= Nsamp, ncol=ncol(cbind_res), byrow = TRUE)
      }
      dprint("cbind_res"); dprint(cbind_res)
      cbind_res
    }
  )

  f_tovect <- function(X) {	# turn one column matrix to vector
    if (length(dim(X))>1) {
      if (dim(X)[2]==1) X <- as.vector(X)	
    }
    X
  }

  if (is.character(expr_str)) {
    expr_call <- try(parse(text=expr_str)[[1]]) 	# parse expression into a call
    if(inherits(expr_call, "try-error")) {
      stop("error while evaluating node "%+% cur.node$name %+%" parameters: "%+%expr_str%+%".\nCheck syntax specification.", call.=FALSE)
    }
  } else if (is.call(expr_str)){
    expr_call <- expr_str
    warning("node "%+%cur.node$name%+%": formula is already a parsed call")
  } else {
    stop("node "%+%cur.node$name%+%": currently can't process node formulas that are not strings or calls")
  }

  subst_call <- eval(substitute(substitute(e, list(t = eval(cur.node$t))), list(e = expr_call))) # Replace t in the node formula expression with current t value (returns a call)
  parse_res <- nodeform_parsers(subst_call) # traverse the node formula call, return TDvar & Var names (node parents) and modify subst_call to handle non-vectorized (summary functions)
  Vnames  <- setNames(parse_res$Vnames, parse_res$Vnames)
  TD_vnames <- setNames(parse_res$TD_vnames, parse_res$TD_vnames)
  TD_t_vnames <- setNames(parse_res$TD_t_vnames, parse_res$TD_t_vnames)
  modified_call <- parse_res$modified_call # modified call that has any non-vectorized function replaced with apply call (with cbind for more than one arg)

  dprint("----------------------")
  dprint("node: "%+%cur.node$name)
  dprint("original expr as call:"); dprint(expr_call)
  # dprint("call with replaced t:"); dprint(subst_call)
  dprint("final exprs:"); dprint(parse_res$modified_call)
  dprint("----------------------")

  anchor_evn <- where("ANCHOR_VARS_OBSDF", env)
  dprint("anchor_evn"); dprint(ls(anchor_evn));
  # check this TD Var doesn't exist in parent environment if df has TD Var => TD Var is not time-dependent and reference TDVar[t] is incorrect - throw exception
  for (TDname in TD_vnames) {
    if (TDname%in%ls(anchor_evn)) stop(paste0("reference ", TDname, "[...]", " at node ", cur.node$name, " is not allowed; node ", TDname," was defined as time-invariant"))
  }
  # check Var (not TD) already exists in parent environment
  for (Vname in Vnames) {
    if (!(Vname%in%ls(anchor_evn))) stop(paste0("formula at node ", cur.node$name, " cannot be evaluated; node ", Vname," is undefined"))
  }

  # ********************************
  # Change to enclose = user.env and envir = c(simdf, node_func)
  # ********************************
  if (is.call(modified_call) && identical(try(modified_call[[1]]), quote(`{`))) { # check for '{' as first function, if so, remove first func, turn call into a list of calls and do lapply on eval
  # print("call and { brace passed...")
  modified_call_nocurl <- modified_call[-1]
  evaled_expr <- try(lapply(X=modified_call_nocurl, FUN=eval, envir = c(lapply(TD_vnames, I), node_func), enclos=anchor_evn))
  } else {
    evaled_expr <- try(eval(modified_call, envir = c(lapply(TD_vnames, I), node_func), enclos=anchor_evn))	# evaluate modified_call in the df namespace with custom '[' function
  }

  if(inherits(evaled_expr, "try-error")) {
    stop("error while evaluating node "%+% cur.node$name %+%" parameters: "%+%expr_str%+%".\nCheck syntax specification.")
  }
  # print("evaled_expr"); print(evaled_expr)
  # convert one column matrices to vectors:
  if (!is.list(evaled_expr)) {
    evaled_expr <- f_tovect(evaled_expr)
  } else if (is.list(evaled_expr)) {
    evaled_expr <- lapply(evaled_expr, f_tovect)
  }
  # Alternative way to setting the environment:
  # lapply(TD_vnames, function(tdname) assign(tdname, tdname, envir = env))	# define TDvars (character) in passed environment (which contains all current variables)
  # assign('[', node_func$'[', envir = env)	# define special '[' function in env environment
  return(list(evaled_expr=evaled_expr, par.nodes=c(Vnames,TD_t_vnames))) # return evaluated expression and parent node names
}
# nocov end