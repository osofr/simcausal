# ************************************************************************************
# TO DO: 
  # x) Extend the checking for non_TD pars to TD parents in find_FormVars() 
  # x) For non_TD var outside of the DAG also check that length(non_TD) < 2
  # x) => Want to allow vectors in user.env to be referenced as uservec[t]
  # x) => This will allow avoiding declaration of node attributes as nodes, will save a ton of memory
# ************************************************************************************


# ------------------------------------------------------------------------------------------------------
# FUNCTIONS FOR PARSING AND EVALUATING NODE FORMULAS (PARAMETERS)
# ------------------------------------------------------------------------------------------------------

# FUNCTION NAMES THAT PRODUCE A VECTOR WHEN APPLIED TO A VECTOR (WILL NOT BE REPLACED BY apply(df,1,func)):
vector_fcns <- c("cbind_mod","vecapply","apply","rowSums","rowMeans", "(", "[", "[[", "{", ":", "rep", "length", "if")
# vectorized operators:
vector_ops_fcns <- c("ifelse", "+", "-", "*","^", "/", "==", "!=", "!", "<", ">", "<=", ">=", "|", "&")
# vectorized math funcs
vector_math_fcns <- c("I","abs","sign","sqrt","round","signif","floor","ceil","ceiling","trunc",
                      "sin","tan","cos","acos","asin","atan","cosh","sinh","tanh",
                      "log","log10","log1p","exp","expm1","plogis",
                      "beta","lbeta","gamma","lgamma","psigamma","digamma","trigamma",
                      "choose","lchoose","factorial","lfactorial")

# a) find TD var calls;
# b) find baseline var calls;
# c) parse the tree at most 10 times and evaluate all atomic expressions
# d) modify calls to summary (non-vectorized) function to apply(DF, 1, func_name), adding cbind to calls with more than 1 arg
nodeform_parsers = function(node_form_call, data.env, user.env)  {
  # combine all default vectorized funs + the user-specified vectorized function in global :
  vector_fcns_all <- c(vector_fcns, vector_ops_fcns, vector_math_fcns, vecfun.get())
  curr.dfvarnms <- data.env[["ANCHOR_ALLVARNMS_VECTOR_0"]]
  # 
  # (not USED) SUMMARY FCNS (non-vectorized): these will be always turned into apply(arg, 1, func)
  # summary_fcns <- c("c","all","any","sum","mean","prod","min","max","range")
  # (not USED) FOR FUTURE IMPLEMENTATION: FUNCTION NAMES THAT AREN'T ALLOWED IN FORMULA EXPRESSIONS:
  # banned_fcns <- c( "apply", "cbind", "&&", "||")

  # * recursively parse the call tree structure for a given expression, find call to '[' or a name, then output that name (TDVar name will be called as TDVar[])

  # ************************************************************************************
  # TO DO: 
  # Extend the same checks for non_TD var existance to TD vars => Want to allow vectors in user.env to be referenced as uservec[t]
  # When TDvar_t not in DAG, check that TD_var exists in user.env, check that its a vector and that length matches t length
  # Decide between method I & II for finding non_TD parents
  # Curently using method I for non_TD vars, plotting DAG will exclude 
  # ************************************************************************************

  find_FormVars <- function(x, vartype="TD") {
    if (is.name(x) & vartype=="non_TD") {
      dprint("x: " %+% as.character(x))

      # Method I: will find all variables referenced by node formula, including vars only defined in user.env and are not part of the DAG
      notis.fun <- eval(substitute(!is.function(try(get(as.character(x)),  silent = TRUE))), envir = data.env, enclos = user.env)
      dprint("is x not a fun? " %+% notis.fun)

      # Method II: will only identify vars that were defined in the DAG. Probably more stable.
      is.inDAG <- as.character(x) %in% curr.dfvarnms
      dprint("is x in DAG? " %+% is.inDAG);

      # CHECK FOR UNDECLARED VARS: Verify if x is defined in user env if (!notis.fun & !is.inDAG)
      # exists.x <- exists(as.character(x), where = user.env, inherits = FALSE) 
      exists.x <- exists(as.character(x), where = user.env, inherits = TRUE)
      dprint("does x exist in user.env? " %+% exists.x)

      # CHECK THAT ITS NOT A SPECIAL (RESERVED) VAR (nF)
      specialVar <- c("nF", "Kmax", "t", "Nsamp")
      special <- as.character(x) %in% specialVar
      dprint("is x special? " %+% special)

      if (notis.fun && !is.inDAG && !exists.x && !special) stop("Undefined variable: " %+% as.character(x), call. = FALSE)

      # ****************************
      # *) For non_TD var outside of the DAG also check that length(non_TD) < 2
      # ****************************

      # if (is.inDAG) varnames <- as.character(x) # METHOD I declares only vars that exist in the DAG as a parent
      if (notis.fun) varnames <- as.character(x) # METHOD II declares any nonfun var as a parent

    } else if (is.atomic(x) || is.name(x)) {
      character()
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`[`)) && is.name(x[[2]])) {
        if (vartype=="TD") {
          varnames <- as.character(x[[2]])
        } else if (vartype=="TD_t") {
          res_t <- eval(x[[3]], envir = data.env, enclos = user.env)
          # res_t <- eval(x[[3]])
          res_t_chr <- try(as.character(res_t), silent = TRUE)
          if (inherits(res_t_chr, "try-error")) {
            warning("the argument inside [...] cannot be parsed: " %+% deparse(x))
            res_t_chr <- deparse(x[[3]])
          }
          varnames <- as.character(as.character(x[[2]]) %+% "_" %+% res_t_chr)
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
        # reached '[', '[[' or 'c' functions, don't need to parse any deeper, return this subtree intact
        if (((identical(x[[1]], quote(`[`)) || identical(x[[1]], quote(`[[`))) && is.name(x[[2]])) || identical(x[[1]], quote(c))) {
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

    dprint("expression before atomic pre-eval: "); dprint(expr)

    preveval_atom_call <- expr
    i <- 1
    samecall <- FALSE # flag for parsed tree being identical to the previous pre-parsed call tree

    # loop for max 10 iterations or when call tree is no longer changing:
    while ((i <= 10) & (!samecall)) {
      eval_atom_call <- eval_atomic(preveval_atom_call)
      samecall <- identical(eval_atom_call, preveval_atom_call)
      preveval_atom_call <- eval_atom_call
      i <- i + 1
    }
    # dprint("expression after atomic pre-eval: "); dprint(eval_atom_call)
    eval_atom_call
  }

  # * TO DO: MIGHT REMOVE THIS FUNCTION COMPLETELY, EITHER ASSUME ALL FUNs ARE ALREADY VECTORIZED OR PRE-TEST FUNS FOR VECTORIZATION AND RETURN ERROR IF FUN RETURNS A NON-VECTOR (SCALAR)
  # * modify the call tree with apply for non-vectorized (summary) functions, also adding cbind_mod() for calls with more than one arg
  # * TO ADD: if call tree starts with '{' need to process each argument as a separate call and return a list of calls instead
  modify_call <- function (x, where = parent.frame()) {
    if (is.atomic(x) & length(x)>1) {
      x <- parse(text = deparse(x, width.cutoff = 500))[[1]]
      modify_call(x, where = where)	# continue parsing recursively, turning result back into call
    }
    if (is.atomic(x) || is.name(x)) {
      if (is.atomic(x)) dprint("atomic: "%+%x)
      if (is.name(x)) dprint("name: "%+%x)
      x	# Leave unchanged
    } else if (is.call(x)) {
      if (identical(x[[1]], quote(`[`)) && is.name(x[[2]])) {	# reached '[' function, don`t need to parse any deeper, return this subtree intact
        x
      } else if (identical(x[[1]], quote(`[[`)) && is.name(x[[2]])) { # reached '[[' function, same as above
        x
      } else if (as.character(x[[1]]) %in% vector_fcns_all)  {  # these functions are already vectorized (if given a vector, will return a vector)
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
        } else if (identical(x[[1]], quote(structure))) {
          modify_call(as.call(x[[2]]), where = where)  # continue parsing recursively, turning result back into call          
          # as.call(lapply(x[[2]], modify_call, where = where))  # continue parsing recursively, turning result back into call          
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

  # eval_atom_call <- node_form_call						      # don't evaluate any atomic expressions
  eval_atom_call <- eval_all_atomic(node_form_call)		# pre-evaluate all atomic expressions

  # Parses the formula and gets all the variable names referenced as [] or as.name==TRUE
  Vnames <- find_FormVars(eval_atom_call, vartype="non_TD")	# returns unique names of none TD vars that were called as VarName
  TD_vnames <- find_FormVars(eval_atom_call, vartype="TD")	# returns unique names TDVar that were called as TDVar[indx]
  TD_t_vnames <- find_FormVars(eval_atom_call, vartype="TD_t") # returns unique names TDVar_t that were called as TDVar[indx]

  dprint("Vnames: "); dprint(Vnames)
  dprint("TD_vnames: "); dprint(TD_vnames)
  dprint("TD_t_vnames: "); dprint(TD_t_vnames)

  modified_call <- modify_call(eval_atom_call) 			# parse current call and replace any non-vectorized function with apply call (adding cbind_mod if more than one arg)
  dprint("modified_call"); dprint(modified_call)

  return(list(Vnames = Vnames, TD_vnames = TD_vnames, TD_t_vnames = TD_t_vnames, modified_call = modified_call))
}

eval.nodeform.full <- function(expr_call, expr_str, self, data.env) {
  # traverse the node formula call, return TDvar & Var names (node parents) and modify subst_call to handle non-vectorized (summary functions):
  parse_res <- nodeform_parsers(node_form_call = expr_call, data.env = data.env, user.env = self$user.env)

  # set the local variables in the formula node to their character values:
  Vnames  <- parse_res$Vnames
  TD_vnames <- parse_res$TD_vnames
  TD_t_vnames <- parse_res$TD_t_vnames
  modified_call <- parse_res$modified_call # modified call that has any non-vectorized function replaced with apply call (with cbind for more than one arg)

  dprint("----------------------")
  dprint("node: "%+%self$cur.node$name)
  dprint("original expr as call:"); dprint(expr_call)
  dprint("final exprs:"); dprint(parse_res$modified_call)
  dprint("----------------------")

  df_varnms <- data.env$ANCHOR_ALLVARNMS_VECTOR_0

  # check this TD Var doesn't exist in parent environment if df has TD Var => TD Var is not time-dependent and reference TDVar[t] is incorrect - throw exception
  for (TDname in TD_vnames) {
    if (TDname%in%df_varnms) stop(paste0("reference ", TDname, "[...]", " at node ", self$cur.node$name, " is not allowed; node ", TDname," was defined as time-invariant"))
  }

  # add special node expression functions to the evaluation environment:
  data.env <- c(self$node_fun, data.env)

  if (is.call(modified_call) && identical(try(modified_call[[1]]), quote(`{`))) { # check for '{' as first function, if so, remove first func, turn call into a list of calls and do lapply on eval
    modified_call_nocurl <- modified_call[-1]
    evaled_expr <- try(lapply(X = modified_call_nocurl, FUN = eval, envir = data.env, enclos = self$user.env))
  } else {
    evaled_expr <- try(eval(modified_call, envir = data.env, enclos = self$user.env)) # evaling expr in the envir of data.df
  }

  if(inherits(evaled_expr, "try-error")) {
    stop("error while evaluating node "%+% self$cur.node$name %+%" formula: \n"%+%parse(text = expr_str)%+%".\nCheck syntax specification.", call. = FALSE)
  }

  dprint("evaled_expr: "); dprint(evaled_expr)

  # convert one column matrix to a vector:
  f_tovect <- function(X) {
    if (length(dim(X))>1) {
      if (dim(X)[2]==1) X <- as.vector(X)
    }
    X
  }

  if (!is.list(evaled_expr)) {
    evaled_expr <- f_tovect(evaled_expr)
  } else if (is.list(evaled_expr)) {
    evaled_expr <- lapply(evaled_expr, f_tovect)
  }

  return(list(evaled_expr = evaled_expr, par.nodes = c(Vnames, TD_t_vnames))) # return evaluated expression and parent node names
}

# evaluate the expression without special functions in self$node_fun ('[', '[[', vecapply, cbind_mod)
eval.nodeform.asis <- function(expr_call, expr_str, self, data.env) {
  evaled_expr <- try(eval(expr_call, envir = data.env, enclos = self$user.env)) # evaling expr in the envir of data.df
  if(inherits(evaled_expr, "try-error")) {
    stop("error while evaluating node "%+% self$cur.node$name %+%" formula: \n"%+%parse(text = expr_str)%+%".\nCheck syntax specification.", call. = FALSE)
  }
  return(list(evaled_expr = evaled_expr, par.nodes = NULL)) # return evaluated expression and parent node names
}

# ------------------------------------------------------------------------------------------
# **** MOVED THE ENTIRE THING TO R6 CLASS STRUCTURE:
# ------------------------------------------------------------------------------------------
# Function takes a string node formula, current node and current observed data environment
# 1) processes expression into R call, replaces t to its current value
# 2) finds all time-dep var names (Var[]) and non-time dep var names (Var)
# 3) replaces all summary function calls, s.a., func(Var) with apply(Var, 1, func)
# 4) replaces all calls to functions with several vectors, s.a., func(X1,X2,X3) with func(cbind(X1,X2,X3))
# 5) evaluates final expression in a special environment where: 
  # -) variables that have been simulated so far in obs.df are accessible
  # -) the subset vector function '[' is replaces with its specialized version, with syntax TDVar[t_range] for subsetting columns of the observed data by time
  # -) vecapply() function that is a wrapper for apply, converts vector to a 1 col matrix
# 6) standardizes the final expression to be a vector (NEED TO CHANGE FOR CATEGORICAL NODES - sapply over each prob formula in expression?)
eval.nodeform.out <- function(expr.idx, self, data.df) {
  expr_str <- self$exprs_list[[expr.idx]]
  sVar.name <- self$sVar.expr.names[expr.idx]
  misXreplace <- self$sVar.misXreplace[expr.idx]
  eval.asis <- self$asis.flags[[expr.idx]]

  if (is.character(expr_str) || is.numeric(expr_str)) {
    expr_call <- try(parse(text=expr_str)[[1]])   # parse expression into a call
    if(inherits(expr_call, "try-error")) {
      stop("error while evaluating node "%+% self$cur.node$name %+%" formula:\n "%+%parse(text=expr_str)%+%".\nCheck syntax specification.", call.=FALSE)
    }

  } else if (is.call(expr_str)){
    expr_call <- expr_str
    warning("node "%+%self$cur.node$name%+%": formula is already a parsed call")
  } else {
    stop("node "%+%self$cur.node$name%+%": currently can't process node formulas that are not strings or calls")
  }

  # Removed self$node_fun from data.env as they interfere with R expressions parsing in nodeform_parsers:
  # define the formula evaluation environment:
  eval.sVar.params <- c(list(self = self),
                        self$df.names(data.df), # special var "ANCHOR_ALLVARNMS_VECTOR_0" with names of already simulated vars
                        list(misXreplace = misXreplace), # replacement value for missing network covars
                        # list(netind_cl = self$netind_cl),
                        # list(t = self$cur.node$t),
                        # list(Kmax = self$netind_cl$Kmax),
                        list(Nsamp = self$Nsamp))
  # add number of friends for each unit if there is a network:
  if (!is.null(self$netind_cl) && ("NetIndClass" %in% class(self$netind_cl))) {
    eval.sVar.params <- append(eval.sVar.params, list(nF = self$netind_cl$nF))
  }
  # add the data:
  data.env <- c(eval.sVar.params, data.df)

  # If t is present (defined) for current node, replace all "t" in node formula by its actual value:
  if (!is.null(self$cur.node$t)) {
    expr_call <- eval(substitute(substitute(e, list(t = eval(self$cur.node$t))), list(e = expr_call)))
  }

  # If network is present replace all "Kmax" in the node formula by the actual network Kmax value:
  if (!is.null(self$netind_cl) && ("NetIndClass" %in% class(self$netind_cl))) {
    expr_call <- eval(substitute(substitute(e, list(Kmax = eval(self$netind_cl$Kmax))), list(e = expr_call)))
  }

  if (eval.asis) {
    return(eval.nodeform.asis(expr_call = expr_call, expr_str = expr_str, self = self, data.env = data.env))
  } else {
    return(eval.nodeform.full(expr_call = expr_call, expr_str = expr_str, self = self, data.env = data.env))
  }
}

# ***********************************************************************
# R6 PARSER COPIED FROM tmlenet: take sVar expression index and evaluate
# Corrected processes and names expressions that evaluate to matrices
# ***********************************************************************
parse.sVar.out <- function(sVar.idx, self, data.df) {
  # no.sVar.name <- self$sVar.noname[sVar.idx]
  # # no.sVar.name <- is.null(sVar.name) || (sVar.name %in% "")
  # if (is.matrix(evaled_expr)) {
  #   if (no.sVar.name) sVar.name <- colnames(evaled_expr)
  #   if (!no.sVar.name && ncol(evaled_expr) > 1) sVar.name <- sVar.name %+% "." %+% (1 : ncol(evaled_expr))
  #   if (no.sVar.name || ncol(evaled_expr) > 1) message(expr_str %+% ": the result matrix is assigned the following column name(s): " %+% paste(sVar.name, collapse = ","))
  # } else {
  #   if (no.sVar.name) stop(expr_str %+% ": summary measures not defined with Var[[...]] must be named.")
  #   evaled_expr <- as.matrix(evaled_expr)
  # }
  # colnames(evaled_expr) <- sVar.name
  # return(evaled_expr)
}

## ---------------------------------------------------------------------
#' @title Class for defining and evaluating user-specified summary measures (exprs_list)
#' @docType class
#' @format An R6 class object.
#' @name Define_sVar
#' @details Following fields are created during initialization
#' \itemize{
#' \item{nodes} ...
#' \item{subset_regs} ...
#' \item{sA_nms} ...
#' \item{sW_nms} ...
#' \item{Kmax} ...
#' }
#' Evaluates and and stores arbitrary summary measure expressions. 
#' The expressions (exprs_list) are evaluated in the environment of the input data.frame.
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
# @export
Define_sVar <- R6Class("Define_sVar",
  class = TRUE,
  portable = TRUE,
  public = list(
    Nsamp = NULL,                 # sample size (nrows) of the simulation dataset
    user.env = NULL,              # user environment to be used as enclos arg to eval(sVar)
    # Kmax = NULL,                  # special reserved variable for max number of friends
    cur.node = list(),            # current evaluation node (set by self$eval.nodeforms())
    netind_cl = NULL,             # pointer to network R6 class
    # netind_cl = NetIndClass$new(nobs = 0, Kmax = 0),
    asis.flags = list(),          # list of flags, TRUE for "as is" node expression evaluation
    ReplMisVal0 = FALSE,          # vector of indicators, for each TRUE sVar.expr[[idx]] will replace all NAs with gvars$misXreplace (0)
    sVar.misXreplace = NULL,      # replacement values for missing sVar, vector of length(exprs_list)
    sVar.noname = FALSE,          # vector, for each TRUE sVar.expr[[idx]] ignores user-supplied name and generates names automatically    

    exprs_list = list(),          # sVar expressions as a list
    sVar.expr.names = character(),# user-provided name of each sVar.expr
    sVar.names.map = list(),

    node_fun = list(
      vecapply = function(X, idx, func) { # custom wrapper for apply that turns a vector X into one column matrix
        if (is.vector(X)) dim(X) <- c(length(X), 1) # returns TRUE only if the object is a vector with no attributes apart from names
        # if (is.atomic(x) || is.list(x)) dim(X) <- c(length(X), 1) # alternative way to test for vectors
          x <- parse(text = deparse(func))[[1]]
          nargs <- length(x[[2]])
          if (nargs>1) {
            funline <- deparse(func)[1]
            stop(funline%+%". Node formulas cannot call non-vectorized functions with more than one named argument. If this is a vectorized function, pass its name to set.DAG(, vecfun=).")
          }
        apply(X, idx, func)
      },

      cbind_mod = function(...) { # cbind wrapper for c(,) calls in node formulas, turns one row matrix into repeat Nsamp row matrix
        env <- parent.frame()
        cbind_res <- do.call("cbind", eval(substitute(alist(...)), envir = env) , envir = env)
        if (nrow(cbind_res)==1) {
          Nsamp <- env$self$Nsamp
          assert_that(!is.null(Nsamp))
          if (Nsamp > 0) {
            cbind_res <- matrix(cbind_res, nrow = Nsamp, ncol = ncol(cbind_res), byrow = TRUE)  
          } else {
            cbind_res <- matrix(nrow = Nsamp, ncol = ncol(cbind_res), byrow = TRUE)
          } 
        }
        dprint("cbind_res"); dprint(cbind_res)
        cbind_res
      },

      # custom function for vector look up '['
      # function takes the name of the TD var and index vector => creates a vector of time-varying column names in df
      # returns matrix TD_var[indx]
      # ***NOTE: current '[' cannot evalute subsetting that is based on values of other covariates such as A1C[ifelse(BMI<5, 1, 2)]
      `[` = function(var, indx, ...) {
        env <- parent.frame()
        # t <- env$t
        t <- env$self$cur.node$t

        var <- substitute(var)
        var.chr <- as.character(var)

        if (is.null(t)) stop("references, s.a. Var[t] are not allowed when t is undefined")

        if (missing(indx)) stop("missing tindex when using Var[tindex] inside the node formula")
        if (max(indx)>t) stop(paste0(var, "[", max(indx),"] cannot be referenced in node formulas at t = ", t))  # check indx<= t

        if (identical(class(indx),"logical")) indx <- which(indx)

        # ******* NOTE *******
        # Don't like the current implementation that defines TDvars as characters and then returns a matrix by cbinding 
        # the existing columins in existing data.frame. This is possibly wasteful. Could we instead subset the existing data.frame?
        TDvars <- var.chr%+%"_"%+%indx
        # Checking the variables paste0(var, "_", indx) exist in simulated data.frame environment:
        dprint("ANCHOR_ALLVARNMS_VECTOR_0:"); dprint(env[["ANCHOR_ALLVARNMS_VECTOR_0"]])

        # ******* TO DO ******* 
        # EXTEND TO CHECKING FOR TDvar IN ENCLOSING ENVIRONMENT (user.env) AS WELL IF TDvar_t doesn't exist in the data
        # IF TDvar exists check that its a vector of appropriate length, index it accordinly (using which(t%in%tvec))
        # will need to first eval such vector the variable as in: 
        # var.val <- eval(var, envir = env)
        existsTDVar <- function(TDvar_t) TDvar_t %in% env[["ANCHOR_ALLVARNMS_VECTOR_0"]]
        check_exist <- sapply(TDvars, existsTDVar)
        if (!all(check_exist)) stop("undefined time-dependent variable(s): "%+%TDvars[which(!check_exist)])
        # THIS STEP COULD BE MORE MEMORY EFFICIENT IF WAS SUBSETTING INSTEAD (BY COLS) ON EXISTING data MATRIX:
        TDvars_eval <- eval(parse(text=paste0("cbind(",paste0(TDvars, collapse=","),")")), envir = env)
        return(TDvars_eval)
      },

      # Builds netVar matrix by using matrix env$NetIndobj$NetInd_k, cbind on result
      # For W[[0]] to work without if else below need to do this:
      # NetInd_k <- cbind(c(1:n), NetInd_k) and then netidx <- netidx + 1
      `[[` = function(var, netidx, ...) {
        env <- parent.frame()
        # t <- env$t
        t <- env$self$cur.node$t
        var <- substitute(var)
        var.chr <- as.character(var)
        # netind_cl <- env$netind_cl
        netind_cl <- env$self$netind_cl
        Kmax <- netind_cl$Kmax

        if (is.null(netind_cl) || is.null(Kmax)) stop("Network must be defined (with simcausal::network(...)) prior to using Var[[netidx]] node syntax")
        if (missing(netidx)) stop("missing netidx when using Var[[netidx]] inside the node formula")

        if (identical(class(netidx),"logical")) netidx <- which(netidx)

        # now checking if var has been previously defined only if its is.name()
        if (is.name(var)) {
          if (! (var.chr %in% env[["ANCHOR_ALLVARNMS_VECTOR_0"]])) {
            stop("variable " %+% var %+% " doesn't exist")
          }
        }

        # evaluate in its own environment, in case its fun(var[tindx])[[netindx]]-type expression:
        # if (is.call(var)) {
        var.val <- try(eval(var, envir = env))
        # var.val <- eval(var, envir = env)
        if(inherits(var.val, "try-error")) {
          stop("\n...attempt to evaluate network indexing variable failed...")
        }
        
        # if result is one column matrix -> convert to a vector, if matrix has >1 columns -> throw an error:
        if (length(dim(var.val)) > 1) {
          var.chr <- colnames(var.val)[1]
          if (dim(var.val)[2]==1) var.val <- as.vector(var.val) else stop("\n...network indexing variable evaluated to more than one column ...")
        }
        if (length(var.chr)>1) var.chr <- "X"
        n <- length(var.val)

        if (identical(class(netidx),"logical")) netidx <- which(netidx)
        netVars_eval <- matrix(0L, nrow = n, ncol = length(netidx))
        colnames(netVars_eval) <- netvar(var.chr, netidx)

        for (neti in seq_along(netidx)) {
          if (netidx[neti] %in% 0L) {
            netVars_eval[, neti] <- var.val
          } else {
            netVars_eval[, neti] <- var.val[netind_cl$NetInd_k[, netidx[neti]]]
            # opting for replace on entire netVars_eval, will need to do benchmarks later to compare:
            # netVars_eval[is.na(netVars_eval[, neti]), neti] <- env$misXreplace
          }
        }
        # need to do benchmarks later to compare to column based replace:
        netVars_eval[is.na(netVars_eval)] <- env$misXreplace
        return(netVars_eval)
      }
    ),

    # No longer capture the user environment here; capture node-specific/expression specific user.env instead in self$set.user.env()
    # this user.env is then used for eval'ing each sVar exprs (enclos = user.env)
    # initialize = function(user.env, netind_cl) {
    # initialize = function(netind_cl) {
    initialize = function() {
      # self$user.env <- user.env
      # self$netind_cl <- netind_cl
      # self$Kmax <- self$netind_cl$Kmax
      invisible(self)
    },

    set.new.exprs = function(exprs_list) {
      self$exprs_list <- exprs_list
      self$sVar.expr.names <- names(self$exprs_list)
      dprint("self$sVar.expr.names: "); dprint(self$sVar.expr.names)

      self$asis.flags <- attributes(exprs_list)[["asis.flags"]]
      if (is.null(self$asis.flags)) { 
        self$asis.flags <- as.list(rep.int(FALSE, length(exprs_list)))
        names(self$asis.flags) <- names(exprs_list)
      }

      if (is.null(self$sVar.expr.names)) self$sVar.expr.names <- rep_len("", length(self$exprs_list))
      if (length(self$exprs_list) != 0 && (is.null(self$sVar.expr.names) || any(self$sVar.expr.names==""))) {
        stop("must provide a name for each node expression")
      }

      if (any(self$sVar.expr.names %in% "replaceNAw0")) {
        ReplMisVal0.idx <- which(self$sVar.expr.names %in% "replaceNAw0")
        self$ReplMisVal0 <- as.logical(self$exprs_list[[ReplMisVal0.idx]])
        self$sVar.expr.names <- self$sVar.expr.names[-ReplMisVal0.idx]
        self$exprs_list <- self$exprs_list[-ReplMisVal0.idx]
        self$asis.flags <- self$asis.flags[-ReplMisVal0.idx]
        dprint("Detected replaceNAw0 flag with value: " %+% self$ReplMisVal0);
      }

      if (any(self$sVar.expr.names %in% "noname")) {
        noname.idx <- which(self$sVar.expr.names %in% "noname")
        self$sVar.noname <- as.logical(self$exprs_list[[noname.idx]])
        self$sVar.expr.names <- self$sVar.expr.names[-noname.idx]
        self$exprs_list <- self$exprs_list[-noname.idx]
        self$asis.flags <- self$asis.flags[-noname.idx]
        dprint("Detected noname flag with value: " %+% self$sVar.noname);
      }

      self$ReplMisVal0 <- rep_len(self$ReplMisVal0, length(self$exprs_list))
      self$sVar.misXreplace <- ifelse(self$ReplMisVal0, gvars$misXreplace, gvars$misval)
      self$sVar.noname <- rep_len(self$sVar.noname, length(self$exprs_list))
      dprint("Final node expression(s): "); dprint(self$exprs_list)
      invisible(self)
    },

    eval.nodeforms = function(cur.node, data.df) {
      self$setnode.setenv(cur.node)
      self$ReplMisVal0 <- FALSE
      # Parse the node formulas (parameters), set self$exprs_list, set new self$sVar.misXreplace and self$sVar.noname if found:
      self$set.new.exprs(exprs_list = cur.node$dist_params)
      self$Nsamp <- nrow(data.df)

      sVar.res_l <- lapply(seq_along(self$exprs_list), eval.nodeform.out, self = self, data.df = data.df)
      names(sVar.res_l) <- names(self$exprs_list)

      # USE BELOW NAMING AND MAPPING SCHEME WHEN SAMPLING MULTIVAR RVs:
      # SAVE THE MAP BETWEEEN EXPRESSION NAMES AND CORRESPONDING COLUMN NAMES:
      # self$sVar.names.map <- lapply(sVar.res_l, colnames)
      # mat.sVar <- do.call("cbind", sVar.res_l)
      return(sVar.res_l)
    },

    # Parse the R expression for the EFU node arg (if not null)
    eval.EFU = function(cur.node, data.df) {
      self$setnode.setenv(cur.node)
      self$ReplMisVal0 <- FALSE
      if (!is.null(cur.node$EFU)) {
        self$set.new.exprs(exprs_list = list(EFU = cur.node$EFU))
        EFU.res <- eval.nodeform.out(expr.idx = 1, self = self, data.df = data.df)$evaled_expr
        if (length(EFU.res)==1) EFU.res <- rep.int(EFU.res, self$Nsamp)
        assert_that(all(is.logical(EFU.res) || is.null(EFU.res)))
        return(EFU.res)
      } else {
        return(NULL)
      }
    },

    # list of variable names from data.df with special var name (ANCHOR_ALLVARNMS_VECTOR_0)
    df.names = function(data.df) {
      return(list(ANCHOR_ALLVARNMS_VECTOR_0 = colnames(data.df)))
    },

    # Set the current node and set the node environment
    # This user.env is used for eval'ing each sVar exprs (enclos = user.env)
    setnode.setenv = function(cur.node) {
    # set.user.env = function(user.env) {
      assert_that(is.node(cur.node) || is.Netnode(cur.node))

      user.env <- cur.node$node.env
      assert_that(!is.null(user.env))
      assert_that(is.environment(user.env))

      self$cur.node <- cur.node
      self$user.env <- user.env
      invisible(self)
    },

    set.net = function(netind_cl) {
      assert_that("NetIndClass" %in% class(netind_cl))
      self$netind_cl <- netind_cl
      invisible(self)
    }    
  ),

  active = list(
    placeholder = function() {}
  ),

  private = list(
    privplaceholder = function() {}
  )
)