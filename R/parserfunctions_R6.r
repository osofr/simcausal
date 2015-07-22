
# ------------------------------------------------------------------------------------------------------
# FUNCTIONS FOR PARSING NODE FORMULA EXPRESSIONS
# ------------------------------------------------------------------------------------------------------

# FUNCTION NAMES THAT PRODUCE A VECTOR WHEN APPLIED TO A VECTOR (WILL NOT BE REPLACED BY apply(df,1,func)):
vector_fcns <- c("cbind_mod","vecapply","apply","rowSums","rowMeans", "(", "[", "{", ":", "rep", "length", "if")
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
nodeparsefun <- list(
  nodeform_parsers <- function(node_form_call) {
    # combine all default vectorized funs + the user-specified vectorized function in global :
    vector_fcns_all <- c(vector_fcns, vector_ops_fcns, vector_math_fcns, vecfun.get())
    print("vector_fcns_all"); print(vector_fcns_all)

    # (not USED) SUMMARY FCNS (non-vectorized): these will be always turned into apply(arg, 1, func)
    # summary_fcns <- c("c","all","any","sum","mean","prod","min","max","range")
    # (not USED) FOR FUTURE IMPLEMENTATION: FUNCTION NAMES THAT AREN'T ALLOWED IN FORMULA EXPRESSIONS:
    # banned_fcns <- c( "apply", "cbind", "&&", "||")

    # * recursively parse the call tree structure for a given expression, find call to '[', output the first argument (TDVar name that is being called as TDVar[])
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
            varnames <- as.character(as.character(x[[2]]) %+% "_" %+% eval(x[[3]]))
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
        # dprint("-------------"); dprint(samecall); dprint(eval_atom_call); dprint("-------------")
        preveval_atom_call <- eval_atom_call; i <- i + 1
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
        # if (is.atomic(x)) dprint("atomic: "%+%x)
        # if (is.name(x)) dprint("name: "%+%x)
        x	# Leave unchanged
      } else if (is.call(x)) {
        if (identical(x[[1]], quote(`[`)) && is.name(x[[2]])) {	# reached '[' function, don't need to parse any deeper, return this subtree intact
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

    # Parses the formula and gets all the variable names referenced as [] or as.name==TRUE
    # **** THIS IS NEEDED FOR PARENTS DAG FIGURE ****
    # This doesn't locate all names, e.g., those not referenced by Var[], so Vnames will be incomplete
    # TO DO: Need to evaluate this in the simulated data + user.env only
    Vnames <- find_FormVars(eval_atom_call, vartype="non_TD")	# returns unique names of none TD vars that were called as VarName
    TD_vnames <- find_FormVars(eval_atom_call, vartype="TD")	# returns unique names TDVar that were called as TDVar[indx]
    TD_t_vnames <- find_FormVars(eval_atom_call, vartype="TD_t") # returns unique names TDVar_t that were called as TDVar[indx]

    # dprint("Vnames"); dprint(Vnames)
    # dprint("TD_vnames"); dprint(TD_vnames)
    # dprint("TD_t_vnames: "); dprint(TD_t_vnames)

    modified_call <- modify_call(eval_atom_call) 			# parse current call and replace any non-vectorized function with apply call (adding cbind_mod if more than one arg)
    return(list(Vnames=Vnames, TD_vnames=TD_vnames, TD_t_vnames=TD_t_vnames, modified_call=modified_call))
  }
)


# ------------------------------------------------------------------------------------------
# **** TO DO: MOVING THE ENTIRE THING TO R6 CLASS STRUCTURE:
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
eval_nodeform_out <- function(expr_str, cur.node, self, data.df) {
# eval_nodeform <- function(expr_str, cur.node, self, env=parent.frame()) {
  sVar.expr <- self$expr_list[[sVar.idx]]
  sVar.name <- self$sVar.expr.names[sVar.idx]
  misXreplace <- self$sVar.misXreplace[sVar.idx]

  if (is.character(expr_str)) {
    expr_call <- try(parse(text=expr_str)[[1]])   # parse expression into a call
    if(inherits(expr_call, "try-error")) {
      stop("error while evaluating node "%+% cur.node$name %+%" parameters: "%+%expr_str%+%".\nCheck syntax specification.", call.=FALSE)
    }
  } else if (is.call(expr_str)){
    expr_call <- expr_str
    warning("node "%+%cur.node$name%+%": formula is already a parsed call")
  } else {
    stop("node "%+%cur.node$name%+%": currently can't process node formulas that are not strings or calls")
  }

  # *********** Q: WHY do we need self$df.names(data.df) available in the evaluation environment????
  # *********** Its no longer needed since we are looking up the var names directly in env[["ANCHOR_ALLVARNMS_VECTOR_0"]]...
  # *********** REMOVING:
  # eval.sVar.params <- c(self$df.names(data.df), list(misXreplace = misXreplace), list(netind_cl = self$netind_cl))
  eval.sVar.params <- c(list(misXreplace = misXreplace), list(netind_cl = self$netind_cl))
  data.env <- c(eval.sVar.params, self$node_fun, data.df)

  # Replace t in the node formula expression with current t value; Replace Kmax its val (returns a call)
  subst_call <- eval(substitute(substitute(e, list(t = eval(cur.node$t), Kmax = eval(self$Kmax))), list(e = subst_call)))
  # subst_call <- eval(substitute(substitute(e, list()), list(e = expr_call)))

  # parse_res <- nodeparsefun$nodeform_parsers(subst_call) # traverse the node formula call, return TDvar & Var names (node parents) and modify subst_call to handle non-vectorized (summary functions)
  # BELOW MIGHT NOT WORK, MIGHT NEED TO MODIFY subst_call FIRST
  # Creating environment just for parsing the call that consists of the call itself, nodeform_parsers fun and data, enclosed by the user.env
  # WILL NEED TO ADD vectorized fun list:
  parsecall.env <- c(list(subst_call = subst_call), nodeparsefun, data.df)
  parse_res <- try(eval(substitute(nodeform_parsers(subst_call)), envir = parsecall.env, enclos = self$user.env))

  # ****************************************************************
  # THIS IS NO LONGER NEEDED, SEE parser in Define_sVar class for easier way to do this:
  # ****************************************************************
  # set the local variables in the formula node to their character values:
  # Vnames  <- setNames(parse_res$Vnames, parse_res$Vnames)
  # TD_vnames <- setNames(parse_res$TD_vnames, parse_res$TD_vnames)
  # TD_t_vnames <- setNames(parse_res$TD_t_vnames, parse_res$TD_t_vnames)

  modified_call <- parse_res$modified_call # modified call that has any non-vectorized function replaced with apply call (with cbind for more than one arg)

  dprint("----------------------")
  dprint("node: "%+%cur.node$name)
  dprint("original expr as call:"); dprint(expr_call)
  # dprint("call with replaced t:"); dprint(subst_call)
  dprint("final exprs:"); dprint(parse_res$modified_call)
  dprint("----------------------")

  # ****************************************************************
  # THIS IS NO LONGER NEEDED, SEE parser in Define_sVar class for easier way to do this:
  # ****************************************************************
  # anchor_evn <- where("ANCHOR_VARS_OBSDF", env)
  df_varnms <- self$df.names(data.df)
  dprint("df_varnms"); dprint(df_varnms);
  # check this TD Var doesn't exist in parent environment if df has TD Var => TD Var is not time-dependent and reference TDVar[t] is incorrect - throw exception
  for (TDname in TD_vnames) {
    if (TDname%in%df_varnms) stop(paste0("reference ", TDname, "[...]", " at node ", cur.node$name, " is not allowed; node ", TDname," was defined as time-invariant"))
  }

  # check Var (not TD) already exists in parent environment:
  for (Vname in Vnames) {
    if (!(Vname%in%df_varnms)) stop(paste0("formula at node ", cur.node$name, " cannot be evaluated; node ", Vname," is undefined"))
  }

  if (is.call(modified_call) && identical(try(modified_call[[1]]), quote(`{`))) { # check for '{' as first function, if so, remove first func, turn call into a list of calls and do lapply on eval
  # print("call and { braces passed...")
  modified_call_nocurl <- modified_call[-1]
  # evaled_expr <- try(lapply(X=modified_call_nocurl, FUN=eval, envir = c(lapply(TD_vnames, I), node_func), enclos=anchor_evn))
  evaled_expr <- try(lapply(X=modified_call_nocurl, FUN=eval, envir = data.env, enclos = self$user.env))
  } else {
  # evaled_expr <- try(eval(modified_call, envir = c(lapply(TD_vnames, I), node_func), enclos=anchor_evn))  # evaluate modified_call in the df namespace with custom '[' function
  evaled_expr <- try(eval(modified_call, envir = data.env, enclos = self$user.env)) # eval'ing expr in the envir of data.df
  }

  if(inherits(evaled_expr, "try-error")) {
    stop("error while evaluating node "%+% cur.node$name %+%" parameters: "%+%expr_str%+%".\nCheck syntax specification.")
  }
  dprint("evaled_expr"); dprint(evaled_expr)

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

  return(list(evaled_expr=evaled_expr, par.nodes=c(Vnames,TD_t_vnames))) # return evaluated expression and parent node names
}


# ****************************************************************
# NEW R6 PARSER COPIED FROM tmlenet:
# ****************************************************************
# take sVar expression index and evaluate:
parse.sVar.out <- function(sVar.idx, self, data.df) {
  sVar.expr <- self$expr_list[[sVar.idx]]
  sVar.name <- self$sVar.expr.names[sVar.idx]
  misXreplace <- self$sVar.misXreplace[sVar.idx]

  # ******
  eval.sVar.params <- c(list(misXreplace = misXreplace), list(netind_cl = self$netind_cl))
  # eval.sVar.params <- c(self$df.names(data.df), list(misXreplace = misXreplace), list(netind_cl = self$netind_cl))
  data.env <- c(eval.sVar.params, self$node_fun, data.df)
  # ******

  if (is.character(sVar.expr)) {
    sVar.expr_call <- try(parse(text=sVar.expr)[[1]])   # parse expression into a call
    if(inherits(sVar.expr_call, "try-error")) {
      stop("error while evaluating expression: " %+% sVar.expr %+% ".\nCheck syntax specification.", call.=FALSE)
    }
  } else if (is.call(sVar.expr)){
    sVar.expr_call <- sVar.expr
    warning(sVar.expr_call %+% ": sVar formula is already a parsed call")
  } else {
    stop("sVar formula class: " %+% class(sVar.expr) %+% ". Currently can't process sVar formulas that are not strings or calls.")
  }

  sVar.expr_call <- eval(substitute(substitute(e, list(Kmax = eval(self$Kmax))), list(e = sVar.expr_call))) # Replace Kmax its val
  evaled_expr <- try(eval(sVar.expr_call, envir = data.env, enclos = self$user.env)) # eval'ing expr in the envir of data.df

  no.sVar.name <- self$sVar.noname[sVar.idx]
  # no.sVar.name <- is.null(sVar.name) || (sVar.name %in% "")

  if (is.matrix(evaled_expr)) {
    if (no.sVar.name) sVar.name <- colnames(evaled_expr)
    if (!no.sVar.name && ncol(evaled_expr) > 1) sVar.name <- sVar.name %+% "." %+% (1 : ncol(evaled_expr))
    if (no.sVar.name || ncol(evaled_expr) > 1) message(sVar.expr %+% ": the result matrix is assigned the following column name(s): " %+% paste(sVar.name, collapse = ","))
  } else {
    if (no.sVar.name) stop(sVar.expr %+% ": summary measures not defined with Var[[...]] must be named.")
    evaled_expr <- as.matrix(evaled_expr)
  }

  colnames(evaled_expr) <- sVar.name
  return(evaled_expr)
}

## ---------------------------------------------------------------------
#' @title Class for defining and evaluating user-specified summary measures (expr_list)
#' @docType class
#' @format An R6 class object.
#' @name mcEvalPsi
#' @details Following fields are created during initialization
#' \itemize{
#' \item{nodes} ...
#' \item{subset_regs} ...
#' \item{sA_nms} ...
#' \item{sW_nms} ...
#' \item{Kmax} ...
#' }
#' Evaluates and and stores arbitrary summary measure expressions. 
#' The expressions (expr_list) are evaluated in the environment of the input data.frame.
#' @importFrom R6 R6Class
#' @importFrom assertthat assert_that
##' @export
Define_sVar <- R6Class("Define_sVar",
  class = TRUE,
  portable = TRUE,
  public = list(
    user.env = emptyenv(),        # user environment to be used as enclos arg to eval(sVar)
    # data.df = NULL,               # data.frame that is used for evaluation of sVar expressions (passed to get.mat.sVar)
    ReplMisVal0 = FALSE,          # vector of indicators, for each TRUE sVar.expr[[idx]] will replace all NAs with gvars$misXreplace (0)
    sVar.misXreplace = NULL,      # replacement values for missing sVar, vector of length(expr_list)
    sVar.noname = FALSE,          # vector, for each TRUE sVar.expr[[idx]] ignores user-supplied name and generates names automatically
    netind_cl = NULL,
    Kmax = NULL,

    expr_list = list(),     #  sVar expressions as a list
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
      cbind_mod = function(...) { # wrapper for cbind that expands one row matrix to nrows=Nsamp
        env <- parent.frame()
        cbind_res <- do.call("cbind", eval(substitute(alist(...)), envir = env) , envir = env)
        if (nrow(cbind_res)==1) {
          Nsamp <- get("Nsamp", envir =env)
          cbind_res <- matrix(cbind_res, nrow= Nsamp, ncol=ncol(cbind_res), byrow = TRUE)
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
        t <- env$t # t <- get("t", envir = env);
        # netind_cl <- env$netind_cl
        dprint("t in Var[t] call: "%+%t)

        if (missing(indx)) stop("missing index for [...] while using Var[t] formula reference")
        if (identical(class(indx),"logical")) indx <- which(indx)

        # *** Consider removing, since there may be interest in using Var[idx] even when t doesn't exist ***
        if (is.null(t)) stop("references, s.a. Var[t] are not allowed when t is undefined")
        # *** Consider removing, since some attributes may be indexed by indx > t and still not violate any time order:
        if (max(indx)>t) stop(paste0(var, "[", max(indx),"] cannot be referenced in node formulas at t = ", t))  # check indx<= t

        # COPIED FROM '[[' fun:
        var <- substitute(var)
        var.chr <- as.character(var)

        # ******* NOTE *******
        # I don't like the current implementation that defines TDvars as characters and then returns a matrix by cbinding 
        # the existing columins in existing data.frame. This is possibly wasteful.
        # Could we instead subset the existing data.frame?
        TDvars <- var.chr%+%"_"%+%indx

        # check the variables paste0(var, "_", indx) exist in simulated data.frame environment:
        # NEED ANOTHER METHOD FOR CHECKING TDvar existance...
        # e.g., try(eval(TDvar)), then is.vector() and length == n?
        # another approach is to modify node_fun to include ANCHOR_ALLVARNMS_LIST that contains a list of character names of all vars...


        # REPLACE THIS WITH JUST A CHECK FOR EXISTING VARIABLE NAME in data.frame OR the enclosing user environment
        # so, just exists() with inherits = TRUE
        # anchor_evn <- where("ANCHOR_VARS_OBSDF", env)
        
        # TODO: EXTEND TO CHECKING THE ENCLOSING ENVIRONMENT AS WELL:
        existsTDVar <- function(TDvar_t) TDvar_t %in% env[["ANCHOR_ALLVARNMS_VECTOR_0"]]
        check_exist <- sapply(TDvars, existsTDVar)
        # check_exist <- sapply(TDvars, function(TDvar) exists(TDvar, where = anchor_evn, inherits = FALSE))
        # anchor_evn$ANCHOR_VARS_OBSDF <- append(anchor_evn$ANCHOR_VARS_OBSDF, list(TDvars)) # only needed for debugging
        if (!all(check_exist)) stop("undefined time-dependent variable(s): "%+%TDvars[which(!check_exist)])
        # dprint("TDvars in '[': "); dprint(TDvars)
        # dprint("exprs text in '[': ");
        # dprint(paste0("cbind(",paste0(TDvars, collapse=","),")"))


        # COPIED FROM '[[' fun:
        # var.val <- eval(var, envir = env)
        # THIS STEP COULD BE MORE MEMORY EFFICIENT IF WAS SUBSETTING INSTEAD (BY COLS) ON EXISTING data MATRIX:
        TDvars_eval <- eval(parse(text=paste0("cbind(",paste0(TDvars, collapse=","),")")), envir = env)


        return(TDvars_eval)
      },

      # Builds netVar matrix by using matrix env$NetIndobj$NetInd_k, cbind on result
      # For W[[0]] to work without if else below need to do this:
      # NetInd_k <- cbind(c(1:n), NetInd_k) and then netidx <- netidx + 1
      `[[` = function(var, netidx, ...) {
        env <- parent.frame()
        t <- env$t # t <- get("t", envir = env);
        netind_cl <- env$netind_cl
        Kmax <- netind_cl$Kmax

        if (!is.null(t)) stop("simultaneous time varying node references Var[t] and network references Var[[netidx]] are currently not supported")
        if (missing(netidx)) stop("index in [[...]] must be provided when using Var[[netidx]] formula reference")
        if (identical(class(netidx),"logical")) netidx <- which(netidx)

        var <- substitute(var)
        var.chr <- as.character(var)
        if (! (var.chr %in% env[["ANCHOR_ALLVARNMS_VECTOR_0"]])) stop("variable " %+% var.chr %+% " doesn't exist")
        var.val <- eval(var, envir = env)

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

    # capture sVar expressions and capture the user environment;
    # user.env is used when eval'ing sVar exprs (enclos = user.env)
    initialize = function(expr_list, user.env, netind_cl) {
      self$user.env <- user.env
      self$expr_list <- expr_list
      self$netind_cl <- netind_cl
      self$Kmax <- self$netind_cl$Kmax
      self$sVar.expr.names <- names(self$expr_list)

      if (is.null(self$sVar.expr.names)) self$sVar.expr.names <- rep_len("", length(self$expr_list))
      if (length(self$expr_list) != 0 && (is.null(self$sVar.expr.names) || any(self$sVar.expr.names==""))) {
        stop("must provide a name for each summary measure expression")
        # dprint("Some summary measures were not named, automatic column name(s) will be generated during evaluation")
      }
      if (any(self$sVar.expr.names %in% "replaceMisVal0")) {
        ReplMisVal0.idx <- which(self$sVar.expr.names %in% "replaceMisVal0")
        self$ReplMisVal0 <- as.logical(self$expr_list[[ReplMisVal0.idx]])
        self$sVar.expr.names <- self$sVar.expr.names[-ReplMisVal0.idx]
        self$expr_list <- self$expr_list[-ReplMisVal0.idx]
        dprint("Detected replaceMisVal0 flag with value: " %+% self$ReplMisVal0);
      }
      if (any(self$sVar.expr.names %in% "noname")) {
        noname.idx <- which(self$sVar.expr.names %in% "noname")
        self$sVar.noname <- as.logical(self$expr_list[[noname.idx]])
        self$sVar.expr.names <- self$sVar.expr.names[-noname.idx]
        self$expr_list <- self$expr_list[-noname.idx]
        dprint("Detected noname flag with value: " %+% self$sVar.noname);
      }
      self$ReplMisVal0 <- rep_len(self$ReplMisVal0, length(self$expr_list))
      self$sVar.misXreplace <- ifelse(self$ReplMisVal0, gvars$misXreplace, gvars$misval)
      self$sVar.noname <- rep_len(self$sVar.noname, length(self$expr_list))
      dprint("Final summary measure expression(s): "); dprint(self$expr_list)
      invisible(self)
    },

    # (NOT USED IN THIS PACKAGE....)
    get.mat.sVar = function(data.df) {
      # call lapply on parse.sVar.out for each sVar in sVar.expr.names -> sVar.res_l
      sVar.res_l <- lapply(seq_along(self$expr_list), parse.sVar.out, self = self, data.df)
      names(sVar.res_l) <- self$sVar.expr.names

      # SAVE THE MAP BETWEEEN EXPRESSION NAMES AND CORRESPONDING COLUMN NAMES:
      self$sVar.names.map <- lapply(sVar.res_l, colnames)
      # print("sVar.res_l: "); print(sVar.res_l)
      # print("data.frame(sVar.res_l): "); print(data.frame(sVar.res_l))
      mat.sVar <- do.call("cbind", sVar.res_l)
      return(mat.sVar)
      # self$mat.sVar <- do.call("cbind", sVar.res_l)
      # invisible(self)
    },

    eval_nodeforms = function(cur.node, data.df) {
      dprint("cur.node dist_params"); dprint(cur.node$dist_params)
      # DOES expr_str NEED TO BE A CHARACTER? CAN IT BE THE CAPTURED CALL STORED IN cur.node?
      lapply(cur.node$dist_params, function(expr) eval_nodeform_out(expr_str = expr, cur.node = cur.node, self = self, data.df = data.df))
      # lapply(cur.node$dist_params, function(expr) eval_nodeform_out(as.character(expr), cur.node = cur.node, self = self, data.df = data.df))
      # eval_nodeform_out(expr_str = expr_str, cur.node = cur.node, self = self, data.df = data.df)
    },

    df.names = function(data.df) { # list of variable names from data.df with special var name (ANCHOR_ALLVARNMS_VECTOR_0)
      return(list(ANCHOR_ALLVARNMS_VECTOR_0 = colnames(data.df)))
      # allvarnms <- list(ANCHOR_ALLVARNMS_VECTOR_0 = vector())
      # allvarnms[["ANCHOR_ALLVARNMS_VECTOR_0"]] <- append(allvarnms[["ANCHOR_ALLVARNMS_VECTOR_0"]], colnames(data.df))
      # allvarnms[["ANCHOR_ALLVARNMS_VECTOR_0"]] <- unique(allvarnms[["ANCHOR_ALLVARNMS_VECTOR_0"]])
      # return(allvarnms)
    }
  ),

  active = list(
    placeholder = function() {}
  ),

  private = list(
    privplaceholder = function() {}
  )
)