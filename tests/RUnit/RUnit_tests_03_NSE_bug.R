# library("simcausal")

test.NSEbug <- function() {

  lookup_parameter <- function(pp, node, s, t, variable) {
    print("s"); print(s)
    print("t"); print(t)
    print("variable"); print(variable)
    pp["t" == t & "s" == s,]
    pp[pp[,"t"] %in% 0 & pp[,"s"] %in% 1,"param"]
  }

  tFUN <- function(s, t, SS) {
    s*t
  }

  pp <- 'name'
  t_start <- 0
  TT <- 2
  s_start <- 1
  SS <- 3

  pp <- data.frame(t = t_start:TT, s = s_start:SS, param = runif(3))

  # vecfun.add("lookup_parameter")

  D <- DAG.empty()

  for(t in t_start:TT ){ # Per month
      for(s in s_start:SS ){ # Per location

        tpos  <- tFUN(s, t, SS)
        ## Initial month values ##

        # if(s == 1 & t == 0){
         D <- D + node('L1',
                 distr = 'rnorm',
                 mean  = .(lookup_parameter(pp, node = 'L1', s = s, t = t, variable = '(Intercept)') ),
                 # mean  = lookup_parameter(pp, node = 'L1', s = .(s), t = t, variable = '(Intercept)'),
                 sd    = .(lookup_parameter(pp, node = 'L1', s = s, t = t, variable = 'mse') ),
                 # sd    = lookup_parameter(pp, node = 'L1', s = .(s), t = t, variable = 'mse'),
                 t     = tpos)
        # }
    }
  }
  D <- set.DAG(D)

}
