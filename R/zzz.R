# Runs when attached to search() path such as by library() or require()
.onAttach <- function(...) {
  if (interactive()) {
  	packageStartupMessage('simcausal')
  	packageStartupMessage('Version: ', utils::packageDescription('simcausal')$Version)
  	packageStartupMessage('Package created on ', utils::packageDescription('simcausal')$Date, '\n')
  	packageStartupMessage('Please note this package is still in its early stages of development. Check for updates and report bugs at http://github.com/osofr/simcausal.', '\n')
  	packageStartupMessage('To see the vignette use vignette("simcausal_vignette", package="simcausal"). To see all available package documentation use help(package = "simcausal") and ?simcausal.', '\n')
  	packageStartupMessage('To see the latest updates for this version, use news(package = "simcausal").', '\n')
  }
}

# Runs when loaded but not attached to search() path; e.g., when a package just Imports (not Depends on) simcausal
.onLoad <- function(libname, pkgname) {
    # Set simcausal package options, # simcausal.<argument name>
    opts = c("simcausal.verbose"="TRUE"
            )
    for (i in setdiff(names(opts),names(options()))) {
        eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
    }
    invisible()
}

# .onUnload <- function(libpath) {
# }