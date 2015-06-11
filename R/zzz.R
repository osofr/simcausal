.onAttach <- function(...) {
	packageStartupMessage('simcausal')
	packageStartupMessage('Version: ', utils::packageDescription('simcausal')$Version)
	packageStartupMessage('Package created on ', utils::packageDescription('simcausal')$Date, '\n')
	packageStartupMessage('Please note the package is in its early development stage', '\n')
	packageStartupMessage('Check for updates and report bugs at http://github.com/osofr/simcausal', '\n')
	packageStartupMessage('To see the vignette use vignette("simcausal_vignette", package="simcausal"); To see the overview help file use ?simcausal; To see the overview of all documentation use help(package = "simcausal");', '\n')
	packageStartupMessage('Use simcausalNEWS() to see recent updates for this version of the package', '\n')
}

#' Show the NEWS file for the simcausal package
#' @param \dots additional arguments passed to \code{RShowDoc}
#' @return A invisible path to the NEWS file
#' @export
simcausalNEWS <- function(...) {
	RShowDoc("NEWS", package = "simcausal", ...)
}
