.onAttach <- function(...) {
	packageStartupMessage('simcausal')
	packageStartupMessage('Version: ', utils::packageDescription('simcausal')$Version)
	packageStartupMessage('Package created on ', utils::packageDescription('simcausal')$Date, '\n')
	packageStartupMessage('Please note this package is still in its early stages of development. Check for updates and report bugs at http://github.com/osofr/simcausal.', '\n')
	packageStartupMessage('To see the vignette use vignette("simcausal_vignette", package="simcausal"). To see all available package documentation use help(package = "simcausal") and ?simcausal.', '\n')
	packageStartupMessage('To see the latest updates for this version, use news(package = "simcausal").', '\n')
}
