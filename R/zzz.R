## Messages to be displayed when the user loads schoenberg:
.onAttach <- function(libname, pkgname) {
    version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(crayon::white("----------------------------------------------------- ", crayon::bold(paste(pkgname, "version", version)), " --"))
    packageStartupMessage("Please report any bugs or feature requests to ", crayon::italic("github.com/jadahlke/schoenberg/issues"), "\n")
}
