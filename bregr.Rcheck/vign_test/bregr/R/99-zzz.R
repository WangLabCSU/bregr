.onLoad <- function(...) {
  S7::methods_register()
}

.onAttach <- function(libname, pkgname) {
  version <- packageDescription(pkgname, fields = "Version")

  msg <- paste0("Welcome to 'bregr' package!
=======================================================================
", "You are using ", pkgname, " version ", version, "

Project home : https://github.com/WangLabCSU/bregr
Documentation: https://wanglabcsu.github.io/bregr/
Cite as      : arXiv:2110.14232
=======================================================================
                 ")
  packageStartupMessage(msg)
}
