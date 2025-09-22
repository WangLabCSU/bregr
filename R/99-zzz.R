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
Cite as      : https://doi.org/10.1002/mdr2.70028
  Wang, S., Peng, Y., Shu, C., Wang, C., Yang, Y., Zhao, Y., Cui, Y., Hu, D. and Zhou, J.-G. (2025),
  bregr: An R Package for Streamlined Batch Processing and Visualization of Biomedical Regression Models. Med Research.
=======================================================================
                 ")
  packageStartupMessage(msg)
}
