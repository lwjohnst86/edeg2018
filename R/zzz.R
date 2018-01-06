#' @import dplyr tidyr mason ggplot2
#' @importFrom magrittr %>%
`%>%` <- magrittr::`%>%`

# This function loads everything after using `devtools::load_all()` so you don't
# need to include all these functions in each code chunk.
#
.onLoad <- function(libname, pkgname) {
    set_options()
}
