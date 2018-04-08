# Renaming ----------------------------------------------------------------

renaming_fats <- function(x, keep.fraction = TRUE) {
    x %>%
        stringr::str_remove('^pct_') %>%
        PROMISE.misc::renaming_fa(keep.fraction = keep.fraction)
}
