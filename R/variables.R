
outcomes <- c("lISI", "lISSI2")
fa_pct <- stringr::str_subset(varnames, "pct_")
fa_conc <- stringr::str_subset(varnames, "^(ne|pl|tg|ce)\\d")
