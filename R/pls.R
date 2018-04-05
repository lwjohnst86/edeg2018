
# Analyze PLS -------------------------------------------------------------

analyze_pls <- function(.data, .yvar, .xvar, .ncomp = 4, .cv = FALSE) {
    .data %>%
        design("pls") %>%
        add_settings(ncomp = .ncomp,
                     validation = "CV",
                     cv.data = .cv) %>%
        add_variables("xvar", .xvar) %>%
        add_variables("yvar", .yvar) %>%
        construct() %>%
        scrub()
}


# Wrangling PLS models ----------------------------------------------------

pls_corr_as_df <- function(.model) {
    expl_var_50 <- sqrt(1 / 2) - 0.07
    model <- cor(model.matrix(.model), pls::scores(.model)) %>%
        as_tibble(rownames = "xvariables") %>%
        tibble::set_tidy_names(syntactic = TRUE) %>%
        rename_at(vars(contains("Comp.")), funs(stringr::str_replace(., "Comp\\.", "C"))) %>%
        mutate(
            xvariables = PROMISE.misc::renaming_fa(xvariables) %>%
                stringr::str_replace("pct_", ""),
            Fraction = extract_fraction(xvariables),
            LargeLoadings = if_else(calc_radius(C1, C2) >= expl_var_50, TRUE, FALSE),
            LargeLoadingsXvar = if_else(LargeLoadings, xvariables, NA_character_),
            Outcome = dimnames(.model$model$Y)[[2]]
        )

    attr(model, "explvar") <- pls::explvar(.model)
    model
}

pls_loadings_as_df <- function(.model) {
    model <- unclass(pls::loadings(.model)) %>%
        as_tibble(rownames = "xvariables") %>%
        tibble::set_tidy_names(syntactic = TRUE) %>%
        rename_at(vars(contains("Comp.")), funs(stringr::str_replace(., "Comp\\.", "C"))) %>%
        mutate(
            xvariables = PROMISE.misc::renaming_fa(xvariables) %>%
                stringr::str_replace("pct_", ""),
            Fraction = extract_fraction(xvariables),
            TotalExplVar = pls::explvar(.model) %>%
                sum() %>%
                round(1) %>%
                as.character(),
            Outcome = dimnames(.model$model$Y)[[2]]
        ) %>%
        purrr::modify(~ {attributes(.x)$explvar <- NULL; .x})

    attr(model, "explvar") <- pls::explvar(.model)
    model
}

scores_as_df <- function(model) {
    .scores <- pls::scores(model)
    attr(.scores, "explvar") <- NULL
    .scores <- .scores %>%
        unclass() %>%
        as.matrix() %>%
        as_tibble() %>%
        rename_all(funs(stringr::str_replace(., " ", "")))

    .data <- bind_cols(
        model$model$X %>%
            as_tibble(),
        model$model$Y %>%
            as_tibble()
        )

    bind_cols(.data, .scores)
}

# Utils -------------------------------------------------------------------

append_large_loadings <- function(.loadings) {
    .loadings %>%
        mutate(xvariables = renaming_fats(xvariables)) %>%
        gather(Components, Loadings, matches("^C\\d$")) %>%
        mutate(Loadings = as.numeric(Loadings),
               Components = forcats::fct_rev(Components)) %>%
        group_by(Components) %>%
        mutate(
            MaxLoading = max(Loadings),
            MinLoading = min(Loadings),
            LargeLoadings = !between(Loadings, -0.2, 0.2),
            FA = if_else(LargeLoadings, xvariables, NA_character_)
        ) %>%
        ungroup()
}

calc_radius <- function(x, y) {
    sqrt(x ^ 2 + y ^ 2)
}

extract_fraction <- function(x) {
    stringr::str_sub(x, 1L, 2L)
}

large_contributors <- function(.model) {
    expl_var_50 <- sqrt(1 / 2)
    .model %>%
        model.matrix() %>%
        cor(pls::scores(.model)[, 1:2, drop = FALSE]) %>%
        as_tibble(rownames = "fattyacid") %>%
        rename_all(funs(stringr::str_replace(., " ", ""))) %>%
        mutate(fattyacid = PROMISE.misc::renaming_fa(fattyacid) %>%
                   stringr::str_replace("pct_", "")) %>%
        filter(calc_radius(Comp1, Comp2) >= expl_var_50)
}
