
generate_corr <- function() {
    corr_conc <- project_data %>%
        analyze_corr(.xvar = outcomes, .yvar = fa_conc)
    corr_pct <- project_data %>%
        analyze_corr(.xvar = outcomes, .yvar = fa_pct)
    devtools::use_data(corr_conc, corr_pct, overwrite = TRUE)
}

generate_pls <- function() {

    # Correlation data between C1 and C2 and actual values.
    pls_corr_pct <- list(
        lISI = project_data %>%
            analyze_pls(.yvar = "lISI", .xvar = fa_pct) %>%
            pls_corr_as_df(),
        lISSI2 = project_data %>%
            analyze_pls(.yvar = "lISSI2", .xvar = fa_pct) %>%
            pls_corr_as_df()
    )

    pls_corr_conc <- list(
        lISI = project_data %>%
            analyze_pls(.yvar = "lISI", .xvar = fa_conc) %>%
            pls_corr_as_df(),
        lISSI2 = project_data %>%
            analyze_pls(.yvar = "lISSI2", .xvar = fa_conc) %>%
            pls_corr_as_df()
    )

    devtools::use_data(pls_corr_pct, pls_corr_conc, overwrite = TRUE)

    # Loadings data for all components.
    pls_loadings_pct <- list(
        lISI = project_data %>%
            analyze_pls(.yvar = "lISI", .xvar = fa_pct) %>%
            pls_loadings_as_df(),
        lISSI2 = project_data %>%
            analyze_pls(.yvar = "lISSI2", .xvar = fa_pct) %>%
            pls_loadings_as_df()
    )

    pls_loadings_conc <- list(
        lISI = project_data %>%
            analyze_pls(.yvar = "lISI", .xvar = fa_conc) %>%
            pls_loadings_as_df(),
        lISSI2 = project_data %>%
            analyze_pls(.yvar = "lISSI2", .xvar = fa_conc) %>%
            pls_loadings_as_df()
    )

    devtools::use_data(pls_loadings_pct, pls_loadings_conc, overwrite = TRUE)

    # TODO: Add data with CV results, for prediction vs actual

}
