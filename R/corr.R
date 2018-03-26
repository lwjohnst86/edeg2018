# Functions to run the correlation analysis

# Analyze -----------------------------------------------------------------

#' Correlation analysis.
#'
#' @param .data Project data.
#' @param .xvar X variable (i.e. to put on X-axis)
#' @param .yvar Y variable (i.e. to put on Y-axis)
#'
analyze_corr <- function(.data, .yvar, .xvar) {
    .data %>%
        design('cor') %>%
        add_settings(method = 'pearson', use = 'complete.obs') %>%
        add_variables('yvars', .yvar) %>%
        add_variables('xvars', .xvar) %>%
        construct() %>%
        scrub() %>%
        mutate_at("Vars1", funs(gsub('l(ISI|ISSI2)', 'log(\\1)', .))) %>%
        mutate(order = as.numeric(stringr::str_sub(Vars2, -1L)),
               order = if_else(order == 0, 10, order)) %>%
        arrange(desc(order)) %>%
        mutate_at(vars(Vars2, Vars1), forcats::as_factor) %>%
        mutate(
            Correlations = round(Correlations, 2),
            Fraction = stringr::str_to_upper(stringr::str_extract(Vars2, "(ne|pl|tg|ce)")),
            FA = renaming_fats(Vars2, keep.fraction = FALSE),
            FA = forcats::as_factor(FA)
        )
}

# Plotting ----------------------------------------------------------------

#' Correlation heatmap plot.
#'
#' @param .results Correlation results
#'
plot_heatmap <- function(.results, text = TRUE) {
    seer::view_heatmap(
            data = .results,
            y = 'FA',
            x = 'Vars1',
            number.colours = 5,
            values.text = text,
            values.size = 4) +
        theme_minimal()
}
