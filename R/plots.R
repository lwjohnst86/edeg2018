
plot_loadings <- function(.data) {
    .data %>%
        append_large_loadings() %>%
        ggplot(aes(x = Loadings, y = Components)) +
        geom_point(aes(alpha = LargeLoadings, colour = Fraction)) +
        ggrepel::geom_text_repel(
            aes(label = FA),
            size = 2.5,
            box.padding = 0.4,
            segment.alpha = 0.3,
            colour = "black"
        ) +
        # viridis::scale_color_viridis(discrete = TRUE) +
        scale_color_brewer(type = "div", palette = "PuOr") +
        labs(y = paste0("Components (", unique(.data$TotalExplVar), "% total \nexplained variance)"),
             x = "Loading (coefficient) values for the component") +
        scale_alpha_discrete(guide = "none") +
        theme_classic()
}

plot_scores <- function(.model) {
    yvar <- dimnames(.model$model$Y)[[2]]
    .scores <- .model %>%
        scores_as_df()
    gather_vars <- grep("^Comp", names(.scores), value = TRUE)

    .output <- .scores %>%
        mutate_at(yvar, funs(as.factor(ntile(., 3)))) %>%
        select_at(vars(-starts_with("pct_"))) %>%
        gather_("Component", "Score", gather_vars) %>%
        mutate_at("Component", funs(stringr::str_replace(., "Comp", "Component ")))

    .output %>%
        ggplot(aes_string(x = yvar, y = "Score")) +
        geom_violin() +
        labs(y = "PLS scores for each individual and each component",
             x = "Tertiles of log(ISI)") +
        scale_x_discrete(labels = c("1st", "2nd", "3rd")) +
        facet_wrap( ~ Component)
}


plot_corr_comps <- function(.data) {
    expl_var <- attributes(.data)$explvar
    .data %>%
        ggplot(aes(x = C1, y = C2, colour = Fraction)) +
        geom_corr_circle(size = 1) +
        ggrepel::geom_text_repel(
            aes(label = LargeLoadingsXvar),
            size = 2.5,
            box.padding = 0.4,
            segment.alpha = 0.3,
            colour = "black"
        ) +
        # viridis::scale_color_viridis(discrete = TRUE) +
        scale_color_brewer(type = "div", palette = "PuOr") +
        coord_cartesian(ylim = c(-1, 1), xlim = c(-1, 1)) +
        labs(
            x = paste0('C1 (', round(expl_var[1], 1), '% explained variance)'),
            y = paste0('C2 (', round(expl_var[2], 1), '% explained variance)')
        ) +
        theme_classic()
}


