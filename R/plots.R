
plot_loadings <- function(.data) {
    .data %>%
        append_large_loadings() %>%
        # ggplot(aes(x = loadings, y = components, colour = Fraction)) +
        ggplot(aes(x = Loadings, y = Components)) +
        geom_point(aes(alpha = LargeLoadings)) +
        ggrepel::geom_text_repel(
            aes(label = FA),
            size = 2,
            box.padding = 0.4,
            segment.alpha = 0.3,
            colour = "black"
        ) +
        # viridis::scale_color_viridis(discrete = TRUE) +
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
    ggplot(data = .data, aes(x = C1, y = C2, colour = as.factor(LargeLoadings))) +
        # ggplot(aes(x = C1, y = C2)) +
        geom_corr_circle()
    # +
    #     scale_alpha_discrete(guide = "none") +
    #     # geom_point(data = fit, aes(colour = Fraction)) +
    #     # viridis::scale_color_viridis(discrete = TRUE) +
    #     # scale_color_brewer(palette = "Set1") +
    #     ggrepel::geom_text_repel(
    #         aes(label = LargeLoadingsXvar),
    #         size = 2,
    #         box.padding = 0.4,
    #         segment.alpha = 0.3
    #     ) +
    #     labs(
    #         x = paste0('C1 (', round(expl_var[1], 1), '% explained variance)'),
    #         y = paste0('C2 (', round(expl_var[2], 1), '% explained variance)'),
    #         title = title
    #     ) +
    #     theme_classic()
}


