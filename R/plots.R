
plot_loadings <- function(.model) {
    total_expl_var <- pls::explvar(.model)[1:2] %>%
        sum() %>%
        round(2) %>%
        as.character()
    .model %>%
        loadings_as_df() %>%
        append_large_loadings() %>%
        filter(components %in% c("Comp 1", "Comp 2")) %>%
        ggplot(aes(x = loadings, y = components)) +
        geom_point(aes(alpha = large_loadings)) +
        geom_text_repel(
            aes(label = xvariables),
            size = 3,
            box.padding = 0.4,
            segment.alpha = 0.3
        ) +
        theme(legend.position = "none") +
        labs(y = paste0("PLS Components (", total_expl_var, "%)"),
             x = "PLS loadings for each fatty acid")
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
