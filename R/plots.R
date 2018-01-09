
plot_loadings <- function(.model) {
    total_expl_var <- pls::explvar(.model)[1:2] %>%
        sum() %>%
        round(1) %>%
        as.character()
    .model %>%
        loadings_as_df() %>%
        append_large_loadings() %>%
        filter(components %in% c("Comp 1", "Comp 2")) %>%
        mutate(components = stringr::str_replace(components, "omp ", "")) %>%
        # ggplot(aes(x = loadings, y = components, colour = Fraction)) +
        ggplot(aes(x = loadings, y = components)) +
        geom_point(aes(alpha = large_loadings)) +
        ggrepel::geom_text_repel(
            aes(label = xvariables),
            size = 3,
            box.padding = 0.4,
            segment.alpha = 0.3,
            colour = "black"
        ) +
        # viridis::scale_color_viridis(discrete = TRUE) +
        labs(y = paste0("Components (", total_expl_var, "% total \nexplained variance)"),
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


plot_corr_comps <- function(outcome, title = NULL) {
    # xloadings <-
    .model <- pls_model(project_data, outcome = outcome, ncomp = 2)
    expl_var_50 <- sqrt(1 / 2)

    fit <- cor(model.matrix(.model), pls::scores(.model)[, 1:2, drop = FALSE]) %>%
        as_tibble(rownames = "xvariables") %>%
        setNames(c('xvariables', 'C1', 'C2')) %>%
        mutate(xvariables = PROMISE.misc::renaming_fa(xvariables) %>%
                   stringr::str_replace("pct_", "")) %>%
        mutate(Fraction = factor(stringr::str_extract(xvariables, "NE|TG|PL|CE"))) %>%
        mutate(xvariables = ifelse(calc_radius(C1, C2) >= expl_var_50, xvariables, NA)) %>%
        mutate(large_loadings = ifelse(calc_radius(C1, C2) >= expl_var_50, TRUE, FALSE))

    circle_outer <- seer:::.circle_data(1)
    circle_inner <- seer:::.circle_data(sqrt(1 / 2))

    fig <- ggplot(fit, aes_string(x = "C1", y = "C2")) +
        geom_segment(aes(
            x = -1,
            y = 0,
            xend = 1,
            yend = 0
        ), colour = 'grey90') +
        geom_segment(aes(
            x = 0,
            y = -1,
            xend = 0,
            yend = 1
        ), colour = 'grey90') +
        geom_path(data = circle_outer, aes(x = x, y = y)) +
        geom_path(data = circle_inner, aes(x = x, y = y), linetype = 'dotted') +
        geom_point(data = fit, aes(alpha = large_loadings)) +
        scale_alpha_discrete(guide = "none") +
        # geom_point(data = fit, aes(colour = Fraction)) +
        # viridis::scale_color_viridis(discrete = TRUE) +
        # scale_color_brewer(palette = "Set1") +
        ggrepel::geom_text_repel(
            data = fit,
            aes(label = xvariables),
            size = 3,
            box.padding = 0.4,
            segment.alpha = 0.3
        ) +
        labs(
            x = paste0('C1 (', round(pls::explvar(.model)[1], 1), '% explained variance)'),
            y = paste0('C2 (', round(pls::explvar(.model)[2], 1), '% explained variance)'),
            title = title
        ) +
        theme_classic()
    fig
}


