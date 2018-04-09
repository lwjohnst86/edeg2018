prep_gee_data <- function(outcome) {
    .pls_results <- project_data %>%
        pls_model(outcome = outcome, ncomp = 2) %>%
        scores_as_df() %>%
        select_at(vars(-matches("pct_"))) %>%
        mutate(SID = project_data %>%
                   select_at(c("SID", outcome)) %>%
                   na.omit() %>%
                   .[["SID"]])

    full_join(over_time_data, .pls_results) %>%
        fill(Comp1, Comp2)
}

analyze_gee_pls <- function() {

    isi <- prep_gee_data(outcome = "lISI") %>%
        design('gee') %>%
        add_settings(family = stats::gaussian(),
                            corstr = 'ar1', cluster.id = 'SID') %>%
        add_variables('yvars', "lISI") %>%
        add_variables('xvars', c("Comp1", "Comp2")) %>%
        construct() %>%
        scrub() %>%
        polish_filter("Term", 'term') %>%
        padjust_model_output() %>%
        mutate_at(vars(estimate, conf.low, conf.high), funs((exp(.) - 1) * 100))

    issi2 <- prep_gee_data(outcome = "lISSI2") %>%
        design('gee') %>%
        add_settings(family = stats::gaussian(),
                            corstr = 'ar1', cluster.id = 'SID') %>%
        add_variables('yvars', "lISSI2") %>%
        add_variables('xvars', c("Comp1", "Comp2")) %>%
        add_variables("covariates", "YearsFromBaseline") %>%
        construct() %>%
        scrub() %>%
        polish_filter("Term", 'term') %>%
        padjust_model_output() %>%
        mutate_at(vars(estimate, conf.low, conf.high), funs((exp(.) - 1) * 100))

    bind_rows(isi, issi2)
}

padjust_model_output <- function(gee_results) {
    gee_results %>%
        polish_adjust_pvalue(method = 'BH') %>%
        rename(unadj.p.value = p.value, p.value = adj.p.value)
}
