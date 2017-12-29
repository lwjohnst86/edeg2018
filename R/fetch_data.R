fetch_data <- function() {
    project_data <-
        left_join(
            PROMISE.data::fattyacids %>%
                select(SID, matches("^(ne|tg|pl|ce)")),
            PROMISE.data::PROMISE %>%
                filter(VN %in% c(0, 1)) %>%
                arrange(SID, VN) %>%
                PROMISE.scrub::scr_duplicates(c("SID"), action = "keepfirst") %>%
                select(SID, HOMA2_S, ISI, ISSI2, IGIIR, TAG, HDL, Chol, Glucose0,
                       ALT, CRP, MAP = MeanArtPressure, Waist, DM),
            by = "SID"
        ) %>%
        left_join(
            PROMISE.data::dhq %>%
                filter(VN == 3) %>%
                select(SID, diet_added_sugar, diet_discret_fat),
            by = "SID"
        ) %>%
        filter(DM != 1 | is.na(DM)) %>%
        mutate(
            TotalFA = rowSums(select(., matches("^(ne|tg|pl|ce)"))),
            invHDL = 1 / HDL
        )

    project_data <- project_data %>%
        select_at(vars(matches("^(ne|tg|pl|ce)"))) %>%
        rename_all(funs(paste0("pct_", .))) %>%
        mutate_all(funs((. / project_data$TotalFA) * 100)) %>%
        bind_cols(project_data)

    project_data <- project_data %>%
        select_at(vars(TAG, ALT, CRP, ISI, ISSI2, IGIIR)) %>%
        rename_all(funs(paste0("l", .))) %>%
        mutate_all(log) %>%
        bind_cols(project_data)

    # Save the dataset to the data/ folder.
    devtools::use_data(project_data, overwrite = TRUE)
}
