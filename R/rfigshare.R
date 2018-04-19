
to_figshare <- function() {
    rfigshare::fs_new_article(
        "Specific clusters of fatty acids (FA) across multiple serum lipid fractions underlie pathophysiological features of type 2 diabetes in the Canadian PROMISE cohort",
        "Poster presented at the European Diabetes Epidemiology Group Meeting in Elsinore, Denmark from April 21-24, 2018.",
        type = "poster",
        tags = c(
            "type 2 diabetes",
            "fatty acids",
            "lipid fractions",
            "cohort",
            "longitudinal",
            "diabetes pathogenesis"
        ),
        categories = c("Diseases", "Pathogenesis", "Epidemiology"),
        links = "https://github.com/lwjohnst86/edeg2018",
        files = "doc/poster.pdf",
        visibility = "draft"
    )
}
