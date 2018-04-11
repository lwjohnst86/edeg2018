# Set options for all documents and scripts.
#
set_options <- function() {
    # Set the options here for individual packages

    # For tables (uncomment below if using pander)
#     pander::panderOptions('table.split.table', Inf)
#     pander::panderOptions('table.style', 'rmarkdown')
#     pander::panderOptions('table.alignment.default',
#                   function(df)
#                       ifelse(sapply(df, is.numeric), 'center', 'left'))

    # For the document (knitr)
    knitr::opts_chunk$set(
        warning = FALSE, message = FALSE, collapse = TRUE
    )
}

