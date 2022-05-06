## UI info message
.info_msg <- function() {
    HTML(
        paste(
            h4("Use the left panel to"),
            "<ul>
                <li>Indicate if the scores should be log-transformed;</li>
                <li>Select a <i>Decoy</i> variable</li>
                <li>Select a <i>Score</i> variable</li>
                <li>Select the number of <i>Bins</i> for the histogram </li>
            </ul>",
            h4("The following conditions must be met"),
            "<ul>
                <li> Decoy variable is a boolean that indicates if the score belongs to a target or a decoy</li>
                <li> Score variable contains the scores of the search engine, which have to be continuous (larger scores are assumed to be better. E-values are typically -log10(e-value) transformed.)</li>
                <li> You can verify the selected data and settings using the table below, and, the plots in the histogram and PP-Plot tabs.</li>
            </ul>",
            "Press button <b>Stop app</b> to return to R."
        )
    )
}


## Helper to find variables from a specific type
.find_vars <- function(data, filter) {
    names(data)[vapply(data, filter, logical(1))]
}
