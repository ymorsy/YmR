#' Process Venn Data
#'
#' @param filepath Path to the data file
#' @return A tibble with processed Venn data
#' @importFrom rio import
#' @importFrom tibble as_tibble
#' @importFrom dplyr rename mutate
#' @importFrom tidyr fill
#' @importFrom stringr str_replace_all
#' @export
#'
process_venn_data <- function(filepath) {
    df_venn_hm <- import(filepath, na.strings = "") %>%
        as_tibble() %>%
        rename(groups = total, names = Names) %>%
        fill(names, groups) %>%
        mutate(names = paste(names, groups, sep = "_") %>%
            str_replace_all(" ", "_"))

    return(df_venn_hm)
}
library(tidyverse)
