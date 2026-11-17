## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----calculation--------------------------------------------------------------
# #' Individuals observed by site
# #'
# #' Counts recorded during surveys, not a complete population census.
# #' @measure
# individuals_by_site <- function(survey_db) {
#   DBI::dbGetQuery(
#     survey_db,
#     "SELECT site, SUM(count) AS individuals FROM observations GROUP BY site ORDER BY site"
#   )
# }

## ----constructor--------------------------------------------------------------
# #' Create the survey commons
# #' @param observations A data frame with site and count columns.
# #' @return A commons object.
# #' @export
# biodiversity_commons <- function(observations) {
#   commons::commons(
#     data_sources = list(
#       survey_db = commons::data_source(observations = observations)
#     ),
#     semantic_layer = commons::semantic_layer(
#       system.file("measures", package = "surveys", mustWork = TRUE)
#     )
#   )
# }

## ----initialize---------------------------------------------------------------
# observations <- data.frame(
#   site = c("Oak Bluff", "Oak Bluff", "Pine Ridge"),
#   count = c(12, 8, 5)
# )
# 
# biodiversity <- surveys::biodiversity_commons(observations)
# biodiversity$measure("individuals_by_site")

## ----agent--------------------------------------------------------------------
# biodiversity <- surveys::biodiversity_commons(observations)
# agent <- commons::agent(
#   client = ellmer::chat("anthropic/claude-sonnet-5"),
#   commons = biodiversity
# )

