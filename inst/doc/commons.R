## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

documentation_marker <- function(
  tag,
  alt = commons:::provenance_display[[tag]]$label
) {
  icon <- c(
    A = "trusted-icon.svg",
    B = "citation-mark.svg",
    C = "warning-icon.svg"
  )[[tag]]

  htmltools::tags$img(
    class = "commons-documentation-marker",
    src = knitr::image_uri(
      system.file("www", "commons-chat", "figs", icon, package = "commons")
    ),
    alt = alt
  )
}

## ----trust-flow---------------------------------------------------------------
knitr::include_graphics("../man/figures/trust-flow.svg")

