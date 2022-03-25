#' Find the pallette of your dreams
#'
#' @param min_colours Minimum number of colours that the pallette must support
#' @param colourblind_friendly Does the pallette need to be colourblind friendly
#' @param type What type? Allowed values are "categorical", "diverging_discrete" or "sequential_discrete"
#'
#' @return A tibble of metadata describing the pallettes found
#' @export
#'
#' @examples
#' find_pallette()
#' find_pallette(min_colours=12)
#' find_pallette(type="categorical")
find_pallette <- function(min_colours=0, colourblind_friendly=FALSE, type=NULL) {

  # Kludge to stop check() complaining about column names
  NULL -> Type
  NULL -> Colours
  NULL -> Friendly

  assertthat::assert_that(min_colours==0 | assertthat::is.count(min_colours), msg="Min colours must be a single value of 0 or more")
  assertthat::assert_that(assertthat::is.flag(colourblind_friendly))

  if (! is.null(type)) {
    if (! type %in% friendlypallettes::all_metadata$Type) {
      paste(dplyr::distinct(friendlypallettes::all_metadata,Type)%>%dplyr::pull(Type), collapse = " ") -> valid_values
      base::stop(
        base::paste(
          "Invalid type",
          type,
          "valid values are",
          valid_values
        )
      )
    }
  }

  friendlypallettes::all_metadata -> pallette_answer

  if (min_colours > 0) {
    pallette_answer %>%
      dplyr::filter(Colours >= min_colours) -> pallette_answer
  }

  if (colourblind_friendly) {
    pallette_answer %>%
      dplyr::filter(Friendly) -> pallette_answer
  }

  if (!is.null(type)) {
    pallette_answer %>%
      dplyr::filter(Type == type) -> pallette_answer
  }

  pallette_answer %>% return()
}
