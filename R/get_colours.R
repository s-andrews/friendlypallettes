get_colours <- function(short_name, num_colours=0) {

  NULL -> pallette
  NULL -> index
  NULL -> colour

  if (! short_name %in% friendlypallettes::all_colours$pallette) {
    base::stop(base::paste("Couldn't find a pallette called",short_name))
  }

  assertthat::assert_that(assertthat::is.number(num_colours))

  assertthat::assert_that(
    friendlypallettes::all_colours %>% dplyr::filter(pallette==short_name) %>% base::nrow() >= num_colours,
    msg = "Not enough colours in this pallette"
  )


  friendlypallettes::all_colours %>%
    dplyr::filter(pallette == short_name) %>%
    dplyr::arrange(index) -> return_colours

  if (num_colours > 0) {
    return_colours %>%
      dplyr::filter(index <= num_colours) -> return_colours
  }

  return_colours %>% dplyr::pull(colour) %>% return()
}
