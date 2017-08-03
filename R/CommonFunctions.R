##' Helper functions

#' The function compares strings ignoring case
#' @export
isequal_strings_ignorecase <- function (first, second){
  if (!is.null(first) & !is.null(second))
  {
    return (toupper(first) == toupper(second))
  }
  else
  {
    return (FALSE)
  }
}

frequency_to_int <- function(freq) {

  if (is.null(freq) || is.na(freq)) {
    return(365)
  } else {
    switch(freq,
           'D'    = 365,
           'W'   = 52,
           'M'  = 12,
           'Q' = 4,
           'H' = 2,
           'Y'   = 1,
           1)
  }
}
