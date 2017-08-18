## Helper functions

# The function compares strings ignoring case

IsEqualStringsIgnoreCase <- function (first, second){
  if (!is.null(first) & !is.null(second))
  {
    return (toupper(first) == toupper(second))
  }
  else
  {
    return (FALSE)
  }
}

FrequencyToInt <- function(freq) {

  if (is.null(freq) || is.na(freq)) {
    return(365)
  } else {
    switch(freq,
           "D"    = 365,
           "W"   = 52,
           "M"  = 12,
           "Q" = 4,
           "H" = 2,
           "A"   = 1,
           1)
  }
}
