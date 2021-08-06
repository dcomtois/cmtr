#' Set Multiline Option On or Off
#'
#' Toggle the multiline behavior or set it explicitly to TRUE or FALSE.
#'
#' @param value Logical. Should comments span over more than one line?
#' @export
block <- function(value=NULL) {
  op <- getOption("cmtr")
  if (is.null(value)) {
    options(cmtr = list(chars = op$chars,
                        block = !op$block))
    message("block commenting switched ", switch(as.character(!op$block),
                                           "TRUE"="On",
                                           "Off"))
  } else {
    options(cmtr = list(chars = op$chars,
                        block = as.logical(value)))
  } 
} 
