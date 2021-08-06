#' Set chars Option
#'
#' Define the opening and closing character(s) for commenting
#'
#' @param value 
#' @export
chars <- function(...) {
  op <- getOption("cmtr")
  mc <- as.character(match.call())
  if (length(mc) == 1) {
    message("Current char(s): ", paste(op$chars, collapse = " ... "))
  } else {
    if (length(mc) > 3) {
      warning("only the first two strings were registered")
      length(mc) <- 3
    }  
    options(cmtr = list(chars = mc[2:length(mc)],
                        block = op$block))
  } 
}
