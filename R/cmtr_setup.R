#' Set cmtr Comment Character(s)
#'
#' Indicate which symbols to use as comment delimiters.
#'
#' @param chars Character vector of length 1 or 2, indicating what symbol or
#'   combination of symbols to use as opening, and optionaly closing comment
#'   delimiter.
#' @param block Logical. Indicates whether to apply or remove the
#'   comment delimiters on (multiline) blocks (\code{TRUE}), or on a
#'   line-by-line basis (\code{FALSE}).
#' 
#' @export
cmtr_setup <- function(chars=c("/*", "*/"),
                       block = TRUE) {
  if (length(names(match.call())) == 0) { 
    getOption("cmtr")
  } else {  
    options(cmtr = list(chars = chars,
                        block = block))
  }
}
