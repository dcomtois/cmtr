#' Comment Selection
#'
#' Call this function as an RStudio addin to comment/uncomment selection
#'
#' @importFrom rstudioapi getActiveDocumentContext primary_selection insertText
#' @export
cmt <- function() {
  
  context   <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context)
  
  chars     <- getOption("cmtr")$chars
  block     <- getOption("cmtr")$block
  
  if (isFALSE(multiline)) {
    text <- strsplit(selection$text, "\\r?\\n")[[1]]
    cat("Not yet implemented")
  } else {
    text_in <- selection$text
    if (length(chars) == 2) {
      chars_esc <- gsub("(\\W)", "\\\\\\1", chars)

      re <- paste0("^(\\s*)(",
                   chars_esc[1],
                   #"\\/\\*",
                   " ?)(.+?)( ?",
                   chars_esc[2], 
                   #"\\*\\/",
                   ")(\\s*)$")
      
      if (grepl(re, text_in)) {
        text_out <- sub(re, "\\1\\3\\5", text_in)
      } else {
        text_out <- sub("(.+?)(\\s*\\r?\\n?\\s*)$",
                        paste0(chars[1], 
                               #"/* ", 
                               " \\1 ",
                               chars[2], 
                               #"*/",
                               "\\2"),
                        text_in)
      }
    } else {
      cat("Not implement yet")    
    }
  }
  
  #re <- "^(\\s*)(\\/\\* ?)(.+?)( ?\\*\\/)(\\s*)$"
  rstudioapi::insertText(
    location = selection$range,
    text = text_out)
}
