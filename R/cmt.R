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
  
  text_in   <- selection$text
  
  if (length(chars) == 1) 
    chars[2] <- "" 
  
  # Escape control characters in commenting chars
  chars_esc <- gsub("(\\W)", "\\\\\\1", chars)
  # exception for < and >
  chars_esc <- gsub("(\\\\(<|>))", "\\2", chars_esc) 

  # regex used to detect existing comments  
  re <- paste0("^(\\s*)(",
               chars_esc[1],
               " ?)(.+?)( ?",
               chars_esc[2], 
               ")(\\s*)$")
  
  if (isTRUE(block) || !grepl("\\n", text_in)) {
    
    if (grepl(re, text_in)) {
      text_out <- sub(re, "\\1\\3\\5", text_in)
    } else {
      text_out <- sub("(.+?)(\\s*\\r?\\n?\\s*)$",
                      paste0(chars[1], 
                             " \\1 ",
                             chars[2], 
                             "\\2"),
                      text_in)
    }
    
  } else {
    # Block-commenting is off -- do line-by-line matching
    text <- strsplit(selection$text, "\\r?\\n")[[1]]

    # Determine line feed type
    if (grepl("\\r\\n", text_in))
      lf <- "\r\n"
    else
      lf <- "\n"
    
    # Check if ALL lines are already commented
    if (sum(grepl(re, text)) == length(text)) {
      text_out <- paste(sub(re, "\\1\\3\\5", text), collapse = lf)
    } else {
      text_out <- paste(sub("(.+?)(\\s*\\r?\\n?\\s*)$",
                            paste0(chars[1], 
                                   " \\1 ",
                                   chars[2], 
                                   "\\2"),
                            text),
                        collapse = lf)
    }
  }   

  rstudioapi::insertText(
    location = selection$range,
    text = text_out)
}
