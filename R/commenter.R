#' Comment Selection
#'
#' Call this function as an RStudio addin to comment/uncomment selection
#'
#' @importFrom rstudioapi getActiveDocumentContext primary_selection insertText
#' @export
commenter <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  selection <- rstudioapi::primary_selection(context)
  re1 <- "^(\\s*)(\\/\\* ?)(.+?)( ?\\*\\/)(\\s*)$"
  if (grepl(re1, selection$text)) {
    text <- sub(re1, "\\1\\3\\5", selection$text)
  } else {
    text <- sub("(.+?)(\\s*\\r?\\n?\\s*)$", "/* \\1 */\\2", selection$text)
  }
  rstudioapi::insertText(
    location = selection$range,
    text = text)
}
