#' Turn a text file into a vector of words
#'
#' Reads a text file with [utils::read.delim()] (or uses a character string directly), converts everything to lower case, replaces stopword characters such as `.`, `,` and `;` with spaces, and splits the text on spaces.
#'
#' Stopword characters are matched as fixed text (`fixed = TRUE`), so special regular-expression characters such as `.` or `(` are treated literally. They are replaced with a space rather than deleted, so that words joined by a dash or a slash are split apart: "die--to" (with a long dash) and "well-being" become two words each.
#'
#' @param file Path to a text file where each row is a piece of text (for example one sentence per row). Ignored if `text` is given.
#' @param text Optional character vector with text. Use this instead of `file` to analyse text that is already in R.
#' @param stopwords Character vector of characters to replace with a space before splitting into words. The defaults include the hyphen, the en dash and the em dash.
#' @param header Logical. Does the file have a header row? Passed on to [utils::read.delim()]. Defaults to `FALSE`.
#'
#' @return A character vector with one word per element, all in lower case. Empty strings are dropped.
#'
#' @examples
#' preprocess_text(text = "Hello, World; this is a TEST.")
#'
#' f <- system.file("extdata", "diary.txt", package = "wordmood")
#' head(preprocess_text(f))
#'
#' @export
preprocess_text <- function(file = NULL,
                            text = NULL,
                            stopwords = c(".", ",", ";", ":", "!", "?",
                                          "(", ")", "\"", "-", "/",
                                          "\u2013", "\u2014","\t"),
                            header = FALSE) {
  if (is.null(file) && is.null(text)) {
    stop("Provide either `file` or `text`.", call. = FALSE) #debug no text
  }

  if (is.null(text)) {
    if (!file.exists(file)) {
      stop("File not found: ", file, call. = FALSE) #debug no file
    }
    df <- utils::read.delim(file, header = header, quote = "",
                            colClasses = "character",
                            na.strings = character(0)) #reads tab delimited text file
    # Read the cells row by row so the words stay in their original order
    text <- as.vector(t(as.matrix(df))) #flattens matrix
  }


  text <- tolower(paste(as.character(text), collapse = " ")) #convert to lowercase

  for (s in stopwords) {
    text <- gsub(s, " ", text, fixed = TRUE) #removes stopwords
  }

  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  words[words != ""] #removes empty strings (stopwords)
}
