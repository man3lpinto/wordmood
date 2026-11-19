#' wordmood: Simple Word-Based Sentiment Scoring
#'
#' The package scores a text document by counting positive and negative words, where every word carries a confidence weight. It is built from four layers that are meant to be used together:
#'
#' 1. [preprocess_text()] reads a text file (or a character string), turns it into lower case, removes punctuation and returns a vector of words.
#' 2. [count_matches()] compares that word vector with a list of labelled words (positive or negative). Patterns may contain a `*` wildcard as prefix and/or suffix, and may carry a confidence weight between 0 and 1. It returns the number of matching words or their summed weight.
#' 3. [is_negated()] flags words that have a negating word ("not", "never", "don't", ...) within the two words before them, as in "not very happy".
#' 4. [sentiment_score()] is the user-facing function. It runs the steps above for the positive and the negative word list, flips negated words to the other side, and summarises the result as counts, weighted scores, a ratio, a balance and a label.
#'
#' Default weighted word lists are available through [default_positive_words()] and [default_negative_words()], and the negating words through [default_negators()]. Their weights are our own judgement, not evidence based; supply your own lists for real analyses.
#'
#' @section Requirements: Base R only (the `utils` package is used for `read.delim()`).
#'
#' @examples
#' txt <- "I felt happy today, but also a bit nauseous and not very relaxed."
#' sentiment_score(text = txt)
#'
#' @keywords internal
"_PACKAGE"
