#' Find words that are negated by a word just before them
#'
#' Checks, for every word, whether one of the previous `window` words is a negating word such as "not", "never" or "don't". With the default `window = 2` this would catch both "not happy" and "not very happy" scenarios.
#'
#' [sentiment_score()] uses this to turn negated positive words into negative ones ("not happy") and negated negative words into weakly positive ones ("not bad").
#'
#' Caveat: Punctuation is removed by [preprocess_text()], so the window can reach across the end of a sentence: in "I could not sleep. Happy now" the word "happy" counts as negated.
#'
#' @param words Character vector of words, typically from [preprocess_text()].
#' @param negators Character vector of negating words or patterns. The `*` wildcard works as in [count_matches()]. Defaults to [default_negators()].
#' @param window Number of words before each word to look at. Use `0` to switch negation off.
#'
#' @return A logical vector, the same length as `words`. `TRUE` means that the word has a negating word within the `window` words before it.
#'
#' @examples
#' w <- c("i", "am", "not", "very", "happy", "today")
#' is_negated(w)
#' w[is_negated(w)]
#'
#' # Only look at the single word before
#' is_negated(w, window = 1)
#'
#' @export
is_negated <- function(words, negators = default_negators(), window = 2) {
  words <- as.character(words)
  n <- length(words)
  negated <- rep(FALSE, n) #creates initial vector of FALSE, for n words

  if (window < 1 || length(negators) == 0 || n < 2) { #if vector does not need the function, return the vector
    return(negated)
  }

  # Which words are themselves negators? One call checks all words at once.
  is_negator <- count_matches(words, negators, per_word = TRUE) > 0

  # Shift the negator positions forward by 1, 2, ..., `window` words.
  # A word is negated if a negator sits `step` words before it.
  for (step in seq_len(min(window, n - 1))) { #determines the number of iterations in the loop
    later <- (step + 1):n           # indices of the words that have at least `step` preceding positions available
    negated[later] <- negated[later] | is_negator[later - step] #Update the negated vector
  }

  negated
}
