#' Score the mood of a text using weighted words and negation
#'
#' Runs [preprocess_text()] on the input, finds negated words with [is_negated()] and then uses [count_matches()] with the positive and the negative word list.
#'
#' **Weights.** Every word carries a confidence weight (see [default_positive_words()]), so a word we are unsure about (for example "fine", weight 0.25) moves the result less than a clear one (for example "happy", weight 1).
#'
#' **Negation.** A sentiment word with a negating word ("not", "never", "don't", ...) within the `window` words before it is flipped:
#'
#'     A negated **positive** word ("not happy", "not very happy") counts as **negative**, with its weight multiplied by `negated_positive` (default 1, i.e. as strong as a negative word).
#'     A negated **negative** word ("not bad", "no pain") counts as **positive**, with its weight multiplied by `negated_negative` (default 0.5, because "not bad" is weaker than "good").
#'
#' The ratio, balance and label are based on the final weighted scores.
#'
#' @param file Path to a text file (see [preprocess_text()]). Ignored if `text` is given.
#' @param text Optional character vector with text, used instead of `file`.
#' @param positive Positive patterns: a character vector (all weight 1) or a named numeric vector of confidence weights (see [count_matches()]). Defaults to [default_positive_words()].
#' @param negative Negative patterns, in the same format as `positive`. Defaults to [default_negative_words()].
#' @param negators Negating words, see [is_negated()]. Defaults to [default_negators()].
#' @param window Number of words before a sentiment word that are checked for a negator. Defaults to `2`. Use `0` to switch negation off.
#' @param negated_positive Number between 0 and 1. How strongly a negated positive word counts as negative. Defaults to `1`.
#' @param negated_negative Number between 0 and 1. How strongly a negated negative word counts as positive. Defaults to `0.5`. Use `0` to simply ignore negated negative words.
#' @param neutral_band A number between 0 and 1. If the absolute balance is at most this value, the text is labelled "neutral". Defaults to `0.1`, so a near tie (such as 50 against 49) is not called positive or negative.
#' @param ... Further arguments passed on to [preprocess_text()], for example `stopwords` or `header`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{n_words}{Total number of words in the text.}
#'   \item{n_positive}{Number of words counted as positive (including negated negative words).}
#'   \item{n_negative}{Number of words counted as negative (including negated positive words).}
#'   \item{n_negated}{Number of sentiment words that were flipped because of a negation.}
#'   \item{score_positive}{Weighted positive score.}
#'   \item{score_negative}{Weighted negative score.}
#'   \item{ratio}{`score_positive / score_negative`; `NA` if the negative score is 0.}
#'   \item{balance}{`(score_positive - score_negative) / (score_positive + score_negative)`, between -1 (only negative) and 1 (only positive); `NA` if no words matched.}
#'   \item{label}{"positive", "negative" or "neutral".}
#' }
#'
#' @examples
#' sentiment_score(text = "I am happy and I love this wonderful day.")
#'
#' # Negation: "not very happy" counts as negative
#' sentiment_score(text = "I am not very happy today.")
#'
#' # "not bad" counts as weakly positive
#' sentiment_score(text = "The food was not bad.")
#'
#' # Switch negation off
#' sentiment_score(text = "I am not very happy today.", window = 0)
#'
#' # Custom weighted word lists with wildcards
#' sentiment_score(
#'   text = "Felt nauseous, then nauseated, but the dinner was fine.",
#'   positive = c("great" = 1, "fine" = 0.25),
#'   negative = c("nause*" = 1)
#' )
#'
#' # From a file
#' f <- system.file("extdata", "diary.txt", package = "wordmood")
#' sentiment_score(file = f)
#'
#' @export
sentiment_score <- function(file = NULL,
                            text = NULL,
                            positive = default_positive_words(),
                            negative = default_negative_words(),
                            negators = default_negators(),
                            window = 2,
                            negated_positive = 1,
                            negated_negative = 0.5,
                            neutral_band = 0.1,
                            ...) {
  for (arg in c("negated_positive", "negated_negative", "neutral_band")) { #loop that checks each parameter individually
    x <- get(arg)
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0 || x > 1) { #debug: is the parameter valid
      stop("`", arg, "` must be a single number between 0 and 1.",
           call. = FALSE)
    }
  }

  words <- preprocess_text(file = file, text = text, ...) #use preprocess_text function defined previously
  negated <- is_negated(words, negators = negators, window = window) #use is_negated function defined previously

  plain <- words[!negated]    # words without a negation before them
  flipped <- words[negated]   # words with a negation before them

  # Words that keep their meaning #use count_matches function defined previously
  n_pos <- count_matches(plain, positive)
  n_neg <- count_matches(plain, negative)
  s_pos <- count_matches(plain, positive, weighted = TRUE)
  s_neg <- count_matches(plain, negative, weighted = TRUE)

  # Negated words switch sides
  n_pos_flip <- count_matches(flipped, positive)   # "not happy" -> negative
  n_neg_flip <- count_matches(flipped, negative)   # "not bad"   -> positive
  s_pos_flip <- count_matches(flipped, positive, weighted = TRUE)
  s_neg_flip <- count_matches(flipped, negative, weighted = TRUE)

  score_pos <- s_pos + negated_negative * s_neg_flip
  score_neg <- s_neg + negated_positive * s_pos_flip
  if (negated_negative > 0) n_pos <- n_pos + n_neg_flip
  if (negated_positive > 0) n_neg <- n_neg + n_pos_flip

  ratio <- if (score_neg == 0) NA_real_ else score_pos / score_neg
  total <- score_pos + score_neg
  balance <- if (total == 0) NA_real_ else (score_pos - score_neg) / total

  label <- if (is.na(balance) || abs(balance) <= neutral_band) {
               "neutral"
  }        else if (balance > 0) {
               "positive"
  }        else {
               "negative"
  }

  list(
    n_words        = length(words),
    n_positive     = n_pos,
    n_negative     = n_neg,
    n_negated      = n_pos_flip + n_neg_flip,
    score_positive = score_pos,
    score_negative = score_neg,
    ratio          = ratio,
    balance        = balance,
    label          = label
  )
}
