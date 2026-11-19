#' Count or score the words that match a list of patterns
#'
#' Compares a word vector (from [preprocess_text()]) with a list of patterns, for example all positive words. Each pattern can carry a **confidence weight** between 0 and 1: how sure we are that a word matching the pattern really has that sentiment.
#'
#' A `*` in a pattern is a wildcard:
#'
#' `"nause*"` matches words that **start with** "nause" (nauseous, nauseated)
#' `"*ache"` matches words that **end with** "ache" (headache, stomachache)
#' `"*as*"` matches words that **contain** "as"
#' `"happy"` (no `*`) matches only the exact word
#'
#' Each word is counted at most once. If a word matches several patterns, the **highest** weight is used. This lets a broad, less certain pattern such as `"pain*" = 0.75` (which also matches "painting") work together with a specific, certain one such as `"painful" = 1`.
#'
#' @param words Character vector of words, typically from [preprocess_text()].
#' @param patterns Either
#'   * a character vector of words or patterns, where every pattern gets weight 1, or
#'   * a named numeric vector, where the names are the patterns and the values are confidence weights between 0 and 1, for example `c("happy" = 1, "fine" = 0.25)`.
#'   Patterns are converted to lower case.
#' @param weighted Logical. If `FALSE` (default), return the number of words that match at least one pattern. If `TRUE`, return the sum of the weights of those words.
#' @param per_word Logical. If `TRUE`, return one value per word instead of a total: the weight of the best matching pattern, or 0 if the word does not match. `weighted` is then ignored. Used by [is_negated()] to check all words in one call. #to reduce complexity (BigO)
#'
#' @return A single number: the number of matching words, or their summed weight if `weighted = TRUE`. With `per_word = TRUE`, a numeric vector the same length as `words`.
#'
#' @examples
#' w <- c("i", "felt", "nauseous", "and", "nauseated", "with", "a", "headache")
#' count_matches(w, c("nause*"))          # =2
#' count_matches(w, c("*ache"))           # =1
#' count_matches(w, c("nause*", "*ache")) # =3
#'
#' # With confidence weights
#' pats <- c("nause*" = 1, "*ache" = 0.75)
#' count_matches(w, pats)                  # =3 words
#' count_matches(w, pats, weighted = TRUE) # =1 + 1 + 0.75 = 2.75 weight
#'
#' # One weight per word
#' count_matches(w, pats, per_word = TRUE)
#'
#' @export
count_matches <- function(words, patterns, weighted = FALSE, per_word = FALSE) {
  words <- as.character(words)

  if (is.numeric(patterns)) {
    if (is.null(names(patterns))) {
      stop("A numeric `patterns` vector needs names (the patterns).", #debug no pattern names
           call. = FALSE)
    }
    weights <- unname(patterns) #weighted patterns - unname patterns - keep weights
    patterns <- names(patterns) #unweight patterns - keep names
  } else {
    patterns <- as.character(patterns) #unweighted patterns (every pattern=1)
    weights <- rep(1, length(patterns))
  }

  if (any(is.na(weights) | weights < 0 | weights > 1)) { #validate that weights are between 0-1
    stop("Weights must be between 0 and 1.", call. = FALSE)
  }

  patterns <- tolower(patterns) #to lowercase
  keep <- !is.na(patterns) & patterns != "" #remove invalids
  patterns <- patterns[keep]
  weights <- weights[keep]

  # Best (highest) weight found so far for each word; 0 = no match
  best <- rep(0, length(words))

  #Loop through each pattern
  for (i in seq_along(patterns)) {
    p <- patterns[i]
    has_start <- startsWith(p, "*")   # pattern begins with wildcard -> match the ending
    has_end   <- endsWith(p, "*")     # pattern ends with wildcard   -> match the beginning
    core <- gsub("*", "", p, fixed = TRUE)

    if (core == "") next              # a pattern of only "*" is ignored

    #test for wildcards
    if (has_start && has_end) {
      hit <- grepl(core, words, fixed = TRUE)
    } else if (has_end) {
      hit <- startsWith(words, core)
    } else if (has_start) {
      hit <- endsWith(words, core)
    } else {
      hit <- words == core
    }

    #uptades score for matching word (keeps best score if vector was already scored)
    best[hit] <- pmax(best[hit], weights[i])
  }

  if (per_word) return(best)                   #returns score per word
  if (weighted) sum(best) else sum(best > 0)   # returns total score
}
