# wordmood

An R package that scores how positive or negative a text is by counting positive and negative words. Made for the exam in the Intermediate R programming course, using the "semantic algorithm" task. It only uses base R.

## Installing

```r
devtools::install_github("man3lpinto/wordmood")
```

or, if you have downloaded the folder:

```r
devtools::install("wordmood")
```

## Example

```r
library(wordmood)

f <- system.file("extdata", "diary.txt", package = "wordmood")
sentiment_score(file = f)

sentiment_score(text = "I am not very happy today, but the food was not bad.")
```

`sentiment_score()`
returns a list with the number of words, the number of positive and negative words, a weighted positive and negative score, the ratio between them, a balance from -1 to 1 and a label ("positive", "negative" or "neutral").

## How it works

The package has four main functions that are used one after the other:

1. `preprocess_text()` reads the file with `read.delim()`, makes everything lower case, removes punctuation and splits the text into single words.
2. `is_negated()` marks words that come right after a negation like "not", "never" or "don't" (it looks two words back, so "not very happy" is caught too).
3. `count_matches()` compares the words with a list of positive or negative words and counts the matches.
4. `sentiment_score()` puts everything together and calculates the result.

The word lists are in `default_positive_words()`, `default_negative_words()` and `default_negators()`. You can also give your own lists to `sentiment_score()`.

A few things we added on top of the basic task:

- A `*` in a word list works as a wildcard. `"nause*"` matches both nauseous and nauseated, and `"*sick"` matches homesick and seasick.

- Every word has a weight between 0 and 1 for how sure we are that it is positive or negative. "wonderful" has weight 1, while a vague word like "fine" only has 0.25. Broad wildcards that can match the wrong word (e.g. `"pain*"`, which also matches "painting") get a lower weight.

- A negated positive word ("not happy") counts as negative, and a negated negative word ("not bad") counts as a bit positive (half weight).

- If the positive and negative scores are almost equal, the text is called neutral instead of positive or negative (`neutral_band = 0.1`).

## Limitations

The package looks at one word at a time, so it does not understand context.
For example "I never felt better" ends up negative, and "the proud man" is counted as positive even when it is meant badly.
Negations that are more than two words away are missed ("no amount of coffee seems to help"), and because punctuation is removed the negation check can carry over into the next sentence.
The word lists and weights are our own choices and not taken from a proper established colelction.

## Authors

Manuel Pinto
