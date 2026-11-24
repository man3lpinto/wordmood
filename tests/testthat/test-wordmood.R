test_that("preprocess_text lowercases, strips stopwords and splits", {
  expect_equal(preprocess_text(text = "Hello, World; TEST."),
               c("hello", "world", "test"))
})

test_that("count_matches handles exact, prefix, suffix and contains", {
  w <- c("nauseous", "nauseated", "headache", "happy", "cheese")
  expect_equal(count_matches(w, "nause*"), 2)
  expect_equal(count_matches(w, "*ache"), 1)
  expect_equal(count_matches(w, "*eas*"), 0)
  expect_equal(count_matches(w, "*hee*"), 1)
  expect_equal(count_matches(w, "happy"), 1)
  expect_equal(count_matches(w, c("nause*", "nauseous")), 2) # no double count
})

test_that("sentiment_score returns the expected structure", {
  r <- sentiment_score(text = "happy happy sad", positive = "happy",
                       negative = "sad")
  expect_equal(r$n_positive, 2)
  expect_equal(r$n_negative, 1)
  expect_equal(r$label, "positive")
  expect_equal(r$ratio, 2)
})

test_that("zero negatives gives NA ratio, no match gives neutral", {
  r <- sentiment_score(text = "nothing here", positive = "x", negative = "y")
  expect_true(is.na(r$ratio))
  expect_true(is.na(r$balance))
  expect_equal(r$label, "neutral")
})

test_that("missing input errors clearly", {
  expect_error(preprocess_text(), "Provide either")
  expect_error(preprocess_text(file = "nope.txt"), "File not found")
})

test_that("count_matches uses confidence weights", {
  w <- c("painful", "painting", "happy")
  pats <- c("pain*" = 0.75, "painful" = 1)
  expect_equal(count_matches(w, pats), 2)
  # highest weight wins: painful = 1, painting = 0.75
  expect_equal(count_matches(w, pats, weighted = TRUE), 1.75)
})

test_that("negated positive words count as negative", {
  for (txt in c("I am not happy", "I am not very happy")) {
    r <- sentiment_score(text = txt, positive = "happy", negative = "sad")
    expect_equal(r$n_negated, 1)
    expect_equal(r$label, "negative")
  }
})

test_that("negated negative words count as weakly positive", {
  r <- sentiment_score(text = "not bad", positive = "good", negative = "bad")
  expect_equal(r$score_positive, 0.5)
  expect_equal(r$label, "positive")
})

test_that("near ties are neutral", {
  txt <- paste(c(rep("good", 10), rep("bad", 9)), collapse = " ")
  r <- sentiment_score(text = txt, positive = "good", negative = "bad")
  expect_equal(r$label, "neutral")
})
