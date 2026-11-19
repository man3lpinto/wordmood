#' Default positive and negative word lists
#'
#' Word lists for scoring everyday text such as diaries, covering mood, emotions, health and body symptoms. Every pattern has a **confidence weight**: how sure we are that a word matching it is really positive (or negative). The weights are our own judgement.
#'
#' Four confidence levels are used:
#'
#' * **1* - clearly positive/negative in almost any context ("wonderful", "nauseous").
#' * **0.75* - usually positive/negative, or a wildcard that sometimes catches an unrelated word (`"pain*"` also matches "painting", `"*ache"` also matches "cache").
#' * **0.5* - often positive/negative but depends on context ("hope" in "I hope it passes", "lost", "help").
#' * **0.25* - weak or ambiguous words that only hint at a mood ("fine", "well", "heavy", "barely").
#'
#' Wildcards (see [count_matches()]): `"stem*"` matches word endings (suffixes) such as -ed, -ing, -ly, -ness and -ful, so `"frustrat*"` matches frustrated, frustrating and frustration. `"*stem"` matches word beginnings (prefixes), so `"*sick"` matches homesick and seasick.
#'
#' A broad wildcard with a lower weight is often combined with the exact, certain forms at weight 1. [count_matches()] uses the highest weight that matches, so "painful" scores 1 while "painting" only scores 0.75.
#'
#' Generic negating affixes such as `"un*"`, `"dis*"` or `"*less"` are not used, because they match too many neutral words ("under", "until", "unless"). Negated words (unhappy, hopeless, ...) are listed explicitly, and no word is meant to match both lists.
#'
#' Known limitation: negation is not handled, so "not happy" still counts as a positive word.
#'
#' @return A named numeric vector. The names are the words/patterns and the values are the confidence weights (between 0 and 1).
#'
#' @examples
#' head(default_positive_words())
#' head(default_negative_words())
#'
#' # Patterns we are less sure about names(default_negative_words())[default_negative_words() < 1]
#'
#' # Use them directly with count_matches()
#' w <- c("frustrated", "painting", "painful", "happy")
#' count_matches(w, default_negative_words())                  # 3 words
#' count_matches(w, default_negative_words(), weighted = TRUE) # 1 + 0.75 + 1
#'
#' @name word_lists
NULL

# Give every pattern in `...` the same confidence weight `w`.
# Returns a named numeric vector (names = patterns, values = weights).
weigh <- function(w, ...) {
  patterns <- c(...)
  weights <- rep(w, length(patterns))
  names(weights) <- patterns
  weights
}

#' @rdname word_lists
#' @export
default_positive_words <- function() {
  c(
    # 1 - clearly positive
    weigh(1,
          # happiness
          "happy", "happi*", "joy*", "enjoy*", "glad", "gladly", "gladness",
          "cheerful*", "delight*", "pleased", "pleasant*", "pleasure*", "smil*",
          "laugh*", "fun", "excit*", "thrill*", "ecstatic", "elated",
          # love, care and gratitude
          "love", "loved", "lovely", "loving", "adore", "adored", "adoring",
          "adorable", "appreciat*", "thank*", "grateful*", "gratitude",
          "kindness",
          # calm and well-being
          "calm*", "relax*", "peace*", "comfortable", "comforting", "cozy",
          "cosy", "rested", "refresh*", "energetic", "healthy", "healed",
          "healing", "improve", "improved", "improvement*", "improving",
          "better", "best",
          # quality
          "good", "great", "nice", "nicer", "nicest", "wonderful*",
          "excellent*", "amazing*", "awesome", "fantastic*", "brilliant*",
          "perfect*", "beautiful*", "superb*", "marvel*", "terrific",
          "favorite*", "favourite*",
          # confidence, achievement
          "proud*", "pride", "confident", "confidently", "confidence",
          "optimis*", "hopeful*", "motivat*", "inspired", "inspiring",
          "inspiration*", "success", "successes", "successful*", "achiev*",
          "accomplish*", "satisf*", "celebrat*", "fortunate*", "lucky"
    ),

    # 0.75 - usually positive, or a wildcard that can catch other words
    weigh(0.75,
          "glad*",      # also glade, gladiator
          "great*",     # greater, greatly
          "beaut*",     # beauty, beauties
          "bless*", "recover*", "friendly", "caring", "cute", "win",
          "winning", "positive*"
    ),

    # 0.5 - often positive but depends on context
    weigh(0.5,
          "hope", "hoped", "hoping",           # "I hope it passes"
          "pleas*",                            # also please
          "improv*",                           # also improvise
          "inspir*",                           # also inspiratory
          "success*",                          # also succession
          "confiden*",                         # also confidential
          "energ*", "health*", "help", "helped", "helping", "helpful*",
          "support*", "friend*", "kindly", "easy", "easi*", "safe*",
          "strong*", "warm*", "sunny", "bright*", "fresh", "sweet*",
          "interest*", "funny", "luck*"
    ),

    # 0.25 - weak hint of a positive mood
    weigh(0.25,
          "fine", "well", "ok", "okay", "kind", "cool", "free"
    )
  )
}

#' @rdname word_lists
#' @export
default_negative_words <- function() {
  c(
    # 1 - clearly negative
    weigh(1,
          # sadness
          "sad", "sadly", "sadness", "sadder", "saddest", "unhappy*",
          "unhappi*", "depress*", "miserab*", "gloom*", "lonel*", "cry",
          "cried", "cries", "crying", "grief", "griev*", "mourn*", "heartbr*",
          "heartache*", "disappoint*", "regret*", "hopeless*", "helpless*",
          "worthless*", "useless*", "pointless*",
          # fear and worry
          "worr*", "anxi*", "stress*", "nervous*", "fear", "feared",
          "fearful*", "afraid", "scared", "scary", "panic*", "frighten*",
          "terrified", "terrifying", "dread*", "uneasy",
          # anger
          "angr*", "anger*", "furious*", "annoy*", "irritat*", "frustrat*",
          "hate*", "hatred", "resent*", "disgust*", "jealous*", "envious",
          "cruel*",
          # shame
          "guilt*", "ashamed", "shame*", "embarrass*", "humiliat*",
          # sickness
          "sick", "sickness", "sickly", "*sick", "ill", "illness*", "unwell",
          "nause*", "vomit*", "dizz*", "feverish", "cough", "coughs",
          "coughing", "coughed", "ache", "aches", "aching", "headache*",
          "backache*", "stomachache*", "toothache*", "earache*", "pain",
          "pains", "painful*", "hurt", "hurts", "hurting", "hurtful*", "sore",
          "soreness", "cramp", "cramps", "cramping", "migraine*", "infection*",
          "injur*", "swollen", "uncomfortabl*", "discomfort*",
          # tiredness
          "tired*", "exhausted", "exhausting", "exhaustion", "fatigu*",
          "weary", "sleepless*", "insomnia", "restless*",
          # general negative evaluation
          "bad", "badly", "worse", "worst", "terribl*", "horribl*", "awful*",
          "dreadful*", "nasty", "upset*", "struggl*", "fail*", "unfortunate*",
          "unlucky", "disaster*", "tragic*", "tragedy", "nightmare*",
          "devastat*"
    ),

    # 0.75 - usually negative, or a wildcard that can catch other words
    weigh(0.75,
          "sad*",       # also saddle
          "pain*",      # also painting
          "*ache",      # also cache, mustache
          "sick*",      # also sickle
          "hurt*",      # also hurtle
          "cough*", "cramp*", "fever*", "weak*", "exhaust*", "infect*",
          "isolat*", "bitter*", "overwhelm*", "trouble*", "difficult*",
          "boring", "bored", "trapped", "stuck", "broken", "wrong", "poorly",
          "drained", "unkind*", "negative*"
    ),

    # 0.5 - often negative but depends on context
    weigh(0.5,
          "cry*",       # also crystal
          "problem*",   # also "no problem"
          "alone", "lost", "lose", "losing", "mad", "tough", "forced",
          "unable", "die", "died", "dying", "dead", "death", "kill*", "tears",
          "symptom*"
    ),

    # 0.25 - weak hint of a negative mood
    weigh(0.25,
          "hard", "heavy", "cold", "dark", "barely", "strange", "tear", "slow",
          "busy", "miss", "missed", "missing"
    )
  )
}

#' Default negating words
#'
#' Words that reverse the meaning of the words right after them, such as "not" in "not happy" or "never" in "never felt better". Used by [is_negated()] and [sentiment_score()].
#'
#' Contractions are matched with the `*` wildcard: `"*n't"` matches don't, can't, wasn't, didn't and so on. Both the straight and the curly apostrophe are covered, as well as common spellings without an apostrophe ("dont", "cant").
#'
#' @return A character vector of negating words and patterns.
#'
#' @examples
#' default_negators()
#'
#' @export
default_negators <- function() {
  c("not", "no", "never", "nothing", "nobody", "none", "neither", "nor",
    "without", "hardly", "cannot",
    "*n't", "*n\u2019t",   # straight and curly apostrophe
    "dont", "didnt", "doesnt", "isnt", "wasnt", "arent", "werent", "cant",
    "couldnt", "wouldnt", "shouldnt", "havent", "hasnt", "hadnt")
}
