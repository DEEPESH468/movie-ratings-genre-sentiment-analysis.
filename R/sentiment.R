# A transparent lexicon baseline used for locally sampled audience reviews.
# A word preceded by a negator in the previous three tokens flips polarity.
score_review <- function(review_text) {
  positive <- c(
    "amazing", "beautiful", "best", "brilliant", "charming", "enjoyed", "excellent",
    "fantastic", "fun", "funny", "good", "great", "impressive", "love", "loved",
    "masterpiece", "moving", "perfect", "powerful", "strong", "wonderful"
  )
  negative <- c(
    "annoying", "awful", "bad", "boring", "confusing", "disappointing", "dull",
    "fail", "flat", "forgettable", "hate", "hated", "mess", "poor", "predictable",
    "slow", "terrible", "weak", "waste", "worst"
  )
  negators <- c("no", "not", "never", "neither", "nor", "hardly", "n't")

  text <- tolower(gsub("<[^>]+>", " ", paste(review_text, collapse = " ")))
  text <- gsub("&[a-z]+;", " ", text)
  words <- unlist(strsplit(gsub("[^[:alpha:]']", " ", text), "\\s+"), use.names = FALSE)
  words <- words[nzchar(words)]
  if (!length(words)) return(list(sentiment = "Mixed", score = 0.5))

  polarity <- ifelse(words %in% positive, 1, ifelse(words %in% negative, -1, 0))
  hits <- which(polarity != 0)
  for (i in hits) {
    if (i > 1L) {
      prior <- words[max(1L, i - 3L):(i - 1L)]
      is_negated <- any(prior %in% negators) || any(grepl("n't$", prior))
      if (is_negated) polarity[i] <- -polarity[i]
    }
  }
  net <- sum(polarity)
  hit_count <- sum(polarity != 0)
  scaled <- if (hit_count == 0L) 0 else net / hit_count
  sentiment <- if (scaled >= 0.15) "Positive" else if (scaled <= -0.15) "Critical" else "Mixed"
  list(sentiment = sentiment, score = (scaled + 1) / 2)
}
