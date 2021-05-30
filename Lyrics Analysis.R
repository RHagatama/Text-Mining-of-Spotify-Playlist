library(tidyverse)
library(tidytext)
library(geniusr)
library(wordcloud)
library(topicmodels)

## Load Playlist
playlist <- read_csv("my_playlist.csv")
playlist

## Input API token
genius_token()

## Get song ID
song_id <- list()
for (i in 1:nrow(playlist)) {
  song_id[i] <- search_genius(paste(playlist$song[i], playlist$artist[i]))$content[[1]]$id
  print(i)
}

song_id <- as_vector(song_id)

## Get Lyric
lyrics <- tibble()
while (length(song_id) > 0) {
  for (i in song_id) {
    tryCatch({
      lyrics <- rbind(get_lyrics_id(i), lyrics)
      successful <- unique(lyrics$song_id)
      song_id <- song_id[!song_id %in% successful]
      print(paste("done -", i))
      print(paste("New length is", length(song_id)))
    }, error = function(e){})
  }
}

## Tokenisasi
add_stop_words <- tribble(
  ~word, ~lexicon,
  "ooh", "custom",
  "whoa", "custom"
)

stop_words2 <- stop_words |>
  bind_rows(add_stop_words)

lyrics_tidy <- lyrics |>
  unnest_tokens(word, line) |>
  anti_join(stop_words2)

## Count word
word_count <- lyrics_tidy |>
  count(word) |>
  arrange(desc(n)) |>
  mutate(word2 = fct_reorder(word, n))

## Visualisasi word frequency
word_count2 <- word_count |>
  top_n(30, n)

ggplot(word_count2, aes(word2, n)) +
  geom_col() +
  coord_flip()

wordcloud(
  words = word_count2$word, 
  freq = word_count2$n,
  max.words = 30,
  colors = "brown"
)

## Get sentiment
lyrics_sentiment <- lyrics_tidy |>
  inner_join(get_sentiments("nrc"))

## Sentiment count
sentiments_count <- lyrics_sentiment |>
  filter(!sentiment %in% c("positive", "negative")) |>
  count(word, sentiment) |>
  group_by(sentiment) |>
  top_n(15, n) |>
  ungroup() |>
  mutate(
    word2 = fct_reorder(word, n),
    sentiment2 = fct_reorder(sentiment, n)
  ) |>
  arrange(desc(n))

ggplot(sentiments_count, aes(word2, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

sentiments_count2 <- lyrics_sentiment |>
  filter(!sentiment %in% c("positive", "negative")) |>
  count(sentiment) |>
  top_n(30, n) |>
  mutate(sentiment2 = fct_reorder(sentiment, n)) |>
  arrange(desc(n))

ggplot(sentiments_count2, aes(sentiment2, n)) +
  geom_col() +
  coord_flip()

## Sentiment Analysis
# Make dtm
lyrics_mx <- lyrics_tidy |>
  count(song_id, word) |>
  cast_dtm(song_id, word, n) |>
  as.matrix()

lda_topics <- lyrics_mx |>
  LDA(
    k = 2, 
    method = "Gibbs",
    control = list(seed = 123)
  ) |>
  tidy(matrix = 'beta') |>
  arrange(desc(beta))

# Visualisasi
word_prob <- lda_topics |>
  group_by(topic) |>
  top_n(15, beta) |>
  ungroup() |>
  mutate(term2 = fct_reorder(term, beta))

ggplot(word_prob, aes(term2, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
