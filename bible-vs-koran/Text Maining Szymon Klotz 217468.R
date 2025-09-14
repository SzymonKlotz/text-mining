# Analiza tekstowa Biblii i Koranu
# Autor Szymon Klotz

# Instalacja i załadowanie potrzebnych pakietów
install.packages(c("tidyverse", "tidytext", "textstem", "wordcloud","textdata"))
library(textdata)
library(tidyverse)
library(tidytext)
library(textstem)
library(wordcloud)
library(ggplot2)
library(syuzhet)
library(dplyr)

# Załadowania baz tesktowych zaimportowanych z bazy gutenberg.org
bible_raw <- readLines("C:/Users/.../Biblia.txt", encoding = "UTF-8") # Biblia
quran_raw <- readLines("C:/Users/.../Koran.txt", encoding = "UTF-8") # Koran

# __Analiza występowania słów__

# Podtsawowe oczyszczanie zbioru tekstowego
bible_df <- tibble(source = "Bible", text = bible_raw)
quran_df <- tibble(source = "Quran", text = quran_raw)

# Złączenie dwóch ksiąg w jeden zbiór + kolejne oczyszczanie
all_texts <- bind_rows(bible_df, quran_df) %>%
  filter(str_detect(text, "[a-zA-Z]")) %>%           
  mutate(text = str_to_lower(text))                  

# Tokenizacja słów z tekstów
tokens <- all_texts %>%
  unnest_tokens(word, text)

# Usunięcie stop words i lematyzacja tekstu
data("stop_words")
tokens_clean <- tokens %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!str_detect(word, "^\\d+$"))  # usunięcie słów będących cyframi

# Policzenie powtarzających się słów dla każdej ze świętych ksiąg
word_freq <- tokens_clean %>%
  count(source, word, sort = TRUE)

# Policzenie łącznej liczby słów w każdej księdze
word_totals <- tokens_clean %>%
  count(source) %>%
  rename(total_words = n)

# Połączenie z word_freq (częstość słów)
word_freq_normalized <- word_freq %>%
  left_join(word_totals, by = "source") %>%
  mutate(freq_percent = n / total_words * 100)

# Filtr: tylko słowa występujące >5 razy
word_freq_filtered <- word_freq %>%
  filter(n > 5)

# Dla każdej księgi wybierane 10 najczęstszych słów
top10_words <- word_freq %>%
  group_by(source) %>%
  slice_max(n, n = 10) %>%
  ungroup()

# TOP 10 słów znormalizowane wg udziału procentowego
top10_words_norm <- word_freq_normalized %>%
  group_by(source) %>%
  slice_max(freq_percent, n = 10) %>%
  ungroup()

# Wykres porównujący 10 najczęstszych słów (wg procentów)
ggplot(top10_words_norm, aes(x = reorder(word, freq_percent), y = freq_percent, fill = source)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top 10 najczęstszych słów (procentowo)",
       x = "Słowo", y = "% wszystkich słów") +
  theme_minimal()

# Zbiór słów i liczba ich wystąpień – osobno dla każdej księgi
word_freq_bible <- tokens_clean %>%
  filter(source == "Bible") %>%
  count(word, sort = TRUE)

word_freq_quran <- tokens_clean %>%
  filter(source == "Quran") %>%
  count(word, sort = TRUE)

# Chmura słów – Biblia
wordcloud(words = word_freq_bible$word,
          freq = word_freq_bible$n,
          min.freq = 5,
          max.words = 200,
          scale = c(4, 0.5),
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"),
          main = "Bible")

# Chmura słów – Koran
wordcloud(words = word_freq_quran$word,
          freq = word_freq_quran$n,
          min.freq = 5,
          max.words = 200,
          scale = c(4, 0.5),
          random.order = FALSE,
          colors = brewer.pal(8, "Set1"),
          main = "Quran")

#____________________________Podsumowanie Występowanie Słów____________________________
# Biblia - najczęściej występujące słowa to: lord, god, you, thy, thou, ye, shall, son, king, israel.
# Biblia - tematyka skupia się na Bogu, narodzie Izraela, relacjach rodzinnych i zasadach moralnych.
# Koran - najczęściej występujące słowa to: god, ye, mohammed, you, hath, lord, verily.
# Koran - wyraźne skupienie na Bogu oraz proroku Mahomecie.
#______________________________________________________________________________________

# __Analiza Bigramów___

# Tokenizacja bigramów
bigrams <- all_texts %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Rozdzielenie na dwa słowa
bigrams_sep <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

# Usunięcie stop words i bigramów z cyframi
data("stop_words")
bigrams_clean <- bigrams_sep %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1), !is.na(word2),
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d"))

# Połączenie słów i ich liczenie
bigrams_count <- bigrams_clean %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(source, bigram, sort = TRUE)

# Usunięcie bigramów NA NA
bigrams_count <- bigrams_count %>%
  filter(bigram != "NA NA")

# Wybranie top 10 bigramów z każdej księgi
top10_bigrams <- bigrams_count %>%
  group_by(source) %>%
  slice_max(n, n = 10) %>%
  ungroup()

# Wykres najczęściej występujących bigramów
ggplot(top10_bigrams, aes(x = reorder(bigram, n), y = n, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 najczęstszych bigramów",
       x = "Bigram", y = "Liczba wystąpień") +
  theme_minimal()

#____________________________Podsumowanie Bigramy____________________________
# Bigramy pokazują różnice w języku.
# Biblia: styl archaiczny, gramatyka XVII-wieczna, styl nakazowo-proroczy.
# Koran: styl bardziej formalny i komentarzowy, odniesienia do tradycji islamskiej.
#____________________________________________________________________________

# __Analiza Sentymentów__
# Pojedyńcze Słowa

# Załaduj słownik NRC
nrc <- get_sentiments("nrc")

# Dodanie emocji do słów
emotion_words <- tokens_clean %>%
  inner_join(nrc, by = "word")  # dołącz emocje

# Obliczanie liczby słów przypisanych do emocji w każdej księdze
emotion_counts <- emotion_words %>%
  count(source, sentiment, sort = TRUE)

# Policz łączną liczbę słów z emocjami dla każdej księgi
emotion_totals <- emotion_words %>%
  count(source) %>%
  rename(total_emotion_words = n)

# Policz wystąpienia każdej emocji
df_emotions <- emotion_words %>%
  count(source, sentiment) %>%
  left_join(emotion_totals, by = "source") %>%
  mutate(freq_percent = n / total_emotion_words * 100)

# Wykres procentowy emocji
ggplot(df_emotions, aes(x = sentiment, y = freq_percent, fill = source)) +
  geom_col(position = "dodge") +
  labs(title = "Emocje w Biblii i Koranie (procentowo)",
       x = "Emocja", y = "% wszystkich słów z emocjami") +
  theme_minimal()
# ____________ Podsumowanie analizy sentymentu słów _______________________
# W obu tekstach najwięcej słów wyraża emocje pozytywne, z lekką przewagą w Biblii.
# Biblia zawiera nieco więcej słów związanych z 'joy' (radość) i 'positive' emocjami.
# Koran wykazuje większy udział słów związanych z 'fear' (strach), 'anger' (złość) i 'disgust' (wstręt).
# Ogólnie Biblia ma bardziej pozytywny profil emocjonalny, natomiast Koran wykazuje więcej emocji związanych z surowością i przestrogą.
# _________________________________________________________________________

# Zdania



# Usuniecie numerów wersetów i cyfr na końcach zdań
bible_clean <- gsub("\\d+:\\d+", "", bible_raw)
quran_clean <- gsub("\\d+:\\d+", "", quran_raw)
quran_clean <- gsub("(?<=\\b)\\d+(?=\\b)", "", quran_clean, perl = TRUE)  # usuń same cyfry
quran_clean <- gsub("\\s*\\d+\\.", ".", quran_clean)  # zamiana np. " 8." na kropkę

# Połączenie tekstu w jeden ciąg
bible_text <- paste(bible_clean, collapse = " ")
quran_text <- paste(quran_clean, collapse = " ")

# Zamiana ";" i ":" na "." - uznanie ich za koniec zdania
bible_text <- gsub("[;:]", ".", bible_text)
quran_text <- gsub("[;:]", ".", quran_text)

# Dzielenie na zdania
bible_sentences <- get_sentences(bible_text)
quran_sentences <- get_sentences(quran_text)

# Analiza sentymentu
bible_sentiment <- get_sentiment(bible_sentences, method = "syuzhet")
quran_sentiment <- get_sentiment(quran_sentences, method = "syuzhet")

# Najbardziej pozytywne i negatywne zdania
most_pos_bible <- bible_sentences[which.max(bible_sentiment)]
most_neg_bible <- bible_sentences[which.min(bible_sentiment)]
most_pos_quran <- quran_sentences[which.max(quran_sentiment)]
most_neg_quran <- quran_sentences[which.min(quran_sentiment)]

# 7. Wyświetlenie
cat("\U0001F4D8 Biblia – najbardziej pozytywne zdanie:\n", most_pos_bible, "\n\n")
cat("\U0001F4D7 Koran – najbardziej pozytywne zdanie:\n", most_pos_quran, "\n\n")
cat("\U0001F4D8 Biblia – najbardziej negatywne zdanie:\n", most_neg_bible, "\n\n")
cat("\U0001F4D7 Koran – najbardziej negatywne zdanie:\n", most_neg_quran, "\n\n")

# Wykresy do analizy sentymentu zdań

y_min <- min(c(bible_sentiment, quran_sentiment))
y_max <- max(c(bible_sentiment, quran_sentiment))

par(mfrow = c(1, 2))

plot(bible_sentiment, type = "l", col = "blue",
     main = "Sentyment zdań – Biblia",
     ylab = "Sentyment", xlab = "Zdania",
     ylim = c(y_min, y_max))

plot(quran_sentiment, type = "l", col = "darkgreen",
     main = "Sentyment zdań – Koran",
     ylab = "Sentyment", xlab = "Zdania",
     ylim = c(y_min, y_max))


# Histogram rozkładu sentymentu
bible_df <- data.frame(sentiment = bible_sentiment, source = "Bible")
quran_df <- data.frame(sentiment = quran_sentiment, source = "Quran")

sentiment_all <- rbind(bible_df, quran_df)

ggplot(sentiment_all, aes(x = sentiment, fill = source)) +
  geom_histogram(binwidth = 0.5, position = "identity", alpha = 0.6) +
  labs(title = "Rozkład sentymentu zdań",
       x = "Wartość sentymentu", y = "Liczba zdań") +
  theme_minimal()

# Średni sentyment – wykres słupkowy

avg_sentiment <- sentiment_all %>%
  group_by(source) %>%
  summarise(mean_sentiment = mean(sentiment))

ggplot(avg_sentiment, aes(x = source, y = mean_sentiment, fill = source)) +
  geom_col() +
  labs(title = "Średni sentyment zdań", y = "Średni sentyment") +
  theme_minimal()

# ____________ Podsumowanie analizy sentymentu zdań _______________________
# Biblia ma szerszy zakres zdań, ale bardziej zagęszczony rozkład wokół 0.
# Koran wykazuje bardziej równomierne wahania i większą ilość skrajnych wartości.
# Najwięcej zdań w obu tekstach ma sentyment bliski zeru (neutralny).
# Biblia ma więcej zdań o wyraźnym sentymencie dodatnim i ujemnym.
# Koran ma bardziej rozłożony rozkład, z większym udziałem słabszych emocji.
# Średni sentyment zdań w Koranie jest wyższy niż w Biblii.
# Wskazuje to, że mimo występowania negatywnych emocji, ogólna tonacja Koranu jest nieco bardziej pozytywna.
# __________________________________________________________________________

# Obie święte księgi mają wyraźnie duchowy i normatywny charakter, ale różnią się tonem.

# Biblia częściej akcentuje relacje i obietnice, Koran częściej ostrzega i nakazuje.
