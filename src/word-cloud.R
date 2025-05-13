# Required packages
library(dplyr)
library(stringr)
library(wordcloud)

# Define function
make_wordcloud <- function(data) {
  word_list <- data |>
    filter(!is.na(words)) |>
    pull(words) |>
    str_split(",") |>
    unlist() |>
    str_trim() |>
    str_to_lower()
  
  word_freq <- table(word_list)
  
  wordcloud(words = names(word_freq),
            freq = as.integer(word_freq),
            min.freq = 1,
            scale = c(2, 0.05),
            random.order = FALSE,
            colors = c("#cceeff", "#99ddff", "#66ccff", "#33bbff", "#0099cc", "#006699"))
}

# Example usage:
load("throughput/processed-data.RData")
sample <- data_processed |> filter(name == "Well Water")

make_wordcloud(sample)
