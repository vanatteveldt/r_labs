library(tidyverse)
library(ellmer)


chat <- chat_anthropic(model = "claude-sonnet-5-5")
chat$chat("Waarom moet een sociologie student het R package ellmer leren?")



texts <- read_csv("data/trump_tweets.csv") |> select(id, text)

chat <- chat_anthropic(model = "claude-sonnet-5-5")
instruction = "Please code whether this tweet contains false or misleading information about the elections or election outcomes. Start with yes or no, then give your reasoning"

texts <- texts |> 
  head() |> 
  mutate(prompt=interpolate("{{instruction}}\n\nThe tweet:\n{{text}}"))

texts <- texts |> mutate(answer=parallel_chat_text(chat, prompt))
texts$answer[1]

texts <- texts |> mutate(clean=str_to_lower(answer) |> str_replace_all("[^a-z]", " ") |> str_squish()) 

summarize(texts, disinfo = mean(str_starts(clean, "yes")))

                      