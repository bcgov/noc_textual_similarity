# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(tidyverse)
library(here)
library(vroom)
library(janitor)
library(tidytext)
library(widyr)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)
raw <- vroom(here("data","noc_2021_version_1.0_-_elements.csv"))|>
  clean_names()|>
  filter(element_type_label_english %in% c("Illustrative example(s)",
                                               "All examples",
                                               "Exclusion(s)",
                                               "Main duties"))|>
  select(noc=code_noc_2021_v1_0, description=class_title, text=element_description_english)

tf_idf <- raw|>
  unnest_tokens(word, text)|>
  mutate(word=textstem::lemmatize_words(word))|>
  count(noc, word, sort = TRUE)|>
  bind_tf_idf(word, noc, n)

similarity <- tf_idf|>
  pairwise_similarity(item = noc, feature = word, value = tf_idf)

noc_names <- raw|>
  select(noc, description)|>
  distinct()

similarity|>
  left_join(noc_names, by = c("item1" = "noc"))|>
  rename(noc1 = item1, description1 = description)|>
  left_join(noc_names, by = c("item2" = "noc"))|>
  rename(noc2 = item2, description2 = description)|>
  write_rds(here("out", "similarity.rds"))

tf_idf|>
  left_join(noc_names, by = "noc")|>
  write_rds(here("out", "tf_idf.rds"))

