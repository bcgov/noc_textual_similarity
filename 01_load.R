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
library(readxl)
library(janitor)
library(tidytext)
library(widyr)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions-------------------------

fractional_minkowski <- function(x, y, p) {
  sum(abs(x - y)^p)^(1/p)
}

read_data <- function(file_name){
  read_excel(here("data", file_name))%>%
    clean_names()%>%
    select(o_net_soc_code, element_name, scale_name, data_value)%>%
    pivot_wider(names_from = scale_name, values_from = data_value)%>%
    mutate(score=sqrt(Importance*Level),
           category=(str_split(file_name,"\\.")[[1]][1]))%>%
    unite(element_name, category, element_name, sep=": ")%>%
    select(-Importance, -Level)
}

#textual similarity----------------------------------------------

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
  pairwise_similarity(item = noc, feature = word, value = tf_idf)|>
  pivot_wider(names_from = item2, values_from = similarity)|>
  column_to_rownames("item1")|>
  scale()|>
  as.data.frame()|>
  rownames_to_column("item1")|>
  pivot_longer(-item1, names_to = "item2", values_to = "similarity")|>
  mutate(item1=as.numeric(item1),
         item2=as.numeric(item2))|>
  na.omit()


noc_names <- raw|>
  select(noc, description)|>
  distinct()

similarity <- similarity|>
  left_join(noc_names, by = c("item1" = "noc"))|>
  rename(noc1 = item1, description1 = description)|>
  left_join(noc_names, by = c("item2" = "noc"))|>
  rename(noc2 = item2, description2 = description)

write_rds(similarity, here("out", "similarity.rds"))

tf_idf|>
  left_join(noc_names, by = "noc")|>
  write_rds(here("out", "tf_idf.rds"))

# distance based on ONET skill differences ----------------------------

mapping <- read_excel(here("data", "onet2019_soc2018_noc2016_noc2021_crosswalk_consolodated.xlsx"))%>%
  mutate(noc2021=str_pad(noc2021, "left", pad="0", width=5))%>%
  unite(noc, noc2021, noc2021_title, sep=": ")%>%
  select(noc, o_net_soc_code = onetsoc2019)%>%
  distinct()

#the onet data-----------------------------------
tbbl <- tibble(file=c("Skills.xlsx", "Abilities.xlsx", "Knowledge.xlsx", "Work Activities.xlsx"))%>%
  #tbbl <- tibble(file=c("Skills.xlsx"))%>%
  mutate(data=map(file, read_data))%>%
  select(-file)%>%
  unnest(data)%>%
  pivot_wider(id_cols = o_net_soc_code, names_from = element_name, values_from = score)%>%
  inner_join(mapping)%>%
  ungroup()%>%
  select(-o_net_soc_code)%>%
  select(noc, everything())%>%
  group_by(noc)%>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))%>% #mapping from SOC to NOC is not one to one: mean give one value per NOC
  mutate(across(where(is.numeric), ~ if_else(is.na(.), mean(., na.rm=TRUE), .)))|> #for 11 occupations and 4 variables replace missing values with the mean
  column_to_rownames("noc")
# do principal components analysis---------------------

pca <- prcomp(tbbl, center = TRUE, scale. = TRUE)

#screeplot for pca-------------------
factoextra::get_eig(pca)
#keep only the first 5 components of pca---------------------

pca_df <- as.data.frame(pca$x[, 1:8])

# calculate the distance between the first components---------------------

unscaled_distance <- pca_df |>
  proxy::dist(method = function(x, y) fractional_minkowski(x, y, p = .5))|>
  as.matrix()

distance <- unscaled_distance |>
  scale()|>
  as.data.frame()|>
  rownames_to_column("noc")|>
  pivot_longer(-noc, names_to = "noc2", values_to = "distance")|>
  separate(noc, into = c("noc1", "description1"), sep = ": ")|>
  separate(noc2, into = c("noc2", "description2"), sep = ": ")|>
  select(-contains("description"))|>
  mutate(noc1=as.numeric(noc1),
         noc2=as.numeric(noc2))|>
  na.omit()

inner_join(similarity, distance, by = c("noc1", "noc2"))|>
  write_rds(here("out", "dist_and_sim.rds"))

