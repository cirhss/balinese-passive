library(tidyverse)
library(readxl)
library(vcd)
library(ggpubr)

# # exclude these columns from sheet 2 that do not match with columns in sheet 1
# excluded_column_from_sheet2 <- setdiff(sh2col, sh1col)

# read the sheets for CORPUS Data =====
## sheet 1 =====
# read_xlsx("data-first-and-second-batch-20240209.xlsx", sheet = 1) |> 
#   write_rds("corpus-data-from-sheet-1.rds")
sheet1 <- read_rds("corpus-data-from-sheet-1.rds") |> 
  # NOTE: empty cells in `action_or_state` column need to be filled with "action"
  mutate(action_or_state = if_else(is.na(action_or_state), "action", action_or_state)) |> 
  filter(No != 291) |> 
  rename(`verba target` = `verbat target`) |> 
  mutate(sheets = 1,
         halaman = as.integer(halaman)) |> 
  mutate(`Tipe pasif` = if_else(is.na(`Tipe pasif`) & No == 25 & `akar verba` == "antenang" & `verba target` == "antenangaa", "a", `Tipe pasif`),
         `Tipe pasif` = if_else(is.na(`Tipe pasif`) & No == 219 & `akar verba` == "carita" & `verba target` == "kacaritayang", "ka", `Tipe pasif`),
         agent_phrase = replace(agent_phrase, No == 261, "teken"))
sheet1 <- sheet1 |> 
  mutate(root_level = if_else(is.na(root_level) & `verba target` == "antenangaa", "low", root_level),
         root_level = if_else(is.na(root_level) & `verba target` == "kasorang tanding", "mider", root_level),
         root_level = if_else(is.na(root_level) & `akar verba` %in% c("cerita", "carita"), "low", root_level),
         `akar verba` = replace(`akar verba`, `akar verba` == "in" & sheets == 1 & `verba target` == "panggih", "panggih"))
## sheet 2 ====
# read_xlsx("data-first-and-second-batch-20240209.xlsx", sheet = 2) |>
#   write_rds("corpus-data-from-sheet-2.rds")
sheet2 <- read_rds("corpus-data-from-sheet-2.rds") |> 
  # select(!all_of(excluded_column_from_sheet2)) |> 
  # NOTE: empty cells in `action_or_state` column need to be filled with "action"
  mutate(action_or_state = if_else(is.na(action_or_state), "action", action_or_state)) |> 
  mutate(sheets = 2,
         halaman = as.integer(halaman)) |> 
  rename(`verba target` = `verbat target`) |> 
  mutate(root_level = if_else(is.na(root_level) & `akar verba` %in% c("jangkep_puput"), "high", root_level),
         root_level = replace(root_level, root_level == "halus", "high")) |> 
  mutate(`verba target` = replace(`verba target`, `Tipe pasif` == "kaserahan", "kaserahan"),
         `Tipe pasif` = replace(`Tipe pasif`, `Tipe pasif` == "kaserahan", "ka"))

## combine sheet 1 and sheet 2 ======
df <- bind_rows(sheet1, sheet2) |> 
  mutate(across(where(is.character), ~str_trim(., "both"))) |> 
  mutate(across(where(is.character), ~str_replace_all(., "\\s{2,}", " "))) |> 
  mutate(passive_combination = if_else(str_detect(`Tipe pasif`, "_"), "mixed", "single")) |> 
  mutate(root_level = replace(root_level, root_level == "kow", "low"))

### further edits ======
df <- df |> 
  #### editing the roots =====
  mutate(affiks = replace(affiks, `akar verba` == "ubadin", "in_kausatif")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` == "ubadin", "ubad")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("tepukin", "tepukina"), "tepuk")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("tekekang"), "tekek")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("tegakang"), "tegak")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("tangkilin"), "tangkil")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("suwudan"), "suwud")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("sautin"), "saut")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("saurin"), "saur")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("sadahang"), "sadah")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("rurubin"), "rurub")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("punduhang"), "punduh")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("pesuang"), "pesu")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("pademang"), "padem")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("matiang"), "mati")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("kencanin"), "kencan")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("karyanin"), "karya")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("kalahin"), "kalah")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("jemakin"), "jemak")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("jangkuake"), "jangkuak")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("jagjagin"), "jagjag")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("isinin"), "isi")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("gambahang"), "gambah")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("gadeang"), "gade")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("dudugang"), "dudug")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("dengokin"), "dengok")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("dapetang"), "dapet")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("aturang"), "atur")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("antenang"), "anten")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("adanin"), "adan")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("anggo", "anggon", "anngo", "anngon"), "anggo")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("angge", "anggen"), "angge")) |> 
  mutate(`akar verba` = replace(`akar verba`, `akar verba` %in% c("carita", "cerita"), "carita")) |> 
  mutate(agent_eksplisit = replace(agent_eksplisit, is.na(agent_eksplisit), "unknown"), # NA in agent_eksplisit refers to 'unknown'
         agent_eksplisit = replace(agent_eksplisit, agent_eksplisit == "2nd person impeartive", "2nd person imperative"), # typo
         agent_eksplisit = replace(agent_eksplisit, agent_eksplisit == "obligatory", "y")) # entries marked with "obligatory" is also explicit mention of the agent, but the phrase type is obligatory NP, not a PP (which is for "y")

# Analysis =====

## 1. count the expression of agent (`agent_eksplisit`) and the type of the passives ("ka-" or "-a") ======
df_count <- df %>% 
  mutate(`Tipe pasif` = str_split(`Tipe pasif`, "_")) |> 
  unnest_longer(col = "Tipe pasif") |> 
  select(agent_eksplisit, `Tipe pasif`) |> 
  count(agent_eksplisit, `Tipe pasif`)

agent_expression_a <- df_count %>% 
  filter(`Tipe pasif` == "a") |> 
  arrange(desc(n)) |> 
  mutate(perc = round(n/sum(n) * 100, 2)) |> 
  write_tsv("output/01-agent-realisation-A-passive.tsv")

agent_expression_ka <- df_count %>% 
  filter(`Tipe pasif` == "ka") |> 
  arrange(desc(n)) |> 
  mutate(perc = round(n/sum(n) * 100, 2)) |> 
  write_tsv("output/01-agent-realisation-KA-passive.tsv")

is_agent_explisit <- agent_expression_a |> 
  bind_rows(agent_expression_ka) |> 
  filter(str_detect(agent_eksplisit, "^(2nd|1st)", negate = TRUE)) |> 
  mutate(`Tipe pasif` = if_else(`Tipe pasif` == "ka", "ka-", "-a"),
         agent_presence = if_else(agent_eksplisit %in% c("y"),
                                  "yes (in the same clause with the target verb)",
                                  "in previous clause"),
         agent_presence = if_else(agent_eksplisit == "unknown",
                                  "unknown",
                                  agent_presence),
         agent_presence = if_else(agent_eksplisit == "next clause",
                                  "next clause",
                                  agent_presence),
         agent_presence = factor(agent_presence, levels = c("yes (in the same clause with the target verb)",
                                                            "in previous clause",
                                                            "unknown",
                                                            "next clause")),
         `Tipe pasif` = factor(`Tipe pasif`, levels = c("ka-", "-a"))) |> 
  filter(agent_presence != "next clause")
is_agent_explisit_summarised <- is_agent_explisit |> 
  group_by(agent_presence, `Tipe pasif`) |> 
  summarise(n = sum(n),
            .groups = "drop") |> 
  arrange(`Tipe pasif`)

### matrix table ====
is_agent_explisit_mtx <- is_agent_explisit_summarised |> 
  pivot_wider(names_from = `Tipe pasif`, values_from = "n", values_fill = 0L) |> 
  column_to_rownames(var = "agent_presence") |> 
  as.matrix()
names(dimnames(is_agent_explisit_mtx)) <- c("Agent presence", "Passive type")

### table for the explisit presence of the Agent ====
is_agent_explisit_summarised |> 
  group_by(`Tipe pasif`) |> 
  mutate(perc_by_passive_type = round(n/sum(n) * 100, 2)) |> 
  mutate(vals = paste(n, " (", perc_by_passive_type, "%)", sep = "")) |> 
  select(-perc_by_passive_type, -n) |> 
  pivot_wider(names_from = `Tipe pasif`, 
              values_from = "vals", 
              values_fill = "0") |> 
  rename(`'-a` = `-a`) |> 
  select(agent_presence, `'-a`, `ka-`) |> 
  write_tsv("output/01-tables-for-agent-presence.tsv")


### fisher exact test ====
fisher.test(is_agent_explisit_mtx)
fisher.test(is_agent_explisit_mtx)$p.value
fisher.test(is_agent_explisit_mtx)$p.value < 0.001
fisher.test(is_agent_explisit_mtx)$p.value < 0.0001
fisher.test(is_agent_explisit_mtx)$p.value < 0.00001

### chi-squared test ====
chisq.test(is_agent_explisit_mtx)

# Pearson's Chi-squared test
# 
# data:  is_agent_explisit_mtx
# X-squared = 74.43, df = 2, p-value < 2.2e-16

chisq.test(is_agent_explisit_mtx)$residuals

### cramer's V ====
colnames(is_agent_explisit_mtx) <- factor(c("ka-", "-a"), levels = c("ka-", "-a"))
round(vcd::assocstats(is_agent_explisit_mtx)$cramer, digits = 2)

### association plot ====
png("figs/01-2-association-plot-agent-presence.png", width = 9, height = 6,
    units = "in", res = 300)
vcd::assoc(t(is_agent_explisit_mtx), shade = TRUE)
dev.off()

### barplot =====
is_agent_explisit_summarised |> 
  group_by(`Tipe pasif`) |> 
  mutate(perc_by_passive = round(n/sum(n) * 100, 1)) |> 
  ggplot(aes(fill = agent_presence, y = n, x = `Tipe pasif`)) + 
  geom_col(position = "fill") + 
  # coord_flip() + 
  # scale_fill_discrete(limits = c("-a", "ka-")) +
  geom_text(aes(label = paste("n=", n, " (", perc_by_passive, "%)", sep = "")), 
            position = position_fill(.5)) +
  labs(x = "Passive types", y = "Proportion",
       fill = "Agent presence") +
  theme(axis.text.x.bottom = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        axis.title.y.left = element_text(size = 14))
ggsave("figs/01-1-barplot-for-agent-presence.png", height = 6, width = 10, units = "in",
       dpi = 300)


### 1.1 for agent_eksplisit marked as "y", what are the frequencies of NP vs. PP for each type of passive? ====
#### 1.1.1 NP vs. PP agents for "ka-" and "a-" ======
agent_phrase_type <- df |> 
  filter(agent_eksplisit == "y") |> 
  mutate(agent_phrase = if_else(!agent_phrase %in% c("np", "NP"), "pp", agent_phrase))  |>  
  select(`Tipe pasif`, agent_phrase) |> 
  
  # split the multiple entry first
  mutate(passive_type = str_split(`Tipe pasif`, "_")) |> 
  unnest_longer(col = passive_type) |> 
  select(-`Tipe pasif`) |> 
  count(agent_phrase, passive_type) |> 
  group_by(passive_type) |> 
  mutate(perc_by_passive_type = round(n/sum(n) * 100, 2)) |> 
  arrange(passive_type, desc(perc_by_passive_type))
agent_phrase_type

#### 1.1.2 PP freq. for "ka-" and "-a" =======
agent_preposition_type <- df |> 
  filter(agent_eksplisit == "y") |> 
  filter(!agent_phrase %in% c("np", "NP")) |> 
  
  # split the multiple entry first
  mutate(passive_type = str_split(`Tipe pasif`, "_")) |> 
  unnest_longer(col = passive_type) |> 
  select(-`Tipe pasif`) |> 
  count(agent_phrase, passive_type) |> 
  arrange(passive_type, desc(n)) |> 
  group_by(passive_type) |> 
  mutate(perc_by_passive_type = round(n/sum(n) * 100, 2))
agent_preposition_type 

#### 1.1.3 PP freq. all passives =======
agent_preposition_all_passive <- df %>%
  filter(agent_eksplisit == "y") %>%
  filter(!agent_phrase %in% c("np", "NP")) %>%
  
  # split the multiple entry first
  mutate(passive_type = str_split(`Tipe pasif`, "_")) |> 
  unnest_longer(col = passive_type) |> 
  count(agent_phrase, sort = TRUE) |> 
  mutate(perc_agent_prep_all = round(n/sum(n) * 100, 2))
agent_preposition_all_passive

## 2. coordination between passive types =======
### 2.1. for co-occurring passive verbs, how many of them are from the same types? (e.g., ka- and ka-, and -a and -a) ====
passive_coordination <- df |> 
  filter(str_detect(`Tipe pasif`, "_")) |> 
  count(`Tipe pasif`, sort = TRUE)
passive_coordination

### 2.2. if mixed, how many times are all -a vs. all ka-? collapsing all -a and ka- =====
passive_coordination_collapse <- passive_coordination |> 
  mutate(passive_type = "mixed (ka- with -a)",
         passive_type = if_else(str_detect(`Tipe pasif`, "(ka_ka|ka_ka_ka)"), "ka- with ka-", passive_type),
         passive_type = if_else(str_detect(`Tipe pasif`, "(\\ba_a\\b|\\ba_a_a\\b|\\ba_a_a_a\\b)"), "-a with -a", passive_type)) |> 
  group_by(passive_type) |> 
  summarise(n = sum(n)) |> 
  mutate(perc = round(n/sum(n) * 100, 2))
passive_coordination_collapse
passive_coordination_collapse |> 
  write_tsv("output/02-2-passive-coordination-analysis.tsv")

### 2.3. counting the frequency of mixed vs. single passive type =====
passive_coordination_mixed <- passive_coordination |> 
  mutate(identity_type = if_else(`Tipe pasif` %in% c("ka_a", "a_ka"), "mixed", "single"))
passive_coordination_mixed |> 
  group_by(identity_type) |> 
  summarise(n = sum(n))
passive_coordination_mixed

## 3. Speech level of the root of the passive verbs ========
### 3.1 check if the multiple roots in a row match with the other annotation (e.g., root_level) ====
multiple_root_is_a_match_yes <- df |> 
  filter(str_detect(`akar verba`, "_")) |> 
  select(`akar verba`, root_level) |> 
  mutate(n_word = str_count(`akar verba`, "_"), 
         n_level = str_count(root_level, "_")) |> 
  mutate(word_equal_level = n_word == n_level) |> #
  as.data.frame()
multiple_root_is_a_match_yes
all(multiple_root_is_a_match_yes$word_equal_level)
# [1] TRUE <- yes, the number of multiple root matches the number of multiple annotation of the speech level

### 3.2 frequency of speech level of the roots by the passive types ("ka-" and "-a") =====
root_level <- df |> 
  select(root = `akar verba`, root_level, passive_type = `Tipe pasif`) |> 
  mutate(across(.cols = where(is.character), ~str_split(., "_"))) |> 
  unnest_longer(col = c(root, root_level, passive_type)) |> 
  mutate(root = str_to_lower(root))
root_level
root_level_by_passive_type <- root_level |> 
  group_by(passive_type, root_level) |> 
  summarise(n_root = n_distinct(root)) |> 
  arrange(passive_type, desc(n_root)) |> 
  mutate(perc_by_passive_type = round(n_root/sum(n_root) * 100, 2)) |> 
  mutate(passive_type = if_else(passive_type == "ka", "ka-", "-a"),
         passive_type = factor(passive_type, level = c("ka-", "-a")),
         root_level = factor(root_level, level = c("high", "mider", "low")))
root_level_by_passive_type

#### 3.2.1 try chi-square or fisher exact to test type freq. of verb root by speech level and passive type =====
root_level_by_passive_type_mtx <- root_level_by_passive_type |> 
  select(-perc_by_passive_type) |> 
  ungroup() |> 
  pivot_wider(names_from = root_level, values_from = n_root) |> 
  column_to_rownames(var = "passive_type") |> 
  as.matrix()
names(dimnames(root_level_by_passive_type_mtx)) <- c("Passive type", "Speech level categories")
chisq.test(root_level_by_passive_type_mtx)
chisq.test(root_level_by_passive_type_mtx)$residuals
round(vcd::assocstats(root_level_by_passive_type_mtx)$cramer, digits = 2)

#### 3.2.2 tables for the count ====
root_level_by_passive_type |> 
  mutate(vals = paste(n_root, " (", perc_by_passive_type, "%)", sep = "")) |> 
  select(-n_root, -perc_by_passive_type) |> 
  pivot_wider(names_from = passive_type, values_from = "vals") |> 
  write_tsv(file = "output/3-2-2_tables-for-the-root-speech-level-and-passive-type.tsv")

#### 3.2.3 barplot ====
root_level_by_passive_type |> 
  ggplot(aes(x = passive_type, 
             y = n_root, 
             fill = root_level)) + 
  geom_col(position = "fill") +
  geom_text(aes(label = paste("n=", n_root, " (", perc_by_passive_type, "%)", sep = "")), 
            position = position_fill(.5)) +
  labs(x = "Passive types", y = "Proportion",
       fill = "Root Speech Level") +
  theme(axis.text.x.bottom = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.x.bottom = element_text(size = 14),
        axis.title.y.left = element_text(size = 14))
ggsave("figs/02-2-barplot-for-root-speech-level-and-passive-type.png", height = 6, width = 7, units = "in",
       dpi = 300)


#### 3.2.4 association plot ====
colnames(root_level_by_passive_type_mtx)[colnames(root_level_by_passive_type_mtx) == "mider"] <- "neutral"
png("figs/02-1-association-plot-root-speech-level-and-passive-type.png", width = 7, height = 6,
    units = "in", res = 300)
vcd::assoc(root_level_by_passive_type_mtx[c(2, 1),], shade = TRUE)
dev.off()

## 4. Grammaticality judgment ========

### 4.1 Load the results =======
# ratings <- read_tsv("acceptability.tsv") |>
#   rename(Email = `Email Address`,
#          ParticipantNames = `Nama Lengkap`,
#          Department = `Program Studi`,
#          Univ = `Asal Universitas`) |> 
#   mutate(ParticipantID = row_number())
# 
# ratingsLong <- ratings |>
#   pivot_longer(cols = -c(Timestamp, Email, ParticipantNames,
#                          NIM, Department, Univ, ParticipantID),
#                names_to = "Sentences",
#                values_to = "Rating") |>
#   mutate(sentenceID = str_extract(Sentences, "^\\d+"),
#          Sentences = str_replace_all(Sentences, "\\s+", " "),
#          Sentences = str_replace_all(Sentences, "tangju", "tanju"))

# ratingsLong |> 
#   select(-Email, -ParticipantNames, -NIM, -Department, -Univ) |> 
#   write_tsv("acceptability.tsv")
# ratingsLong |> 
#   select(-Email, -ParticipantNames, -NIM, -Department, -Univ) |> 
#   write_rds("acceptability.rds")


# ratingsLong <- read_tsv("acceptability.tsv")
ratingsLong <- read_rds("acceptability.rds")
verbs_rgx <- "(tanju|lempag|ampat|injak|umbas|adol)"

# readxl::read_xlsx("Daftar-Stimulus.xlsx") |> 
#   write_rds("stimulus-sentences.rds")
stims <- read_rds("stimulus-sentences.rds") |> 
  rename(sentenceID = no) |> 
  mutate(sentenceID = as.character(sentenceID))

ratingsLong <- ratingsLong |> 
  left_join(stims)

ratingsLong |> 
  filter(str_detect(clause_type, "imper")) |> 
  group_by(passive_type, clause_type) |> 
  summarise(avg = mean(Rating))

### 4.2 ANALYSIS: Imperative =====
ratingsLong |> 
  filter(str_detect(clause_type, "imper")) |> 
  group_by(passive_type, clause_type, verb) |> 
  summarise(avg = mean(Rating)) |> 
  arrange(verb)

ratingsLong |> 
  filter(str_detect(clause_type, "imper")) |> 
  group_by(passive_type, clause_type) |> 
  summarise(avg = mean(Rating))

ratingsLong |> 
  filter(str_detect(clause_type, "imper")) |> 
  group_by(passive_type) |> 
  summarise(avg = mean(Rating))

# colour blind colours

palette.colors(palette = "Okabe-Ito")

#### 4.2.1 violin plot =====
ratingsLong |> 
  filter(str_detect(clause_type, "imper")) |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-"))) |> 
  ggplot(aes(x = passive_type, y = Rating)) +
  geom_violin(aes(fill = passive_type)) +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .5) +
  theme_light() +
  labs(x = "Passive type",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12))
ggsave(filename = "figs/04-2-violin-plot-for-IMPERATIVE.png", 
       height = 7, 
       width = 7, 
       units = "in",
       dpi = 300
       )
#### 4.2.2 T-test =====
rating_imper <- ratingsLong |> 
  filter(str_detect(clause_type, "imper"))
shapiro.test(rating_imper[rating_imper$passive_type == "a", ]$Rating)
shapiro.test(rating_imper[rating_imper$passive_type == "ka", ]$Rating)

rating_imper |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE,
                    alternative = "greater"))() # alternative greater because we theoretically expect higher acceptability for -a than ka- with imperative structure

rating_imper |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE,
                    alternative = "greater"))() |> 
  (\(x) x$p.value < 0.0001)()

# standard deviation
## -a
sd(rating_imper[rating_imper$passive_type == "a", ]$Rating)
## ka-
sd(rating_imper[rating_imper$passive_type == "ka", ]$Rating)

# IQR
## -a
IQR(rating_imper[rating_imper$passive_type == "a", ]$Rating)
## ka-
IQR(rating_imper[rating_imper$passive_type == "ka", ]$Rating)


### 4.3 ANALYSIS: Purposive =====
ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  group_by(passive_type, clause_type, verb) |> 
  summarise(avg = mean(Rating)) |> 
  arrange(verb)

ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  group_by(passive_type, clause_type) |> 
  summarise(avg = mean(Rating))

a_subordinate <- ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  select(passive_type, clause_type, Rating) |> 
  filter(passive_type == "a")
summary(a_subordinate$Rating)
sd(a_subordinate$Rating)
IQR(a_subordinate$Rating)

ka_subordinate <- ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  select(passive_type, clause_type, Rating) |> 
  filter(passive_type == "ka")
summary(ka_subordinate$Rating)
sd(ka_subordinate$Rating)
IQR(ka_subordinate$Rating)

# boxplot(a_subordinate$Rating, ka_subordinate$Rating, names = c("a", "ka"))
# t.test(a_subordinate$Rating, ka_subordinate$Rating)

#### 4.3.1 violin plot ====
ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-"))) |> 
  ggplot(aes(x = passive_type, y = Rating)) +
  geom_violin(aes(fill = passive_type)) +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .5) +
  theme_light() +
  labs(x = "Passive type",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12))
ggsave(filename = "figs/04-3-violin-plot-for-SUBORDINATION-PURPOSIVE.png", 
       height = 7, 
       width = 7, 
       units = "in",
       dpi = 300
)
#### 4.3.2 Statistical test =====
shapiro.test(a_subordinate$Rating)
shapiro.test(ka_subordinate$Rating)
ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))()
# Wilcoxon rank sum test
# 
# data:  Rating by passive_type
# W = 25978, p-value = 2.109e-14
# alternative hypothesis: true location shift is not equal to 0
ratingsLong |> 
  filter(str_detect(clause_type, "sub")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))() |> 
  (\(x) x$p.value < 0.0001)()


### 4.4 ANALYSIS: Adverb of duration =====
ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak")) |> 
  group_by(passive_type, verb) |> 
  summarise(avg = mean(Rating)) |> 
  arrange(verb)

ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak")) |> 
  group_by(passive_type, adverb_phrase) |> 
  summarise(avg = mean(Rating))

a_adverb <- ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak"), passive_type == "a") |> 
  select(passive_type, Rating, adverb_phrase)

ka_adverb <- ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak"), passive_type == "ka") |> 
  select(passive_type, Rating, adverb_phrase)

boxplot(a_adverb$Rating, ka_adverb$Rating, names = c("a", "ka"))

#### 4.4.1 Violin plot ====

ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak")) |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-"))) |> 
  ggplot(aes(x = passive_type, y = Rating)) +
  geom_violin(aes(fill = passive_type)) +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .5) +
  theme_light() +
  labs(x = "Passive type",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12))
ggsave(filename = "figs/04-4-violin-plot-for-ADVERB-OF-DURATION.png", 
       height = 7, 
       width = 7, 
       units = "in",
       dpi = 300
)

shapiro.test(a_adverb$Rating)
shapiro.test(ka_adverb$Rating)

ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))()

ratingsLong |> 
  filter(str_detect(adverb_phrase, "mak")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))() |> 
  (\(x) x$p.value < 0.0001)()

summary(a_adverb$Rating)
summary(ka_adverb$Rating)

sd(a_adverb$Rating)
sd(ka_adverb$Rating)

IQR(a_adverb$Rating)
IQR(ka_adverb$Rating)

### 4.5 ANALYSIS: High- and Low-level =====

ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "h", verb_speech_level)) |> 
  group_by(verb, passive_type, verb_speech_level) |> 
  summarise(avg = mean(Rating)) |> 
  arrange(verb_speech_level, verb, passive_type)

ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "h", verb_speech_level)) |> 
  select(verb_speech_level, passive_type, Rating) |> 
  filter(verb_speech_level == "l") |>
  (\(x) {boxplot(Rating ~ passive_type, data = x, col = c("#3FA0FF", "#FFE099"))})()

low_root <- ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "h", verb_speech_level)) |> 
  select(verb_speech_level, passive_type, Rating) |> 
  filter(verb_speech_level == "l")

ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "h", verb_speech_level)) |> 
  select(verb_speech_level, passive_type, Rating) |> 
  filter(verb_speech_level == "h") |>
  (\(x) {boxplot(Rating ~ passive_type, data = x, col = c("#3FA0FF", "#FFE099"))})()

high_root <- ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "h", verb_speech_level)) |> 
  select(verb_speech_level, passive_type, Rating) |> 
  filter(verb_speech_level == "h")

high_low <- high_root |> 
  bind_rows(low_root) |> 
  mutate(verb_speech_level = factor(verb_speech_level, levels = c("h", "l")))

compare_means(Rating ~ passive_type, data = high_low, method = "wilcox.test", group.by = "verb_speech_level")
# # A tibble: 2 × 9
# verb_speech_level .y.    group1 group2        p    p.adj p.format p.signif method  
# <fct>             <chr>  <chr>  <chr>     <dbl>    <dbl> <chr>    <chr>    <chr>   
#   1 h                 Rating a      ka     2.92e- 5 2.90e- 5 2.9e-05  ****     Wilcoxon
# 2 l                 Rating a      ka     4.07e-54 8.10e-54 < 2e-16  ****     Wilcoxon


#### 4.5.1 violin plot ====

ratingsLong |> 
  mutate(verb_speech_level = if_else(is.na(verb_speech_level), "High speech level", verb_speech_level),
         verb_speech_level = replace(verb_speech_level, verb_speech_level == "l", "Low speech level")) |>
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-"))) |> 
  ggplot(aes(x = passive_type, y = Rating)) +
  geom_violin(aes(fill = passive_type)) +
  facet_wrap(~verb_speech_level, scales = "free_x") +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .2) +
  theme_light() +
  labs(x = "Passive type",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12),
        strip.text.x.top = element_text(size = 14))
ggsave(filename = "figs/04-5-violin-plot-for-ROOT-SPEECH-LEVEL.png", 
       height = 7, 
       width = 7, 
       units = "in",
       dpi = 300
)

# t.test(filter(low_root, passive_type == "a")$Rating, filter(low_root, passive_type == "ka")$Rating)

# t.test(filter(high_root, passive_type == "a")$Rating, filter(high_root, passive_type == "ka")$Rating)


### 4.6 ANALYSIS: Adverb of Intention ("Nyelap") =====

ratingsLong |> 
  filter(adverb_phrase == "nyelap") |> 
  group_by(passive_type) |> 
  summarise(avg = mean(Rating), sd = sd(Rating), median = median(Rating), iqr = IQR(Rating))

# wilcox.test(filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "a")$Rating,
#             filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "ka")$Rating)
# 
# t.test(filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "a")$Rating,
#        filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "ka")$Rating)
# 
# kruskal.test(filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "a")$Rating,
#             filter(ratingsLong, adverb_phrase == "nyelap", passive_type == "ka")$Rating)

ratingsLong |> 
  filter(str_detect(adverb_phrase, "^nyelap")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))()
ratingsLong |> 
  filter(str_detect(adverb_phrase, "^nyelap")) |> 
  (\(x) wilcox.test(Rating ~ passive_type, data = x, correct = FALSE))() |> 
  (\(x) x$p.value < 0.0001)()

#### 4.6.1 violin plot ====
ratingsLong |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-"))) |> 
  ggplot(aes(x = passive_type, y = Rating)) +
  geom_violin(aes(fill = passive_type)) +
  # facet_wrap(~verb_speech_level, scales = "free_x") +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .2) +
  theme_light() +
  labs(x = "Passive type",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = "none") +
  theme(axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12),
        strip.text.x.top = element_text(size = 14))
ggsave(filename = "figs/04-6-violin-plot-for-ADVERB-OF-INTENTION.png", 
       height = 7, 
       width = 7, 
       units = "in",
       dpi = 300
)



### 4.6 ANALYSIS: Contrastive coordination with "kewala" =====

#### summary statistics 
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  (\(x) summary(x$Rating))()
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "tanjung") |> 
  (\(x) summary(x$Rating))()
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "lempag") |> 
  (\(x) summary(x$Rating))()

#### test for normality
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  (\(x) shapiro.test(x$Rating))()
# Shapiro-Wilk normality test
# 
# data:  x$Rating
# W = 0.87191, p-value = 1.297e-11
# NOTE: the distribution of the rating is significantly different from the expected normal distribution (i.e., the rating is skewed)
# NOTE: visualise the data to check the skewness of the rating
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "tanjung") |> 
  (\(x) shapiro.test(x$Rating))()
# Shapiro-Wilk normality test
# 
# data:  x$Rating
# W = 0.86577, p-value = 8.64e-08
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "tanjung") |> 
  (\(x) shapiro.test(x$Rating))() |> 
  (\(x) x$p.value < 0.0001)()
# [1] TRUE
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "lempag") |> 
  (\(x) shapiro.test(x$Rating))()
# Shapiro-Wilk normality test
# 
# data:  x$Rating
# W = 0.87764, p-value = 2.562e-07
ratingsLong |> 
  filter(str_detect(Sentences, "wala"), verb == "lempag") |> 
  (\(x) shapiro.test(x$Rating))() |> 
  (\(x) x$p.value < 0.0001)()
# [1] TRUE

#### compare the means for the two verbs in the coordinated structure
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  (\(x) compare_means(Rating ~ verb, data = x))()
# # A tibble: 1 × 8
# .y.    group1  group2     p p.adj p.format p.signif method  
# <chr>  <chr>   <chr>  <dbl> <dbl> <chr>    <chr>    <chr>   
#   1 Rating tanjung lempag 0.566  0.57 0.57     ns       Wilcoxon
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  (\(x) compare_means(Rating ~ verb, data = x, method = "kruskal.test"))()
# # A tibble: 1 × 6
# .y.        p p.adj p.format p.signif method        
# <chr>  <dbl> <dbl> <chr>    <chr>    <chr>         
#   1 Rating 0.565  0.57 0.57     ns       Kruskal-Wallis
# ratingsLong |> 
#   filter(str_detect(Sentences, "wala")) |> 
#   (\(x) kruskal.test(Rating ~ verb, data = x))()
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  (\(x) wilcox.test(Rating ~ verb, data = x, correct = FALSE))()




#### 4.7.1 violin plot ==== 
ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-")),
         verb = if_else(verb == "lempag", "*lempag* 'hit/smash'", "*tanjung* 'kick'"),
         verb = factor(verb)) |> 
  ggplot(aes(x = verb, y = Rating)) +
  geom_violin(aes(fill = verb)) +
  # facet_wrap(~verb_speech_level, scales = "free_x") +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = .2, 
               colour = "red") +
  stat_summary(fun = "median",
               geom = "crossbar",
               colour = "black") +
  # scale_fill_manual(values = c("#009E73", "#F0E442")) +
  geom_jitter(width = .2, alpha = .2) +
  theme_light() +
  labs(x = "Verb",
       caption = "The black lines are the median rating.\nThe red lines are the mean rating.") +
  guides(fill = "none") +
  theme(axis.text.x = ggtext::element_markdown(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12),
        strip.text.x.top = element_text(size = 14))
ggsave(filename = "figs/04-7-violin-plot-for-CONTRASTIVE-COORDINATION.png", 
       height = 7.5, 
       width = 7, 
       units = "in",
       dpi = 300
)

#### 4.7.2 histogram =====
rating_contrastive <- ratingsLong |> 
  filter(str_detect(Sentences, "wala")) |> 
  mutate(passive_type = if_else(passive_type == "a", "-a", "ka-"),
         passive_type = factor(passive_type, levels = c("-a", "ka-")),
         verb = if_else(verb == "lempag", "*lempag* 'hit/smash'", "*tanjung* 'kick'"),
         verb = factor(verb))
rating_contrastive |> 
  ggplot(aes(x = Rating, fill = verb)) + 
  # geom_histogram(binwidth = 1, alpha = .9) + 
  geom_density(alpha = .3) +
  # facet_wrap(~verb) +
  labs(y = "Density",
       x = "Rating score") +
  theme_bw() +
  theme(axis.text.x = ggtext::element_markdown(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 12),
        legend.text = ggtext::element_markdown(size = 12)
        # strip.text.x.top = ggtext::element_markdown(size = 14)
        )
ggsave(filename = "figs/04-8-density-plot-for-CONTRASTIVE-COORDINATION.png", 
       height = 4.75, 
       width = 7.5, 
       units = "in",
       dpi = 300
)


# prep high level ========
# prep_high <- c("dening", "antuk", "ring", "sareng")
# prep_netral <- c("baan", "olih")
# prep_low <- c("teken", "ajak")
# 
# df %>%
# filter(agent_eksplisit == "y") %>%
# filter(agent_phrase != "np") %>%
# mutate(pp_level = "high",
# pp_level = replace(pp_level, agent_phrase %in% prep_netral, "neutral"),
# pp_level = replace(pp_level, agent_phrase %in% prep_low, "low")) %>%
# count(`Tipe pasif`, pp_level) %>% arrange(`Tipe pasif`, desc(n)) %>%
# filter(`Tipe pasif` %in% c("ka", "a"))
# 
# a<-c(low=37, neutral=2, high=0)
# ka<-c(low=20, neutral=12, high=14)
# mtx <- rbind(a, ka)
# chisq.test(mtx)
# chisq.test(mtx)$stdres
# vcd::assoc(mtx, shade = TRUE)
