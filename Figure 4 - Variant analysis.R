library(tidyverse)
library(outbreakinfo)
library(lubridate)
library(extrafont)
library(zoo)
extrafont::loadfonts()
file_dir = "~/GitHub/outbreak-resources-paper/figures/"

# Define VOCs -------------------------------------------------------------
alpha_lineages = lookupSublineages("alpha", FALSE)
alpha_lineages_string = lookupSublineages("alpha", TRUE)

beta_lineages = lookupSublineages("beta", FALSE)
beta_lineages_string = lookupSublineages("beta", TRUE)

gamma_lineages = lookupSublineages("gamma", FALSE)
gamma_lineages_string = lookupSublineages("gamma", TRUE)

delta_lineages = lookupSublineages("delta", FALSE)
delta_lineages_string = lookupSublineages("delta", TRUE)

omicron_lineages = lookupSublineages("omicron", FALSE)
omicron_lineages_string = lookupSublineages("omicron", TRUE)

epsilon_lineages = lookupSublineages("epsilon", FALSE)
iota_lineages = lookupSublineages("iota", FALSE)

voc_names = c("alpha", "beta", "gamma", "delta", "zeta", "epsilon", "eta", "theta", "iota",  "kappa", "lambda", "mu", "omicron")  

all_queries = c(voc_names, alpha_lineages, beta_lineages, gamma_lineages, delta_lineages, omicron_lineages)

# Get overall number of entries by term
getQueryTotal = function(query) {
  response = getResourcesResponse(query, size = 0, facets="")
  # response = getResourcesResponse(paste0('"', query, '"'), size = 0, facets="")
  df = tribble(~query, ~total,
               query, response$total)
  return(df)
}

total_counts_by_term = map_df(all_queries, getQueryTotal)

total_counts_by_term %>% arrange(desc(total))  %>% View()



# get all resources over times --------------------------------------------
# quick wrapper function to save the VOC name.
getVOCResources = function(query, fields = c("@type", "date")) {
  print(paste0("Querying ", query))
  df = getResourcesData(query, fields = fields, fetchAll = TRUE)
  df = df %>% 
    mutate(variant = query,
           iso_week_date = floor_date(date, unit="week")) %>% 
    filter(date > "2020-02-01", date <= Sys.Date())
  return(df)
}


all_resources = getVOCResources(query="__all__", fields = c("@type", "date", "topicCategory", "keywords", "name", "description", "measurementTechnique"))

all_resources_by_week = all_resources %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  rename(total = n)


all_resources_by_week_type = all_resources %>% 
  group_by(iso_week_date, type=`@type`) %>%
  count() %>% 
  rename(total = n)

topics_all = lapply(all_resources %>% pull(topicCategory), unlist) %>% unlist() %>% as_tibble() %>% count(value)

ggplot(all_resources_by_week, aes(x = iso_week_date, y = total)) +
  geom_col()

# Overall trends in variant searches --------------------------------------
# Date where the variant was declared a VOC by the WHO
voc_who_dates = tribble(~variant, ~date,
                        "Alpha & Beta", as_date("2020-12-18"),
                        "Gamma", as_date("2021-02-01"),
                        "Delta", as_date("2021-05-11"),
                        "Omicron", as_date("2021-11-26")
)

variant_results = getVOCResources("variant OR lineage", 
                                  fields = c("@type", "date", "topicCategory", "keywords", "name", "description", "measurementTechnique"))

variant_by_week = variant_results %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  left_join(all_resources_by_week, by = "iso_week_date") %>% 
  mutate(pct_resources = n / total)



variant_by_week_type = variant_results %>% 
  group_by(iso_week_date, type=`@type`) %>%
  count() %>% 
  left_join(all_resources_by_week_type, by = c("type", "iso_week_date")) %>% 
  mutate(pct_resources = n / total)


ggplot(variant_by_week_type, aes(x = iso_week_date, y = pct_resources, colour = type)) +
  geom_line() +
  facet_wrap(~type, scales="free_y")

# Longitudinal trace
ggplot(variant_by_week, aes(x = iso_week_date, y = pct_resources)) +
  geom_vline(aes(xintercept = date), linetype = 2, data = voc_who_dates, colour = "red") +
  geom_text(aes(x = date, y = 0.085, label = variant),  data = voc_who_dates, colour = "red", hjust = 0, nudge_x = 10, size = 6, family = "DM Sans") +
  geom_line(size=1.25) +
  xlab("date updated") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Research on variants increases as VOCs become the dominant form of SARS-CoV-2", subtitle = "Proportion of resources in the outbreak.info Research Library concerning variants or lineages") +
  theme_minimal() +
  theme(text = element_text(family = "DM Sans", size = 20),
        axis.title.y = element_blank())

ggsave(paste0(file_dir, "Figure4c.pdf"), device = "pdf", width = 7, height = 5)


# By type
variant_by_type = variant_results %>% count(type = `@type`) %>% arrange(n)
variant_by_type$type = factor(variant_by_type$type, variant_by_type$type)

COLORPALETTE = c("#79706E", "#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D")


ggplot(variant_by_type, aes(y = type, x = n, fill = type, label = scales::comma(n))) + 
  geom_col() +
  geom_text(family = "DM Sans", hjust = 0, nudge_x = 100) + 
  ggtitle("Variant resources in outbreak.info Research Library by type") +
  scale_x_continuous(name = "Number of resources", labels = scales::comma) +
  scale_fill_manual(values = COLORPALETTE) +
  theme_minimal() +
  theme(text = element_text(family = "DM Sans", size = 20),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave(paste0(file_dir, "Figure4a.pdf"), device = "pdf", width = 9, height = 5)


keywords = lapply(variant_results %>% pull(keywords), unlist) %>% unlist() %>% as_tibble() %>% count(value)
topics = lapply(variant_results %>% pull(topicCategory), unlist) %>% unlist() %>% as_tibble() %>% count(value)

# Resources by VOC --------------------------------------------------------

# Get resources by Variant of Concern (VOC)
resources_queries = c('"alpha" OR "B.1.1.7" OR "Q.2"', 
                      '"beta" OR "B.1.351"', 
                      '"gamma" OR "P.1" OR "P.1.2"', 
                      '"delta" OR "B.1.617.2" OR "AY.1" OR "AY.2" OR "AY.3" OR "AY.4" OR "AY.4.2"', 
                      '"zeta" OR "P.2"', 
                      '"epsilon" OR "B.1.427" OR "B.1.429"', 
                      '"eta" OR "B.1.525"', 
                      '"theta" OR "P.3"', '"iota" OR "B.1.526"',  
                      '"kappa" OR "B.1.617.1"', 
                      '"lambda" OR "C.37"', 
                      '"mu" OR "B.1.621"', 
                      '"omicron" OR "B.1.1.529" OR "BA.1" OR "BA.1.1" OR "BA.2" OR "BA.2.12.1" OR "BA.2.75" OR "BA.3" OR "BA.4" OR "BA.5"')

shim = function(name) {
  return(paste0("name:*", name, "*"))
}


delta_lineages = c("B.1.617.2","AY.1", "AY.2", "AY.3", "AY.4", "AY.4.2")
omicron_lineages = c("B.1.1.529","BA.1","BA.1.1","BA.2","BA.2.12.1","BA.2.75","BA.3","BA.4","BA.5")
resources_queries = c(paste(c(shim("alpha"), lapply(alpha_lineages, shim)), collapse=" OR "),
                      paste(c(shim("beta"), lapply(beta_lineages, shim)), collapse=" OR "),
                      paste(c(shim("gamma"), lapply(gamma_lineages, shim)), collapse=" OR "),
                      # paste(c(shim("epsilon"), lapply(epsilon_lineages, shim)), collapse=" OR "),
                      # paste(c(shim("iota"), lapply(iota_lineages, shim)), collapse=" OR "),
                      paste(c(shim("delta"), lapply(delta_lineages, shim)), collapse=" OR "),
                      paste(c(shim("omicron"), lapply(omicron_lineages, shim)), collapse=" OR "))



resources_by_voc = map_df(resources_queries, function(x) getVOCResources(x, c("@type", "date", "name", "description")))


resources_by_voc_roll = resources_by_voc %>%
  group_by(variant, date) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(variant) %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(
    count_7day = zoo::rollmean(n, k = 7, fill = NA)
  )


resources_voc_by_week = resources_by_voc %>%
  group_by(variant, iso_week_date) %>%
  count() %>% 
  mutate(variant = case_when(variant == "name:*alpha* OR name:*B.1.1.7* OR name:*Q.1* OR name:*Q.2* OR name:*Q.3* OR name:*Q.4* OR name:*Q.5* OR name:*Q.6* OR name:*Q.7* OR name:*Q.8*" ~ "alpha", 
                             variant == "name:*beta* OR name:*B.1.351* OR name:*B.1.351.1* OR name:*B.1.351.2* OR name:*B.1.351.3* OR name:*B.1.351.4* OR name:*B.1.351.5*" ~ "beta", 
                             variant == "name:*gamma* OR name:*P.1* OR name:*P.1.1* OR name:*P.1.2* OR name:*P.1.3* OR name:*P.1.4* OR name:*P.1.5* OR name:*P.1.6* OR name:*P.1.7* OR name:*P.1.7.1* OR name:*P.1.8* OR name:*P.1.9* OR name:*P.1.10* OR name:*P.1.10.1* OR name:*P.1.10.2* OR name:*P.1.11* OR name:*P.1.12* OR name:*P.1.12.1* OR name:*P.1.13* OR name:*P.1.14* OR name:*P.1.15* OR name:*P.1.16* OR name:*P.1.17* OR name:*P.1.17.1*" ~ "gamma", 
                             variant == "name:*delta* OR name:*B.1.617.2* OR name:*AY.1* OR name:*AY.2* OR name:*AY.3* OR name:*AY.4* OR name:*AY.4.2*" ~ "delta", 
                             variant == "name:*omicron* OR name:*B.1.1.529* OR name:*BA.1* OR name:*BA.1.1* OR name:*BA.2* OR name:*BA.2.12.1* OR name:*BA.2.75* OR name:*BA.3* OR name:*BA.4* OR name:*BA.5*" ~ "omicron", 
                             variant == "Querying name:*epsilon* OR name:*B.1.427* OR name:*B.1.429* OR name:*B.1.429.1*" ~ "epsilon", 
                             variant == " name:*iota* OR name:*B.1.526* OR name:*B.1.526.1* OR name:*B.1.526.2* OR name:*B.1.526.3*" ~ "iota", 
                             TRUE ~ "other")
  )


# VOC prevalence by date --------------------------------------------------
getVOCPrevalence = function(query) {
  query_string = lookupSublineages(query, TRUE)
  df = getPrevalence(query_string)
  df = df %>% mutate(variant = query)
}


voc_prevalence = map_df(c("alpha", "beta", "gamma", "delta", "omicron", "epsilon", "iota"), getVOCPrevalence)

ggplot(voc_prevalence, aes(x = date, y = proportion, colour = variant)) + geom_line()
ggplot(resources_by_voc_roll, aes(x = date, y = count_7day)) + geom_line()

# quick plot --------------------------------------------------------------
ggplot(resources_voc_by_week, aes(x = iso_week_date, y = n, fill = variant)) +
  geom_col() + 
  geom_line(aes(x = date, y = proportion * 40), data = voc_prevalence) +
  facet_wrap(~variant) +
  theme_minimal() + 
  theme(legend.position = "none")

merged_voc = resources_voc_by_week %>% left_join(voc_prevalence, by = c("variant" = "variant", "iso_week_date" = "date"))
merged_voc = merged_voc %>% arrange(iso_week_date)

ggplot(merged_voc, aes(x = n, y = proportion, fill = iso_week_date)) +
  # geom_line()+
  geom_point() + 
  facet_wrap(~variant) +
  theme_minimal() + 
  theme(legend.position = "none")


ggplot(resources_voc_by_week, aes(x = iso_week_date, y = n, fill = variant)) +
  geom_col() + 
  geom_line(aes(x = date, y = proportion * 40), data = voc_prevalence) +
  facet_wrap(~variant) +
  theme_minimal() + 
  theme(legend.position = "none")

# looking at topics over time ---------------------------------------------
treatment = getVOCResources("topicCategory:Treatment") %>% mutate(topicCategory = "Treatment", popularVariantTopic = T)
prev = getVOCResources('topicCategory:"Individual Prevention"')%>% mutate(topicCategory = "Individual Prevention", popularVariantTopic = F)
epi = getVOCResources('topicCategory:"Epidemiology"') %>% mutate(topicCategory = "Epidemiology", popularVariantTopic = F)
mepi = getVOCResources('topicCategory:"Molecular Epidemiology"') %>% mutate(topicCategory = "Molecular Epidemiology", popularVariantTopic = T)
cepi = getVOCResources('topicCategory:"Classical Epidemiology"') %>% mutate(topicCategory = "Classical Epidemiology", popularVariantTopic = F)
pubhealth = getVOCResources('topicCategory:"Public Health Interventions"') %>% mutate(topicCategory = "Public Health Interventions", popularVariantTopic = F)

immuno = getVOCResources('topicCategory:"Immunological Response"') %>% mutate(topicCategory = "Immunological Response", popularVariantTopic = T)
vax = getVOCResources('topicCategory:"Vaccines"') %>% mutate(topicCategory="Vaccines", popularVariantTopic = T)
mech = getVOCResources('topicCategory:"Mechanism"') %>% mutate(topicCategory="Mechanism", popularVariantTopic = T)

topics_time = bind_rows(treatment, prev, epi, mepi, cepi, pubhealth, immuno, vax, mech) %>% 
  group_by(iso_week_date, topicCategory, popularVariantTopic) %>% 
  count()

ggplot(topics_time, aes(x = iso_week_date, y = n, fill = popularVariantTopic)) + 
  geom_vline(xintercept = as.Date("2020-12-18")) + geom_col() +
  facet_wrap(~topicCategory)

# country inequity --------------------------------------------------------
library(tidytext)
library(countrycode)

desc = variant_results %>% select(description) %>%  unnest_tokens(word, description)
name = variant_results %>% select(name) %>%  unnest_tokens(word, name)

country_dict = c(countrycode::countryname_dict$country.name.en,countrycode::countryname_dict$country.name.alt) %>% unique()

variant_text = bind_rows(desc, name)

# ngrams,  by variant -----------------------------------------------------
resources_by_voc = resources_by_voc %>% mutate(name_desc = paste(name, description, sep = " "))

ngrams_by_voc = resources_by_voc %>% select(variant, name_desc) %>%  
  unnest_tokens(ngram, name_desc, token = "ngrams", n = 2)

single_words_by_voc = resources_by_voc %>% select(variant, name_desc) %>%  
  unnest_tokens(word, name_desc, token = "regex", pattern="\\s") %>% 
  filter(!word %in% tidytext::stop_words$word)

single_words_by_voc %>% group_by(variant) %>% count(word) %>% arrange(desc(n)) %>% slice(1:25) %>% View()


# treatment ---------------------------------------------------------------
treatment = getVOCResources("topicCategory:Treatment", c("date", "@type", "name", "description")) %>% mutate(topicCategory = "Treatment", popularVariantTopic = T)
treatment = treatment %>% mutate(name_desc = paste(name, description, sep = " "),
                                 is_before_alpha = date < "2020-12-18")

treatment_words = treatment %>% select(is_before_alpha, name_desc) %>%  
  unnest_tokens(word, name_desc, token = "regex", pattern="\\s") %>% 
  filter(!word %in% tidytext::stop_words$word)

treatment_words %>% bind_tf_idf(word, is_before_alpha, n)
treatment_words = treatment_words %>%  
  group_by(is_before_alpha) %>% 
  count(word) %>% 
  mutate(total = sum(n),
         rank = row_number(), 
         tf = n/total) %>% arrange(desc(tf))


treatment_words %>% 
  select(word, tf, is_before_alpha) %>%
  spread(is_before_alpha, tf) %>% 
  rename(pre_alpha = `TRUE`, post_alpha = `FALSE`) %>% 
  filter(!is.na(pre_alpha), !is.na(post_alpha)) %>% 
  mutate(fc = log10(post_alpha / pre_alpha)) %>% arrange(desc(fc)) %>% View()
