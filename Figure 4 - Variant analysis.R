library(tidyverse)
library(outbreakinfo)
library(lubridate)



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


all_resources = getVOCResources(query="__all__")

all_resources_by_week = all_resources %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  rename(total = n)

ggplot(all_resources_by_week, aes(x = iso_week_date, y = total)) +
  geom_col()

# Overall trends in variant searches --------------------------------------
variant_results = getVOCResources("variant OR lineage", 
                                  fields = c("@type", "date", "topicCategory", "keywords", "name", "description", "measurementTechnique"))

variant_by_week = variant_results %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  left_join(all_resources_by_week, by = "iso_week_date") %>% 
  mutate(pct_resources = n / total)


ggplot(variant_by_week, aes(x = iso_week_date, y = pct_resources)) +
  geom_line() +
  ggtitle("Variant/Lineage resources by update date", subtitle = "Percent of total resources in outbreak.info Research Library")

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



resources_by_voc = map_df(resources_queries, getVOCResources)


resources_voc_by_week = resources_by_voc %>%
  group_by(variant, iso_week_date) %>%
  count() 
  
  
  # VOC prevalence by date --------------------------------------------------
getVOCPrevalence = function(query) {
  query_string = lookupSublineages(query, TRUE)
  df = getPrevalence(query_string)
  df = df %>% mutate(variant = query)
}


voc_prevalence = map_df(resources_queries, getVOCPrevalence)

ggplot(voc_prevalence, aes(x = date, y = proportion, colour = variant)) + geom_line()


# quick plot --------------------------------------------------------------
ggplot(resources_voc_by_date, aes(x = iso_week_date, y = n, fill = variant)) +
  geom_col() + 
  # geom_line(aes(x = date, y = proportion * 200), data = voc_prevalence) +
  facet_wrap(~variant) +
  theme_minimal() + 
  theme(legend.position = "none")
