library(tidyverse)
library(outbreakinfo)
library(lubridate)
library(extrafont)



# setup constants ---------------------------------------------------------
extrafont::loadfonts()
file_dir = "~/GitHub/outbreak-resources-paper/R exports/"
width = 12
width_a = 9
height_a = 5
height_d = 5

# Base lookup functions ----------------------------------------------------
# Get overall number of entries by term
getQueryTotal = function(query) {
  response = getResourcesResponse(query, size = 0, facets="")
  # response = getResourcesResponse(paste0('"', query, '"'), size = 0, facets="")
  df = tribble(~query, ~total,
               query, response$total)
  return(df)
}

# quick wrapper function to save the VOC name.
getVOCResources = function(query, fields = c("@type", "date")) {
  print(paste0("Querying ", query))
  df = getResourcesData(query, fields = fields, fetchAll = TRUE)
  df = df %>% 
    mutate(query = query,
           iso_week_date = floor_date(date, unit="week")) %>% 
    filter(date > "2020-02-01", date <= Sys.Date())
  return(df)
}



# get all resources over times --------------------------------------------
all_resources = getVOCResources(query="__all__", fields = c("@type", "date", "topicCategory", "keywords", "name", "description", "measurementTechnique"))

# Group and count by week
all_resources_by_week = all_resources %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  rename(total = n)


# group by type/week and count
all_resources_by_week_type = all_resources %>% 
  group_by(iso_week_date, type=`@type`) %>%
  count() %>% 
  rename(total = n)


# Overall trends in variant searches --------------------------------------
# Date where the variant was declared a VOC by the WHO
voc_who_dates = tribble(~variant, ~date,
                        "Alpha", as_date("2020-12-18"),
                        "Beta", as_date("2020-12-18"),
                        "Gamma", as_date("2021-02-01"),
                        "Delta", as_date("2021-05-11"),
                        "Omicron", as_date("2021-11-26")
)

variant_results = getVOCResources("variant OR lineage", 
                                  fields = c("@type", "date", "topicCategory", "keywords", "name", "description", "measurementTechnique"))

variant_results = variant_results %>%   
  mutate(`@type` = case_when(
  # convert the small numbers to equivalent types
  `@type` == "SoftwareSourceCode" ~ "ComputationalTool",
  `@type` == "ScholarlyArticle" ~ "Publication",
  TRUE ~ `@type`
))

# Count by week
variant_by_week = variant_results %>% 
  group_by(iso_week_date) %>%
  count() %>% 
  left_join(all_resources_by_week, by = "iso_week_date") %>% 
  mutate(pct_resources = n / total)


# Count by week and type
variant_by_week_type = variant_results %>% 
  group_by(iso_week_date, type = `@type`) %>%
  count() %>% 
  left_join(all_resources_by_week_type, by = c("type", "iso_week_date")) %>% 
  mutate(pct_resources = n / total)


# Figure 4a: variant resources by type ------------------------------------
# By type
variant_by_type = variant_results %>% count(type = `@type`) %>% arrange(n)
variant_by_type$type = factor(variant_by_type$type, variant_by_type$type)


COLORPALETTE = list(Publication = "#4e7aa7", Dataset = "#df5959", ClinicalTrial="#b475a3", 
                    Protocol = "#5aa04d", ImageObject = "#f28e2b", PresentationDigitalDocument = "#F1CE63",
                    ComputationalTool = "#a97252", CreativeWork = "#dddddd")

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

ggsave(paste0(file_dir, "Figure4a.pdf"), device = "pdf", width = width_a, height = height_a)



# Figure 4c: Longitudinal trace -------------------------------------------
ggplot(variant_by_week, aes(x = iso_week_date, y = pct_resources)) +
  geom_vline(aes(xintercept = date), linetype = 2, data = voc_who_dates, colour = "#e15759") +
  geom_text(aes(x = date, y = 0.085, label = variant),  data = voc_who_dates, colour = "#e15759", hjust = 0, nudge_x = 10, size = 6, family = "DM Sans") +
  geom_line(size=1.25) +
  xlab("date updated") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Research on variants increases as VOCs become the dominant form of SARS-CoV-2", subtitle = "Proportion of resources in the outbreak.info Research Library concerning variants or lineages") +
  theme_minimal() +
  theme(text = element_text(family = "DM Sans", size = 20),
        axis.title.y = element_blank())

ggsave(paste0(file_dir, "Figure4c.pdf"), device = "pdf", width = 7, height = 5)


# Figure 4d: Resources by VOC --------------------------------------------------------
# Define VOCs -------------------------------------------------------------
alpha_lineages = lookupSublineages("alpha", FALSE)

beta_lineages = lookupSublineages("beta", FALSE)

gamma_lineages = lookupSublineages("gamma", FALSE)

delta_lineages = lookupSublineages("delta", FALSE)

omicron_lineages = lookupSublineages("omicron", FALSE)

# Figure our which queries have results and are worth querying
# The GET method underlying the API is URI limited to ~ 2000 characters.

# Note: some resources are indexed as keywords, hence using a wildcard search (`*term*`)
buildQuery = function(name) {
  return(paste0("name:*", name, '* OR name:"', name, '"'))
}


alpha_indiv_queries = map_df(alpha_lineages, function(x) getQueryTotal(buildQuery(x)))
beta_indiv_queries = map_df(beta_lineages, function(x) getQueryTotal(buildQuery(x)))
gamma_indiv_queries = map_df(gamma_lineages, function(x) getQueryTotal(buildQuery(x)))
delta_indiv_queries = map_df(delta_lineages, function(x) getQueryTotal(buildQuery(x)))
omicron_indiv_queries = map_df(omicron_lineages, function(x) getQueryTotal(buildQuery(x)))

extract_combine_queries = function(queries, voc_name){
  lineage_string = 
    queries %>% 
    filter(total > 0) %>% 
    pull(query) %>% 
    paste(collapse = " OR ")
  return(paste0(buildQuery(voc_name), ' OR ', lineage_string))
}

# Construct resource queries
voc_queries = list(Alpha = alpha_indiv_queries %>% extract_combine_queries("Alpha"),
                   Beta = beta_indiv_queries %>% extract_combine_queries("Beta"),
                   Gamma = gamma_indiv_queries %>% extract_combine_queries("Gamma"),
                   Delta = delta_indiv_queries %>% extract_combine_queries("Delta"),
                   Omicron = omicron_indiv_queries %>% extract_combine_queries("Omicron")
)

extract_name = function(query) {
  return(Filter(function(x) x == query, voc_queries) %>% names())
}

# Checking below the character limit...
# voc_queries %>% lapply(nchar)

# Get the resources by query
resources_by_voc = map_df(voc_queries, function(x) getVOCResources(x, c("@type", "date", "name", "description")))


resources_by_voc_roll = resources_by_voc %>%
  group_by(query, date) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(query) %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(
    count_7day = zoo::rollmean(n, k = 7, fill = NA)
  )


resources_voc_by_week = resources_by_voc %>%
  group_by(query, iso_week_date) %>%
  count() %>% 
  mutate(variant = extract_name(query)
  )


# Fetch worldwide VOC prevalence by date --------------------------------------------------
getVOCPrevalence = function(query) {
  query_string = lookupSublineages(query, TRUE)
  df = getPrevalence(query_string)
  df = df %>% mutate(variant = query)
}

# Loop over each variant to grab its prevalence
voc_prevalence = map_df(names(voc_queries), getVOCPrevalence)

# sort the variants by date
resources_voc_by_week$variant = factor(resources_voc_by_week$variant, voc_who_dates %>% arrange(date) %>% pull(variant))
voc_who_dates$variant = factor(voc_who_dates$variant, voc_who_dates %>% arrange(date) %>% pull(variant))
voc_prevalence$variant = factor(voc_prevalence$variant, voc_who_dates %>% arrange(date) %>% pull(variant))

# plot VOC research v. VOC sequencing prevalence --------------------------------------------------------------
prevalence_scalar = -50

ggplot(resources_voc_by_week, aes(x = iso_week_date, y = n, fill = variant)) +
  geom_text(aes(x = date, y = 100, label = paste0(variant, " designated\n", format(date, "%d %b %Y"))), 
            data = voc_who_dates, hjust = 0, nudge_x = 80, size = 3,
            family = "DM Sans") + 
  geom_col() + 
  geom_line(aes(x = date, y = proportion * prevalence_scalar), data = voc_prevalence) +
  geom_area(aes(x = date, y = proportion * prevalence_scalar), data = voc_prevalence, alpha = 0.4) +
  geom_vline(aes(xintercept = date), data = voc_who_dates, linetype = 2) + 
  facet_wrap(~variant, nrow = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / prevalence_scalar, labels = scales::percent), name = "number of resources") + 
  scale_fill_manual(values = list(Alpha = "#59a14f", Beta = "#e15759", Delta = "#f28e2b", Gamma = "#B07AA1", Omicron = "#4E79A7")) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        text = element_text(family = "DM Sans", size = 16))

ggsave(paste0(file_dir, "Figure 4d - Resources by VOCs over time.pdf"), height = height_d, width = width, device = "pdf")
