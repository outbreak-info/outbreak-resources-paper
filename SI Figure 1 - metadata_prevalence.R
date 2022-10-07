library(tidyverse)
library(httr)
library(jsonlite)
library(RColorBrewer)
library(extrafont)


# setup -------------------------------------------------------------------
dir_save = "~/Google Drive/Manuscripts/2022 outbreak.info Research Library/Supplemental Data/"


# Calculates the prevalence of each of the metadata fields by type --------
resource_types = c("Analysis", "ClinicalTrial","ComputationalTool", "Dataset", "Protocol", "Publication")

api_url = "https://api.outbreak.info/resources/query?"
url = "https://api.outbreak.info/resources/query?size=0&q=@type:SoftwareSourceCode%20AND%20_exists_:name"


# function to grab the number where that property exists ------------------
getTotal = function(api_url, type, property) {
  print(toupper(type))
  print(property)
  if(!is.na(property)){
    url = paste0(api_url, "size=0&q=@type:", type, " AND _exists_:", property)  
  } else {
    url = paste0(api_url, "size=0&q=@type:", type)
  }
  
  resp = httr::GET(URLencode(url))
  if(resp$status_code == 200) {
    # query if the data 
    results = jsonlite::fromJSON(content(resp, as = "text"))
    
    # save the results to a dataframe
    df = tribble(
      ~property, ~type, ~count,
      property, type, results$total
    )
    
    # for the totals, drop the `property` field
    if(is.na(property)){
      df = df %>% select(-property)
    }
    return(df)
  }
  
}



# Lookup all the properties in the schema ---------------------------------
schema_url = "https://raw.githubusercontent.com/outbreak-info/outbreak.info-resources/master/yaml/outbreak.json"

schema = jsonlite::fromJSON(schema_url)
schema_properties = unique(names(schema$`@graph`$`$validation`$properties))


# Calculate the prevalence for each property,  by type --------------------
df = map_df(resource_types, function(type) map_df(schema_properties, function(prop) getTotal(api_url, type, prop)))

# merge with the totals by category
totals_by_type = map_df(resource_types, function(type) getTotal(api_url, type, NA)) 

totals_by_type = totals_by_type %>% 
  rename(total = count) %>% 
  arrange(total)

df = full_join(df, totals_by_type, by = "type")

df = df %>%
  mutate(percent = count / total)

# sort
prop_totals = df %>% group_by(property) %>% 
  summarise(avg = mean(percent, na.rm = TRUE), prop_total = sum(count)) %>% 
  # remove properties which are nested below the primary level
  filter(prop_total > 0) %>% 
  arrange(desc(avg), property)

df$property = factor(df$property, prop_totals %>% pull(property))
df$type = factor(df$type, totals_by_type %>% pull(type))


# remove properties which are nested below the primary level
df = df %>% 
  filter(property %in% (prop_totals %>% pull(property)))

# Plot as a heatmap -------------------------------------------------------
ggplot(df, aes( x = property, y = type, fill = percent)) + 
  geom_tile(colour = "white") +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlGnBu"), limits = c(0, 1), 
                       labels = scales::percent,na.value = "#DDDDDD") +
  coord_equal() + 
  ggtitle("Prevalence of metadata properties in outbreak.info's Research Library",
          subtitle = "Percentage of records containing each property") +
  theme_minimal() +
  theme(text = element_text(family = "DM Sans"),
        # panel.grid = element_blank(),
        axis.text.x.top = element_text(angle = 45, hjust = 0),
        axis.text.x.bottom = element_text(angle = -45, hjust = 0),
        axis.ticks.length.x = unit(0.1, "cm"),
        axis.ticks.x.bottom = element_line(colour = "black", size = 0.35),
        plot.margin = unit(c(5.5, 60, 5.5, 5.5), "points"),
        legend.position = "bottom")


# save the results --------------------------------------------------------
extrafont::loadfonts()

ggsave(filename = paste0(dir_save, "Supplemental Figure 2 - Metadata Prevalence.pdf"), device = "pdf", width = 10, height = 4)
ggsave(filename = paste0(dir_save, "Supplemental Figure 2 - Metadata Prevalence.png"), device = "png", width = 10, height = 4)

write_csv(df, paste0(dir_save, "Supplemental Table 1 - Metadata Prevalence.csv"))
