# Creates a plot of resources in outbreak.info, broken down by resourece type and source location.
library(tidyverse)
library(outbreakinfo)
library(forcats)

# Set some constants
width = 14
height = 10

# Get the resources by type, to order the totals
resources_by_type_response = getResourcesData(facets="@type", facet_size = 50, size = 0)
# The results are stored in `resources_by_type_response$facets$`@type`$terms`:
resources_by_type = resources_by_type_response$facets$`@type`$terms


# Get the resources by source, to order the totals
resources_by_curator =  getResourcesData(facets="curatedBy.name", facet_size = 50, size = 0)
resources_by_curator = resources_by_curator$facets$curatedBy.name$terms$term

# Get resource count by type / source
getSources = function(type) {
  facets = getResourcesData(types=c(type), facets="curatedBy.name", facet_size = 50, size = 0)
  
  terms = facets$facets$curatedBy.name$terms %>% 
    mutate(type = type)
}

# Could be done with a direct API call, but this is an easier way to access the data.  The API call returns a heavily nested JSON structure, which is slightly annoying to parse.
resources_by_source = map_df(resources_by_type %>% pull(term), getSources)


# Group together fields into "other" category
resources_by_source_grouped = resources_by_source %>% 
  mutate(type_grouped = ifelse(type %in% c("Publication", "ClinicalTrial", "Dataset", "Protocol", "SoftwareSourceCode", "Analysis"),
                               type, "Other")) %>% 
  group_by(type_grouped, term) %>% 
  summarise(count = sum(count))


# Set the levels, so the bar plot sorts by order.
types = c("Other", resources_by_type %>% pull(term) %>% rev())

resources_by_source_grouped$type_grouped = factor(resources_by_source_grouped$type_grouped, types)
resources_by_source_grouped$term = factor(resources_by_source_grouped$term, resources_by_curator %>% rev())

COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                 "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")

names(COLORPALETTE) = resources_by_curator


# Plot the results -- full scale to encapsulate the LitCovid responses
ggplot(resources_by_source_grouped, 
       aes(x = type_grouped, y = count, fill = term, colour = term)) +
  geom_col(position = position_dodge2(width = 1, preserve = "single", padding = 0.2), width = 1) +
  geom_text(aes(y = count - 100, label = scales::comma(count, accuracy = 1)), 
            position = position_dodge2(width = 1, preserve = "single", padding = 0.2), hjust = 1, vjust = 0.5, colour = "white") +
  geom_text(aes(y = count + 1000, label = term), position = position_dodge2(width = 1, preserve = "single", padding = 0.2), hjust = 0, vjust = 0.5) +
  scale_y_continuous(labels=scales::comma, expand = c(0, 100, 0, 20000)) +
  scale_colour_manual(values = COLORPALETTE) +
  scale_fill_manual(values = COLORPALETTE) +
  coord_flip() +
  ggtitle("Resources in outbreak.info by resource type and source", subtitle = paste0("As of ", format(Sys.time(), "%d %B %Y"))) +
  theme_minimal() + 
  theme(legend.position = "none", panel.grid.major.y = element_blank(), axis.title = element_blank(), text = element_text(size = 24))


ggsave(filename = "resources_pubs.svg", device = "svg", width = width, height = height)

# Plot the results -- truncated scale without LitCovid
countMax = resources_by_source_grouped %>% filter(type_grouped != "Publication") %>% 
  pull(count) %>% max()

ggplot(resources_by_source_grouped, 
       aes(x = type_grouped, y = count, fill = term, colour = term)) +
  geom_col(position = position_dodge2(width = 1, preserve = "single", padding = 0.2), width = 1) +
  geom_text(aes(y = count - 35, label = scales::comma(count, accuracy = 1)), position = position_dodge2(width = 1, preserve = "single", padding = 0.2), 
            hjust = 1, vjust = 0.5, colour = "white") +
  geom_text(aes(y = count + 100, label = term), position = position_dodge2(width = 1, preserve = "single", padding = 0.2), hjust = 0, vjust = 0.5) +
  scale_y_continuous(labels=scales::comma, expand = c(0, 100, 0, 1000), limits = c(0, countMax)) +
  scale_colour_manual(values = COLORPALETTE) +
  scale_fill_manual(values = COLORPALETTE) +
  coord_flip() +
  ggtitle("Resources in outbreak.info by resource type and source", subtitle = paste0("As of ", format(Sys.time(), "%d %B %Y"))) +
  theme_minimal() + 
  theme(legend.position = "none", panel.grid.major.y = element_blank(), axis.title = element_blank(), text = element_text(size = 24))


ggsave(filename = "resources_all.svg", device = "svg", width = width, height = height)