# Creates a plot of resources in outbreak.info, broken down by resource type and source location.
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



# Resources by type donut chart
COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                 "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")

names(COLORPALETTE) = resources_by_type_grouped %>% arrange(desc(count)) %>% pull(type_grouped)

resources_by_type_grouped = resources_by_type %>% 
  mutate(type_grouped = ifelse(term %in% c("Publication", "ClinicalTrial", "Dataset", "Protocol", "SoftwareSourceCode", "Analysis"),
                               term, "Other")) %>% 
  group_by(type_grouped) %>% 
  summarise(count = sum(count))

resources_by_type_grouped$type_grouped = factor(resources_by_type_grouped$type_grouped, resources_by_type_grouped %>% pull(type_grouped) %>% rev())

ggplot(resources_by_type_grouped, aes(x = 3, y = count, fill = type_grouped)) + 
  geom_col() +
  coord_polar(theta="y") + 
  xlim(c(1, 4)) +
  theme_void() +
  scale_fill_manual(values = COLORPALETTE) +
  theme(legend.position = "bottom", text = element_text(family = "DM Sans", size = 22))

ggsave(filename = "~/GitHub/outbreak-resources-paper/figures/resources_by_type.svg", device = "svg", width = width, height = height)



  # bargraph of resources by type/source ------------------------------------

COLORPALETTE = c("#4E79A7", "#f28e2b", "#59a14f","#e15759", "#499894","#B6992D",  "#D37295", "#B07AA1","#9D7660", "#bcbd22",
                 "#aecBe8", "#FFBE7D",  "#8CD17D", "#FF9D9A",  "#86BCB6", "#F1CE63","#FABFD2",  "#D4A6C8", "#D7B5A6",  "#79706E")

names(COLORPALETTE) = resources_by_curator

ggplot(resources_by_source_grouped, aes(x = term, y = count, colour = term, fill = term)) + 
  geom_col() +
  scale_fill_manual(values = COLORPALETTE) +
  scale_colour_manual(values = COLORPALETTE) +
  scale_y_continuous(labels=scales::comma) +
  facet_wrap(~type_grouped, scales = "free_x") +
  geom_text(aes(y = count + 50, label = scales::comma(count, accuracy = 1)),  hjust = 0, vjust = 0.5, family = "DM Sans") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(), text = element_text(family = "DM Sans", size = 22))

ggsave(filename = "~/GitHub/outbreak-resources-paper/figures/resources_by_source.svg", device = "svg", width = width, height = height)
