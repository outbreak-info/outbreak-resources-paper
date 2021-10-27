# query the API to get the resources by date. Filter to > 1 January 2020, to remove the weirdos that slipped through.
resources_by_date = getResourcesData(query = "date:[2020-01-01 TO *]", fields = "date", fetchAll = TRUE)

# roll up the number of resources by week
resources_by_date = resources_by_date %>%
  mutate(year = lubridate::year(date),
         iso_week = lubridate::isoweek(date))

# count the number of new resources per week.
resources_per_week = resources_by_date %>%
  count(iso_week, year) %>%
  # convert from iso week back to a date
  mutate(iso_date = lubridate::parse_date_time(paste(year,iso_week, "Mon", sep="-"), "Y-W-a"))

# plot!
fill_color = "#66c2a5"
ggplot(resources_per_week, aes(x = iso_date, y = n)) +
  geom_bar(fill = fill_color, stat="identity") +
  ggtitle("COVID-19 resources have rapidly proliferated", subtitle="Number of publications, datasets, clinical trials, and more added each week to outbreak.info's Research Library") +
  theme_minimal() +
  theme(
    text = element_text(family="DM Sans"),
    axis.title = element_blank(),
    axis.text = element_text(size = 16),
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(colour="#777777", size=9)
  ) +
  scale_y_continuous(label=scales::comma) +
  scale_x_datetime(limits = c(min(resources_per_week$iso_date, na.rm = T), max(resources_per_week$iso_date, na.rm = T)), date_labels = "%b %Y")