###### Sequences per week for percentage calculations: ######
# Calculate Sequences per week for Spike protein sequence results
spw_spike <- allvariants_v %>%
  filter(PANGOLIN_NOM != "") %>%
  filter(S_merged != "") %>%
  count(wy, name = "TotalSeq") %>%
  mutate(ym = tsibble::yearweek(wy)) # count total sequences per week and convert week to yearweek format

# Calculate Total Sequences per week
seqs_per_week <- allvariants_to %>%
  count(wy, name = "TotalSeq") %>%
  mutate(wy = yearweek(wy))

# Calculate Total Valid Sequences per week
v_seqs_per_week <- allvariants_v %>%
  count(wy, name = "TotalSeq") %>%
  mutate(wy = yearweek(wy))

###### Sequences per month for percentage calculations: ######

# Calculate Sequences per month for Spike protein sequence results
spm_spike <- allvariants_v %>%
  filter(PANGOLIN_NOM != "") %>%
  filter(S_merged != "") %>%
  count(my, name = "TotalSeq") %>%
  ungroup() %>%
  mutate(
    Date = as.Date(my),
    YearMonth = format(Date, "%Y %b")
  ) %>%
  select(-Date)  # Drop the temporary Date column if not needed

# Calculate Total Sequences per month
seqs_per_month <- allvariants_to %>%
  count(my, name = "TotalSeq")

# Calculate Total Valid Sequences per month
v_seqs_per_month <- allvariants_v %>%
  count(my, name = "TotalSeq")

###### Sequences per Originating laboratory #######

v_seqs_per_month_origin <- allvariants_v %>%
  group_by(my, Origin) %>%
  count(name = "TotalSeq") %>%
  ungroup() %>%
  mutate(my  = as.Date(paste0(my, " 01"), format="%Y %b %d")) 


# Create a bar chart per month based on Origin
spmlabto <- ggplot(v_seqs_per_month_origin, aes(x = my, y = TotalSeq, fill = Origin)) +
  geom_bar(stat = "identity") +
  labs(title = "Sekvensering av SC2 i Norge",
       x = "MC%ned",
       y = "Antall Sekvenser") +
  scale_x_date(
    breaks = "1 month",  # Show breaks every month
    labels = scales::date_format("%b %Y")  # Format as month name and year
  ) +
  scale_fill_manual(values = origin_color) +  # Set custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  # Center the title and increase its size
  theme(legend.position="right")  # Optional: Remove legend if not needed


spmlabto

# Add a slide to the presentation with Title and Content layout and Office Theme master
export_graph <- add_slide(export_graph, layout = "Title and Content", master = "Office Theme")

# Insert the graph into the slide
export_graph <- ph_with(export_graph, value = spmlabto, location = ph_location_fullsize())

###### Sequences per Originating laboratory  last 6 month #######
# Assuming v_seqs_per_month_origin has a column named 'my' in Date format
v_seqs_per_month_origin_12mo <- v_seqs_per_month_origin %>%
  filter(my >= (Sys.Date() - months(12))) #Keep only the last 12 months


spmlab12mo <- ggplot(v_seqs_per_month_origin_12mo, aes(x = my, y = TotalSeq, fill = Origin)) +
  geom_bar(stat = "identity") +
  labs(title = "Sekvensering av SC2 i Norge",
       x = "MC%ned",
       y = "Antall Sekvenser") +
  scale_x_date(
    breaks = "1 month",  # Show breaks every month
    labels = scales::date_format("%b %Y")  # Format as month name and year
  ) +
  scale_fill_manual(values = origin_color) +  # Set custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +  # Center the title and increase its size
  theme(legend.position="right")  # Optional: Remove legend if not needed


spmlab12mo


# Add a slide to the presentation with Title and Content layout and Office Theme master
export_graph <- add_slide(export_graph, layout = "Title and Content", master = "Office Theme")

# Insert the graph into the slide
export_graph <- ph_with(export_graph, value = spmlab12mo, location = ph_location_fullsize())


ivalseqcount <- anti_join(allvariants_to, allvariants_v) %>% filter(grepl("^FHI", Origin)) %>% count(my, name = "TotalSeq")
valseqcount <- allvariants_v %>% filter(grepl("^FHI", Origin)) %>% count(my, name = "TotalSeq")

# Merge the two data frames on the "my" column
merged_data <- merge(ivalseqcount, valseqcount, by = "my", all = TRUE, suffixes = c("_invalid", "_valid"))
merged_data_p <- merged_data %>% 
  mutate(Percent = (TotalSeq_invalid / (TotalSeq_valid + TotalSeq_invalid)) * 100)

merged_data_long <- merged_data %>%
  gather(key = "type", value = "value", -my) %>%
  mutate(my = as.Date(paste0(my, " 01"), format="%Y %b %d"))


# Create the plot
qcsekvto <- ggplot(merged_data_long, aes(x = my, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Valid and Invalid Sequences per MC%ned",
    x = "MC%ned", 
    y = "Antall av Sekvenser",
    fill = "Legend"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("TotalSeq_valid" = "darkblue", "TotalSeq_invalid" = "red"),
    labels = c("TotalSeq_valid" = "Valid", "TotalSeq_invalid" = "Invalid")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")  # Set breaks for every month


# Print the chart
print(qcsekvto)


# Add a slide to the presentation with Title and Content layout and Office Theme master
export_graph <- add_slide(export_graph, layout = "Title and Content", master = "Office Theme")

# Insert the graph into the slide
export_graph <- ph_with(export_graph, value = qcsekvto, location = ph_location_fullsize())



###################Last 12 month only : #######################################
merged_data12mo_long <- merged_data %>%
  mutate(my = as.Date(paste0(my, " 01"), format="%Y %b %d")) %>%
  filter(my >= (Sys.Date() - months(12))) %>%
  gather(key = "type", value = "value", -my)




# Create the plot
qcsekv12 <- ggplot(merged_data12mo_long, aes(x = my, y = value, fill = type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Valid and Invalid Sequences per MC%ned",
    x = "MC%ned", 
    y = "Antall av Sekvenser",
    fill = "Legend"
  ) +
  scale_fill_manual(
    name = "Legend",
    values = c("TotalSeq_valid" = "darkblue", "TotalSeq_invalid" = "red"),
    labels = c("TotalSeq_valid" = "Valid", "TotalSeq_invalid" = "Invalid")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")  # Set breaks for every month



# Print the chart
print(qcsekv12)

# Add a slide to the presentation with Title and Content layout and Office Theme master
export_graph <- add_slide(export_graph, layout = "Title and Content", master = "Office Theme")

# Insert the graph into the slide
export_graph <- ph_with(export_graph, value = qcsekv12, location = ph_location_fullsize())


# Calculate cumulative sum of TotalSeq by Origin
v_seqs_per_month_origin <- v_seqs_per_month_origin %>%
  group_by(Origin) %>%
  arrange(my) %>%
  mutate(CumulativeSeq = cumsum(TotalSeq))

cumplot <-ggplot(v_seqs_per_month_origin, aes(x = my, y = CumulativeSeq, color = Origin)) +
  geom_line(size = 1) +
  labs(title = "Sekvensering av SC2 i Norge",
       x = "MC%ned",
       y = "Cumulative Antall Sekvenser") +
  scale_x_date(
    breaks = "1 month",
    labels = scales::date_format("%b %Y")
  ) +
  scale_color_manual(values = origin_color) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  theme(legend.position = "right")



# Add a slide to the presentation with Title and Content layout and Office Theme master
export_graph <- add_slide(export_graph, layout = "Title and Content", master = "Office Theme")

# Insert the graph into the slide
export_graph <- ph_with(export_graph, value = cumplot, location = ph_location_fullsize())


