

######################   Collapsing Pangolins  ##################################
# https://www.who.int/activities/tracking-SARS-CoV-2-variants go here to update the list
# Add a Collapsed pango column to the dataset based on the long Pangolin lineage

allvariants_v <- allvariants_v %>%
  mutate(
    Collapsed_pango = case_when(
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.86.1.1.11.1.1.1.3.8.1") ~ "LP.8.1",
      like(FULL_PANGO_LINEAGE, "KP.2.3XEC") ~ "Andre SARS CoV 2",
      like(FULL_PANGO_LINEAGE, "XEC.") ~ "XEC",
      like(FULL_PANGO_LINEAGE, "KS.1.1KP.3.3") ~ "XEC",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.86.1.1.11.1.2") ~ "KP.2",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.86.1.1.11.1.3") ~ "KP.3",
      like(FULL_PANGO_LINEAGE, "BA.2.86.1.1.11.1.2") ~ "KP.2",
      like(FULL_PANGO_LINEAGE, "BA.2.86.1.1.11.1.3") ~ "KP.3",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.86") ~ "BA.2.86",
      like(FULL_PANGO_LINEAGE, "BA.2.86") ~ "BA.2.86",
      like(FULL_PANGO_LINEAGE, "XBB.1.9.2.5.1") ~ "EG.5.1",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.75.3.4.1.1.1.1") ~ "CH.1.1",
      like(FULL_PANGO_LINEAGE, "XBB.") ~ "XBB",
      like(FULL_PANGO_LINEAGE, "BJ.1BM.1.1.1") ~ "XBB",
      #grepl("^\\[", FULL_PANGO_LINEAGE) ~ "Andre recombinanter",
      #like(FULL_PANGO_LINEAGE, "X") ~ "Andre recombinanter",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.5.3.1.1.1.1.1") ~ "BQ.1",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2.75") ~ "BA.2.75",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.5") ~ "BA.5",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.4") ~ "BA.4",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.3") ~ "BA.3",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.2") ~ "BA.2",
      like(FULL_PANGO_LINEAGE, "B.1.1.529.1") ~ "BA.1",
      like(FULL_PANGO_LINEAGE, "B.1.1.529") ~ "Omicron",
      like(FULL_PANGO_LINEAGE, "B.1.617") ~ "Delta",
      grepl("^B\\.1\\.1\\.7$", FULL_PANGO_LINEAGE) ~ "Alpha",
      # Exact match for Alpha variant
      like(FULL_PANGO_LINEAGE, "B.1.351") ~ "Beta",
      like(FULL_PANGO_LINEAGE, "B.1.1.28.1") ~ "Gamma",
      like(FULL_PANGO_LINEAGE, "B.1.427") |
        like(FULL_PANGO_LINEAGE, "B.1.429") ~ "Epsilon",
      like(FULL_PANGO_LINEAGE, "B.1.1.28.2") ~ "Zeta",
      like(FULL_PANGO_LINEAGE, "B.1.525") ~ "Eta",
      like(FULL_PANGO_LINEAGE, "B.1.1.28.3") ~ "Theta",
      like(FULL_PANGO_LINEAGE, "B.1.526") ~ "Iota",
      like(FULL_PANGO_LINEAGE, "B.1.617.1") ~ "Kappa",
      like(FULL_PANGO_LINEAGE, "B.1.1.1.37.1") ~ "Lambda",
      like(FULL_PANGO_LINEAGE, "B.1.621") ~ "Mu",
      TRUE ~ "Andre SARS CoV 2"
    )
  )


# Load the first CSV file
variant_mappings_url_1 <- "https://www.ecdc.europa.eu/sites/default/files/documents/PathogenVariant_public_mappings.csv"
variant_mappings_1 <- read.csv(variant_mappings_url_1)

# Load the second CSV file
variant_mappings_url_2 <- "https://www.ecdc.europa.eu/sites/default/files/documents/PathogenVariant_public_mappings_VUM.csv"
variant_mappings_2 <- read.csv(variant_mappings_url_2)

# Combine the two data frames
combined_variant_mappings <- rbind(variant_mappings_1, variant_mappings_2)

# Split the included.sub.lineages into separate variants
combined_variant_mappings$included.sub.lineages <- strsplit(combined_variant_mappings$included.sub.lineages, "\\|")

# Ensure that included.sub.lineages are character vectors
combined_variant_mappings$included.sub.lineages <- lapply(combined_variant_mappings$included.sub.lineages,
                                                          as.character)

# Modify the find_matched_variant function to return a single value (if any) rather than a vector
find_matched_variant <- function(PANGOLIN_NOM) {
  matched_variants <- combined_variant_mappings$VirusVariant[sapply(combined_variant_mappings$included.sub.lineages, function(variants)
    PANGOLIN_NOM %in% variants)]
  if (length(matched_variants) > 0) {
    return(matched_variants[1])  # Return the first matched variant
  } else {
    return(NA)
  }
}

# Match included.sub.lineages and create a new column "Tessy" in the dataframe
allvariants_v$Tessy <- sapply(allvariants_v$PANGOLIN_NOM, find_matched_variant)


# Fill empty fields in "Tessy" column with "Andre Sars-CoV2"
allvariants_v$Tessy <- ifelse(is.na(allvariants_v$Tessy),
                              "Andre SARS CoV 2",
                              allvariants_v$Tessy)

#Mutation based VOI/VUM/VOC classificaiton (not in use currently)


allvariants_v <- allvariants_v %>%
  mutate(VUM = case_when(
    grepl("XEC", Tessy)  ~ "XEC",
    grepl("LP.8.1", Tessy)  ~ "LP.8.1",
    # grepl("BA.2.86", Tessy) & grepl("F456L", S_merged) ~ "BA.2.86 +F456L",
    TRUE ~ ""
  ))

allvariants_v <- allvariants_v %>%
  mutate(VOI = case_when(
    grepl("BA.2.86", Tessy) ~ "BA.2.86",
    grepl("KP.3", Tessy) ~ "KP.3",
    TRUE ~ ""
  ))



VUM <- allvariants_v %>% filter(my > yearmonth(Sys.Date() %m-% months(6)))  %>% group_by(my, VUM) %>% count()
VOI <- allvariants_v %>% filter(my > yearmonth(Sys.Date() %m-% months(6)))  %>% group_by(my, VOI) %>% count()
Stat <- allvariants_v %>% filter(my > yearmonth(Sys.Date() %m-% months(6))) %>% group_by(my, PANGOLIN_NOM) %>% count()

#write.csv2(Stat, "Stat.csv")
#write.csv2(VUM, "VUM.csv")
#write.csv2(VOI, "VOI.csv")

# Define custom colors for each Collapsed_pango name
custom_colors <- c(
  "BA.2.86.X" = "#83e4da",
  "KP.2" = "#CD6090",
  "KP.3" = "#EE799F",
  "EG.5" = "#EEA9B8",
  "EG.5.1" = "#ff5733",
  "XEC" = "#EE2C2C",
  "CH.1.1" = "#2F6B8EFF",
  "XBB" = "#8B1A1A",
  "BQ.1" = "#31B57BFF",
  "BA.2.75" = "#FFF68F",
  "BA.2.86" = "green",
  "BA.5" = "#556B2F",
  "BA.4" = "#8BD646FF",
  "BA.3" = "#C0FF3E",
  "BA.2" = "#CDC673",
  "BA.1" = "#00EE00",
  "B.1.1.529" = "#26AD81FF",
  "Omikron - " = "#00ffc4",
  "Delta" = "#FF8C00",
  "Alpha" = "#97FFFF",
  "Beta" = "#009ACD",
  "Gamma" = "#104E8B",
  "Epsilon" = "#CAE1FF",
  "Andre SARS CoV 2" = "darkslategray",
  "Mu" = "#668B8B",
  "Lambda" = "#836FFF",
  "Kappa" = "#27408B",
  "Zeta" =  "#5D00D9",
  "Theta" = "#0000ff",
  "Kappa" = "#3399ff",
  "LP.8.1" = "#3C15c4"
)


variant_color <- c(
  "BA.2.86"  =  "#40436d",
  "Andre SARS CoV 2" = "#ec7c73" ,
  "KP.3" = "#e0f0f7",
  "XEC" = "#f0af5e",
  "KP.2" =  "#09181f",
  "XBB" = "#2a6a82",
  "BA.5" = "#f9dc8c",
  "BA.3" = "#a93c38",
  "BA.2" = "#61d2b2",
  "EG.5.1" = "#fee9e6",
  "LP.8.1" = "#5D00D9"
  
)

#"#c8e1ec" "#179463"


# Classification by Origin of Sequences
allvariants_to <- allvariants_to %>%
  mutate(
    Origin = case_when(
      startsWith(KEY, "SUS-") ~ "Stavanger",
      startsWith(KEY, "HUS_n") ~ "Haukeland",
      startsWith(KEY, "OUS") ~ "OUS",
      startsWith(KEY, "STO") ~ "St. Olav",
      grepl("hCoV-19/Norway/Ahus", KEY) ~ "Ahus",
      grepl("hCoV_19_Norway_SIHF", KEY) ~ "SIHF",!is.na(SEQUENCEID_SWIFT) ~ "FHI-NSC",!is.na(SEQUENCEID_NANO29) ~ "FHI-Nano",!is.na(Dekning_Artic) ~ "FHI-Illumina",
      TRUE ~ NA_character_
    )
  )

# Classification by Origin of Sequences
allvariants_v <- allvariants_v %>%
  mutate(
    Origin = case_when(
      startsWith(KEY, "SUS-") ~ "Stavanger",
      startsWith(KEY, "HUS_n") ~ "Haukeland",
      startsWith(KEY, "OUS") ~ "OUS",
      startsWith(KEY, "STO") ~ "St. Olav",
      grepl("hCoV-19/Norway/Ahus", KEY) ~ "Ahus",
      grepl("hCoV_19_Norway_SIHF", KEY) ~ "SIHF",!is.na(SEQUENCEID_SWIFT) ~ "FHI-NSC",!is.na(SEQUENCEID_NANO29) ~ "FHI-Nano",!is.na(Dekning_Artic) ~ "FHI-Illumina",
      TRUE ~ NA_character_
    )
  )

# Custom colors for Origin plot
origin_color <- c(
  "Ahus" = "darkgreen",
  "FHI-Nano" = "#0d3c6f",
  "St.Olav" = "darkred",
  "FHI-NSC" = "#1975d8",
  "OUS" = "slateblue",
  "FHI-Illumina" = "#051629",
  "Stavanger" = "darkgrey",
  "Haukeland" = "purple",
  "SIHF" = "#a0e3be",
  "Andre" = "grey"
  
)
