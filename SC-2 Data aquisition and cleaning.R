#BN COVID SQL query

source(file = "N:/Virologi/Influensa/ARoh/SARS_COV2/Scripts/BN_SC2_adresse.R")

# Extract and process entry information fields
entryinf <- tbl(con, "ENTRYINFOFIELDS") %>%
  select(FIELDID, DISPNAME, NAME) %>%
  collect()


# Extract and process entry fields
entryfld <- tbl(con, 'ENTRYFLD') %>%
  collect() %>%
  left_join(entryinf, by = 'FIELDID') %>%
  mutate(FIELDID = DISPNAME) %>%
  pivot_wider(names_from = DISPNAME, values_from = CONTENT) %>%
  select(-OBJACTIONID, -FIELDID, -NAME) %>%
  group_by(KEY) %>%
  summarise_all( ~ ifelse(all(is.na(.)), NA, paste(na.omit(.), collapse = ";")))


# Extract entry table
entrytable <- tbl(con, "ENTRYTABLE") %>%
  collect()

# Get the mapping of DISPNAME to column names
name_mapping <- entryinf %>%
  filter(!is.na(DISPNAME)) %>%
  select(NAME, DISPNAME) %>%
  distinct()

# Replace column names in 'entrytable' dataframe with DISPNAME from 'entryinf'
entrytable <- entrytable %>%
  rename_with( ~ ifelse(
    . %in% name_mapping$NAME,
    as.character(name_mapping$DISPNAME[match(., name_mapping$NAME)]),
    .
  ), .cols = everything())


entrytable_cols <- names(entrytable)
entryfld_cols <- names(entryfld)

# Step 2: Find columns that are duplicated (i.e., present in both dataframes)
duplicated_cols <- intersect(entrytable_cols, entryfld_cols)

# Step 3: Filter entrytable to exclude duplicated columns
filtered_entrytable <- entrytable %>% select(-all_of(duplicated_cols), KEY)

merged_df <- left_join(filtered_entrytable, entryfld, by = "KEY") %>%
  clean_names()


# Further processing, including merging S columns and adding time variables
allvariants  <- merged_df %>%
  unite("S_merged", s, s2, s3, sep = ";", na.rm = TRUE) %>%
  mutate(
    week = week(as.Date(prove_tatt)),
    # Extract week from Prøve_tatt
    year = year(as.Date(prove_tatt)),
    # Extract year from Prøve_tatt
    wy = yearweek(as.Date(prove_tatt)),
    # Extract year-week from Prøve_tatt
    my = yearmonth(as.Date(prove_tatt))  # Extract year-month from Prøve_tatt
  ) %>%
  select(
    KEY = key,
    PROVE_TATT = prove_tatt,
    P = p,
    FYLKENAVN = fylke,
    MATERIALE = materiale,
    Dekning_Artic = coverage_breadth_artic,
    Dekning_Nano = coverage_breadth_nano,
    SEKV_OPPSETT_NANOPORE = sekv_oppsett_nano,
    SEKV_OPPSETT_SWIFT7 = sekv_oppsett_swift,
    SEQUENCEID_NANO29 = sekv_oppsett_nano,
    SEQUENCEID_SWIFT = sequence_id_swift,
    Dekning_Swift = coverage_breadth_swift,
    GISAID_PLATFORM = gisaid_platform,
    GISAID_EPI_ISL = gisaid_epi_isl,
    GENOTYPE_SVART_I_LABWARE = genotype_svart_i_labware,
    Dekning_Eksterne = coverage_breadth_eksterne,
    Project = p,
    INNSENDER = innsender,
    COVERAGE_DEPTH_SWIFT = coverage_depth_swift,
    COVARAGE_DEPTH_NANO = coverage_depth_nano,
    PANGOLIN_NOM = pangolin_nom,
    FULL_PANGO_LINEAGE = full_pango_lineage,
    N = n,
    ORF1A = orf1a,
    ORF1B = orf1b,
    ORF3A = orf3a,
    E = e,
    ORF6 = orf6,
    ORF7A = orf7a,
    ORF8 = orf8,
    ORF9B = orf9b,
    ORF14 = orf14,
    ORF10 = orf10,
    S_merged,
    # Include the new S_merged column
    week,
    year,
    wy,
    my
  ) %>%
  filter(PROVE_TATT != "")  # Filter out empty PROVE_TATT

# Filter out low quality samples
allvariants_to <- allvariants %>%
  filter((Dekning_Swift != "") |
           (Dekning_Artic != "") |
           (Dekning_Nano != "")  |
           (Dekning_Eksterne != "")
  ) %>%
  filter (PANGOLIN_NOM != "#BESTILT#") %>%
  filter(year >= 2020)

allvariants_v <- allvariants %>%
  filter((Dekning_Swift >= 70) |
           (Dekning_Artic >= 70) |
           (Dekning_Nano >= 70)  |
           (Dekning_Eksterne >= 70)
  ) %>%
  filter (PANGOLIN_NOM != "#BESTILT#") %>%
  filter (PANGOLIN_NOM != "Inkonklusiv") %>%
  filter (PANGOLIN_NOM != "inkonklusiv") %>%
  filter (PANGOLIN_NOM != "Se kommentar") %>%
  filter (PANGOLIN_NOM != "Seekom") %>%
  filter (PANGOLIN_NOM != "") %>%
  filter (PANGOLIN_NOM != "Failed") %>%
  filter (PANGOLIN_NOM != "failed") %>%
  filter (PANGOLIN_NOM != "Unassigned")

rm(entrytable, entryfld, entryinf, merged_df, name_mapping)
