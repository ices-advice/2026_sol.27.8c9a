### report.R — TAF reporting script for report table ###

# Create output folder
mkdir("report")

# ------------------------------
# Table 8.1 - catches by country
# ------------------------------
# ---------------------------------------------------------------------
# 1. Read catch data
# ---------------------------------------------------------------------

catch_raw <- read_csv("data/catch/combined_catch_SOL_8c9a.csv")

# ---------------------------------------------------------------------
# 2. Prepare annual landings by country
# ---------------------------------------------------------------------

landings_country <- catch_raw %>%
  filter(Catch.Cat. == "Landings") %>%
  group_by(Year, Country) %>%
  summarise(
    catch_t = sum(Catch_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(catch_t = round(catch_t, 0)) %>%
  pivot_wider(
    names_from = Country,
    values_from = catch_t,
    values_fill = 0
  )

# ---------------------------------------------------------------------
# 3. Prepare annual discards
# ---------------------------------------------------------------------

discards <- catch_raw %>%
  filter(Catch.Cat. == "Discards") %>%
  group_by(Year) %>%
  summarise(
    `Discards of S. solea` = round(sum(Catch_t, na.rm = TRUE), 0),
    .groups = "drop"
  )

# ---------------------------------------------------------------------
# 4. Create final table
# ---------------------------------------------------------------------

catch_table <- landings_country %>%
  left_join(discards, by = "Year") %>%
  mutate(
    `Discards of S. solea` =
      replace_na(`Discards of S. solea`, 0),
    
    `Total catch` =
      France + Portugal + Spain +
      `Discards of S. solea`,
    
    Year = if_else(
      Year %in% c(2009, 2010),
      paste0(Year, "*"),
      as.character(Year)
    )
  ) %>%
  select(
    Year,
    France,
    Portugal,
    Spain,
    `Total catch`,
    `Discards of S. solea`
  )

# ---------------------------------------------------------------------
# 5. Create flextable
# ---------------------------------------------------------------------

ft <- flextable(catch_table)

ft <- autofit(ft)

ft <- set_caption(
  ft,
  caption = "Table 1. Official catch data used in the assessment of Solea solea in divisions 8.c and 9.a."
)

ft <- theme_booktabs(ft)

ft <- align(ft, align = "center", part = "all")

# ---------------------------------------------------------------------
# 6. Export to Word
# ---------------------------------------------------------------------

doc <- read_docx()

doc <- body_add_flextable(doc, value = ft)

print(
  doc,
  target = "report/Table_1_Catch_Data.docx"
)


# ------------------------------
# Table 8.2 - Copy LBI results
# ------------------------------

file.copy("output/LBI/LBI_indicators_table.docx", "report/Table_8_2.docx", overwrite = TRUE)

# ------------------------------
# Table 8.3 - Combined biomass index
# ------------------------------

# ---------------------------------------------------------------------
# 1. Read data
# ---------------------------------------------------------------------

biomass_index <- read_csv("output/SAG/combined_summary.csv")

# ---------------------------------------------------------------------
# 2. Prepare table
# ---------------------------------------------------------------------

biomass_table <- biomass_index %>%
  select(
    Year,
    Combined_Index
  ) %>%
  rename(
    `Combined biomass index` = Combined_Index
  ) %>%
  mutate(
    Year = as.integer(Year),
    `Combined biomass index` =
      round(`Combined biomass index`, 2)
  )

# ---------------------------------------------------------------------
# 3. Create flextable
# ---------------------------------------------------------------------

ft <- flextable(biomass_table)

ft <- set_caption(
  ft,
  caption = "Table 2. Combined biomass index used in the assessment of Solea solea in divisions 8.c and 9.a."
)

ft <- theme_booktabs(ft)
ft <- align(ft, align = "center", part = "all")
ft <- autofit(ft)

# ---------------------------------------------------------------------
# 4. Export to Word
# ---------------------------------------------------------------------

doc <- read_docx()

doc <- body_add_flextable(doc, value = ft)

print(
  doc,
  target = "report/Table_3_Combined_Biomass_Index.docx"
)
