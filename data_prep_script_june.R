library(tidyverse)
library(widyr)
library(stringr)

# INSTITUTIONS NETWORK AND TABLE

institutions <- readr::read_csv("works_authorships_institutions_marine_only.csv") %>%
  select(id, country_code, display_name, institution_id, ror) %>%
  drop_na() %>%
  unique()

# Process the data - handle duplicate institution names
institutions <- institutions %>%
  # Count the number of unique country codes for each institution
  group_by(display_name) %>%
  mutate(country_code_count = n_distinct(country_code)) %>%
  ungroup() %>%
  # Append the country code to the institution name if there is more than one country code for the institution
  mutate(display_name = if_else(country_code_count > 1,
                               str_c(display_name, " (", country_code, ")"),
                               display_name)) %>%
  # Remove the helper column
  select(-country_code_count)


# Count pairs of institution publications
co_publications <- institutions %>%
  pairwise_count(display_name, id, sort = TRUE)

# Remove duplicate counts
institutions_links <- co_publications %>%
  mutate(pair = map2_chr(item1, item2, ~paste(sort(c(.x, .y)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

names(institutions_links) <- c("source_name", "target_name", "count")

country_names <- readr::read_csv("institutions_names.csv") %>%
  select(ror, country_code) %>% unique()

# Get paper publication counts for each institution
institution_works <- institutions %>%
  #select(display_name, institution_id, id, ror, country_code) %>%
  group_by(display_name) %>% #, publication_year) %>% (previously also grouped by publication year)
  summarize(count = n_distinct(id), .groups = 'drop') %>%
  drop_na()

total_inst_collab <- institutions_links %>%
  pivot_longer(
    cols = c("source_name", "target_name"),
    names_to = NULL,
    values_to = "institution"
  ) %>%
  group_by(institution) %>%
  summarize(total_collaborations = sum(count), .groups = 'drop') %>%
  arrange(-total_collaborations)

total_inst_collab <- institutions %>%
  select(display_name, country_code) %>%
  right_join(total_inst_collab, by = c("display_name" = "institution")) %>%
  unique() %>%
  drop_na()

# I believe this join allows us to size the nodes by the amount of publications rather than amount of publication
# It's also the table we show in the report
  institutions_table <- total_inst_collab %>%
  left_join(institution_works, by = "display_name") %>%
    drop_na()

 # For graph, Limit count to only institutions that have published more than 1000 papers in the biodiversity graph.
  inst_table_filtered <- institutions_table %>%
    filter(count >= 1000) %>%
    rename(name = display_name, category = country_code, value = count)

  # Using total papers as value
  inst_nodes <- inst_table_filtered %>%
    select(name, category, value) %>%
    # In our data currently the smaller nodes are about 20 times smaller than the larger nodes.
    # We will need a better way of doing the scaling.
    mutate(symbolSize = scales::rescale(value, to = c(5, 100)))

# This filters our links to only links where both the source and target are in the filtered nodes
inst_links <- institutions_links %>%
  filter(source_name %in% inst_table_filtered$name &
           target_name %in% inst_table_filtered$name) %>%
  select(source_name, target_name) %>%
  rename(source = source_name, target = target_name) %>%
  unique()

inst_categories <- inst_nodes %>%
  select(category) %>%
  distinct() %>%
  as.data.frame() %>%
  rename(name = category)

inst_data_list <- list(
  nodes = inst_nodes,
  links = inst_links,
  categories = inst_categories
)

# Step 5: Convert this list to a JSON string
inst_data <- jsonlite::toJSON(inst_data_list, pretty = TRUE)

# Step 6: Save the JSON string to a file
writeLines(inst_data, "/Users/jasminekindness/mgrbook2_interactive/data/inst_network_june2.json")

institutions_table <- readr::read_delim("country_names.csv", delim = ";") %>%
  select(code, name_of_state) %>%
  right_join(institutions_table, by = c("code" = "country_code")) %>%
  select(-code) %>%
  rename(Institution = display_name, Country = name_of_state, `Total Collaborations` = total_collaborations, `Total Publications` = count) %>%
  mutate(Country = case_when(
    Country == "National Taiwan University" ~ "Taiwan (Province of China)",
    Country == "University of Hong Kong" ~ "Hong Kong Special Administrative Region of China",
    Country == "National Taiwan Ocean University" ~ "Taiwan (Province of China)",
    Country == "Consejo Nacional de Investigaciones Científicas y Técnicas" ~ "Argentina",
    TRUE ~ Country
  )
  ) %>%
  select(Institution, Country, `Total Collaborations`, `Total Publications`) %>%
  unique() %>% arrange(desc(`Total Publications`))


institutions_table %>%
 write_csv("/Users/jasminekindness/mgrbook2_interactive/data/institutions_table_june.csv")

# FUNDER NETWORK AND TABLE

eu_funders <- c(
  "Horizon 2020",
  "FP7 Capacities",
  "First Health Programme",
  "HORIZON EUROPE Research Infrastructures",
  "HORIZON EUROPE Global Challenges and European Industrial Competitiveness",
  "FP7 People",
  "Consumer Programme",
  "Fifth Framework Programme",
  "FP7 Research for the Benefit of SMEs",
  "H2020 Society",
  "FP7 Space",
  "FP7 Coordination of Research Activities",
  "European Agricultural Fund for Rural Development",
  "H2020 LEIT Nanotechnologies",
  "FP7 Food, Agriculture and Fisheries, Biotechnology",
  "Executive Agency for Small and Medium-sized Enterprises",
  "Directorate-General for International Partnerships",
  "FP7 Environment",
  "FP7 Research Potential of Convergence Regions",
  "H2020 Food",
  "Interreg",
  "H2020 Societal Challenges",
  "Erasmus+",
  "H2020 Spreading Excellence and Widening Participation",
  "FP7 Health",
  "FP7 Joint Technology Initiatives",
  "HORIZON EUROPE Climate, Energy and Mobility",
  "FP7 Science in Society",
  "H2020 LEIT Information and Communication Technologies",
  "HORIZON EUROPE Framework Programme",
  "Seventh Framework Programme",
  "FP7 Coherent Development of Research Policies",
  "HORIZON EUROPE Marie Sklodowska-Curie Actions",
  "H2020 Environment",
  "H2020 Science with and for Society",
  "H2020 Future and Emerging Technologies",
  "HORIZON EUROPE European Research Council",
  "H2020 Leadership in Enabling and Industrial Technologies",
  "FP7 Research infrastructures",
  "H2020 Euratom",
  "FP7 Nanosciences, Nanotechnologies, Materials and new Production Technologies",
  "HORIZON EUROPE Health",
  "H2020 Excellent Science",
  "European Maritime and Fisheries Fund",
  "Sixth Framework Programme",
  "H2020 Research Infrastructures",
  "H2020 Marie Skłodowska-Curie Actions",
  "H2020 LEIT Biotechnology",
  "H2020 Fast Track to Innovation",
  "Directorate-General for Maritime Affairs and Fisheries",
  "European Commission",
  "European Regional Development Fund",
  "FP7 People: Marie-Curie Actions",
  "Education, Audiovisual and Culture Executive Agency",
  "H2020 European Research Council",
  "Joint Research Centre",
  "H2020 European Institute of Innovation and Technology",
  "Directorate-General for Research and Innovation",
  "European Social Fund",
  "Directorate-General for the Environment",
  "Directorate-General for Health and Food Safety",
  "European Research Council",
  "Horizon 2020 Framework Programme",
  "Directorate-General for Development and Cooperation - EuropeAid",
  "Research Executive Agency",
  "FP7 Ideas: European Research Council"
)

funders <- readr::read_csv("works_grants_marine_only_funders.csv") %>%
  select(funder_display_name, id, funder_id) %>%
  drop_na() %>%
  unique()

# Read in data to convert between two letter country codes and three letter country codes
three_to_two_country_codes <- readr::read_csv("wikipedia-iso-country-codes.csv") %>%
  select(`Alpha-2 code`, `Alpha-3 code`) %>%
  rename(two_letter = `Alpha-2 code`, three_letter = `Alpha-3 code`)

# Read in data with economy information

world_bank_income_classifications_2023 <- readr::read_csv("world_bank_income_classifications_2023.csv") %>%
  mutate(`Income group` = ifelse(is.na(`Income group`), "Unclassified", `Income group`)) %>%
  select(Economy, Code, `Income group`) %>%
  drop_na()

# Process the data - handle duplicate funder names

# Count pairs of funders of publications
co_funders <- funders %>%
  pairwise_count(funder_display_name, id, sort = TRUE)

# Remove duplicate counts
funder_links <- co_funders %>%
  mutate(pair = map2_chr(item1, item2, ~paste(sort(c(.x, .y)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

names(funder_links) <- c("source_name", "target_name", "value")

# Get total collaborations

total_funder_collab <- funder_links %>%
  pivot_longer(
    cols = c("source_name", "target_name"),
    names_to = NULL,
    values_to = "funder"
  ) %>%
  group_by(funder) %>%
  summarize(total_collaborations = sum(value), .groups = 'drop') %>%
  arrange(-total_collaborations) %>% rename(name = funder)

# Read in funder_works to get total papers
funder_works <- readr::read_csv("funders.csv") %>%
  select(id, display_name, country_code) %>%
  right_join(funders, by = c("id" = "funder_id")) %>%
  left_join(three_to_two_country_codes, by = c("country_code" = "two_letter")) %>%
  mutate(three_letter = case_when(
    country_code == "XK" ~ "XKX",
    country_code == "BQ" ~ "BES",
    country_code == "SS" ~ "SSD",
    country_code == "CW" ~ "CUW",
    TRUE ~ three_letter)) %>%
  left_join(world_bank_income_classifications_2023, by = c("three_letter" = "Code"))

funder_works <- funder_works %>%
  mutate(
    Economy = case_when(
      country_code == "RE" ~ "France",
      country_code == "GF" ~ "France",
      country_code == "MQ" ~ "France",
      country_code == "GP" ~ "France",
      country_code == "FK" ~ "United Kingdom",
      country_code == "MS" ~ "United Kingdom",
      country_code == "JE" ~ "United Kingdom",
      country_code == "BQ" ~ "Netherlands",
      country_code == "SJ" ~ "Norway",
      country_code == "VA" ~ "Vatican City",
      TRUE ~ Economy
    )
  ) %>%
  select(id.y, display_name, Economy) %>%
  group_by(display_name, Economy) %>%
  summarize(count = n_distinct(id.y), .groups = "drop")

# funder_works <- funder_works %>%
#   mutate(Economy = case_when(
#   display_name %in% eu_funders ~ "European Union",
#   TRUE ~ Economy
# ))

# Join tables
funder_table <- total_funder_collab %>%
  left_join(funder_works, by = c("name" = "display_name")) %>%
  drop_na() %>%
  group_by(name) %>%
  unique() %>%
  ungroup() %>%
  mutate(name = case_when(name %in% eu_funders ~ "European Union", TRUE ~ name)) %>%
  mutate(Economy = case_when(name == "European Union" ~ "European Union", TRUE ~ Economy)) %>%
  group_by(Economy, name) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop')


funder_table_filtered <- funder_table %>% filter(count > 50)

funder_links <- funder_links %>%
  mutate(
    source_name = case_when(
      source_name %in% eu_funders ~ "European Union",
      TRUE ~ source_name),
    target_name = case_when(
      target_name %in% eu_funders ~ "European Union",
      TRUE ~ target_name
    )) %>%
  filter(source_name %in% funder_table_filtered$name &
           target_name %in% funder_table_filtered$name) %>%
  select(source_name, target_name) %>%
  rename(source = source_name, target = target_name) %>%
  unique()

#Use total papers as node instead of collaboration count
funder_nodes <- funder_table_filtered %>%
  select(name, count, Economy) %>%
  rename(value = count, category = Economy) %>%
  # Must pay attention to symbolSize - currently biggest node is 20x bigger than smallest node,
  mutate(symbolSize = scales::rescale(value, to = c(4, 80)))

funder_categories <- funder_nodes %>%
  select(category) %>%
  distinct() %>%
  as.data.frame() %>%
  rename(name = category)

funder_data_list <- list(
  nodes = funder_nodes,
  links = funder_links,
  categories = funder_categories
)

# Step 5: Convert this list to a JSON string
funder_data <- jsonlite::toJSON(funder_data_list, pretty = TRUE)

# Step 6: Save the JSON string to a file
writeLines(funder_data, "/Users/jasminekindness/mgrbook2_interactive/data/funders_june_countries2.json")

# Write out funder table
funder_table %>%
  select(name, Economy, total_collaborations, count) %>%
  filter(count > 60) %>%
  arrange(desc(count)) %>%
  rename(`Funding Body` = name, Country = Economy, `Total Collaborations` = total_collaborations, `Total Papers` = count) %>%
  write_csv("/Users/jasminekindness/mgrbook2_interactive/data/funder_table_june2.csv")


## FUNDER COUNTRY NETWORK

funders_id <- readr::read_csv("funders_id.csv")
eu_funders <- readr::read_csv("data/eu_funders.csv")

funders_id$source_name[funders_id$source_name %in% eu_funders$display_name] <- "European Commission"
funders_id$target_name[funders_id$target_name %in% eu_funders$display_name] <- "European Commission"

# Read in funder_works to get total papers
funder_works <- readr::read_csv("marine_works_grants_updated.csv") %>%
  select(funder_display_name, funder_id, id, country_name) %>%
  group_by(funder_display_name, country_name) %>%
  summarize(count = n_distinct(id), .groups = 'drop') %>%
  drop_na()

funder_country <- funders_id %>%
  left_join(funder_works, by = c("source_name" = "funder_display_name"))

funder_country <- funder_country %>%
  left_join(funder_works, by = c("target_name" = "funder_display_name")) %>%
  select(ror_country.x, ror_country.y, count) %>%
  rename(source = ror_country.x, target = ror_country.y) %>%
  filter(source != target)


## INSTITUTION COUNTRY NETWORK

# Read in data to convert between two letter country codes and three letter country codes
three_to_two_country_codes <- readr::read_csv("wikipedia-iso-country-codes.csv") %>%
  select(`Alpha-2 code`, `Alpha-3 code`) %>%
  rename(two_letter = `Alpha-2 code`, three_letter = `Alpha-3 code`)

# Read in data with economy information

world_bank_income_classifications_2023 <- readr::read_csv("world_bank_income_classifications_2023.csv") %>%
  mutate(`Income group` = ifelse(is.na(`Income group`), "Unclassified", `Income group`)) %>%
  select(Economy, Code, `Income group`) %>%
  drop_na()

# Read in institution data
institutions_1 <- readr::read_csv("works_authorships_institutions_marine_only.csv") %>%
  select(id, country_code, display_name, institution_id, ror) %>%
  drop_na() %>%
  unique()

# Join to establish relationship between two and three letter country codes, then join to world bank data
institutions_1 <- institutions_1 %>%
  left_join(three_to_two_country_codes, by = c("country_code" = "two_letter")) %>%
  mutate(three_letter = case_when(
    country_code == "XK" ~ "XKX",
    country_code == "BQ" ~ "BES",
    country_code == "SS" ~ "SSD",
    country_code == "CW" ~ "CUW",
    TRUE ~ three_letter
  )
  ) %>%
  left_join(world_bank_income_classifications_2023, by = c("three_letter" = "Code"))


institutions_1 <- institutions_1 %>%
  mutate(
    Economy = case_when(
    country_code == "RE" ~ "France",
    country_code == "GF" ~ "France",
    country_code == "MQ" ~ "France",
    country_code == "GP" ~ "France",
    country_code == "FK" ~ "United Kingdom",
    country_code == "MS" ~ "United Kingdom",
    country_code == "JE" ~ "United Kingdom",
    country_code == "BQ" ~ "Netherlands",
    country_code == "SJ" ~ "Norway",
    country_code == "VA" ~ "Vatican City",
    TRUE ~ Economy
    )
  )


# Count pairs of institution publications
country_links <- institutions_1 %>%
  pairwise_count(Economy, id, sort = TRUE)

# Remove duplicate counts
country_institutions_links <- country_links %>%
  mutate(pair = map2_chr(item1, item2, ~paste(sort(c(.x, .y)), collapse = "-"))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)

country_nodes <- country_institutions_links %>%
  pivot_longer(
    cols = c("item1", "item2"),
    names_to = NULL,
    values_to = "country"
  ) %>%
  group_by(country) %>%
  summarize(total_collaborations = sum(n), .groups = 'drop') %>%
  mutate(symbolSize = scales::rescale(total_collaborations, to = c(5, 100))) %>%
  #double check that this scale between biggest and smallest makes sense
  # Currently biggest is 100x the size of the smallest, but I'm adjusting for readability
  arrange(total_collaborations)

country_nodes <- world_bank_income_classifications_2023 %>% select(Economy, `Income group`) %>%
  right_join(country_nodes, by = c("Economy" = "country")) %>%
  rename(name = Economy, value = total_collaborations, category = `Income group`) %>%
  arrange(desc(value)) %>%
  filter(value >= 200)


# # Prepare data for Join tables and use total collaboration count instead of total paper count for this viz
# country_nodes <- country_nodes %>%
#   # left_join(inst_table, by = c("name" = "country")) %>%
#   # drop_na() %>%
#   # group_by(name) %>%
#   # filter(row_number() == 1) %>%
#   # ungroup() %>%
#   rename(value = total_collaborations)

# This filters our links to only links where both the source and target are in the filtered nodes
country_links <- country_links %>%
  filter(item1 %in% country_nodes$name &
           item2 %in% country_nodes$name) %>%
  select(-n) %>%
  rename(source = item1, target = item2) %>%
  unique()


categories <- data.frame(name = c("Low income", "Lower middle income", "Upper middle income",
                                  "High income",  "Unclassified"))

country_data_list <- list(
  nodes = country_nodes,
  links = country_links,
  categories = categories
)

# Step 5: Convert this list to a JSON string
country_data <- jsonlite::toJSON(country_data_list, pretty = TRUE)

# Step 6: Save the JSON string to a file
writeLines(country_data, "/Users/jasminekindness/mgrbook2_interactive/data/country_network_june.json")




