# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(writexl)    # For writing Excel files
library(hrbrthemes)  # Load hrbrthemes for different themes
library(ggalluvial)  # For alluvial plots
library(igraph)      # For network analysis
library(viridis)     # For color palettes
library(gtsummary)   # For creating tables
library(readxl)     # For reading Excel files
library(factoextra)  # For clustering analysis
library(ggrepel)     # For text labels in plots

# Read the raw news dataset from an Excel file
dfNewsRaw <- read_xlsx("data/dataset_noticias.xlsx")

# Filter dfNewsRaw to keep only the rows where Aceptada is equal to 1
dfNews <- dfNewsRaw %>%
  filter(Aceptada == 1)

# Read the media origins dataset from an Excel file
dfMediosOrigen <- read_xlsx("data/medios_origen.xlsx", col_types = c("text", "text", "text", "text"))

# Rename categories in the 'Ámbito' column
dfMediosOrigen <- dfMediosOrigen %>%
  mutate(Ámbito = case_when(
    Ámbito %in% c("Nacional/Internacional") ~ "International/National",
    Ámbito %in% c("Nacional") ~ "International/National",
    Ámbito %in% c("Local/Regional") ~ "Regional/Local",
    Ámbito %in% c("Temático especializado") ~ "Specialized Thematic Media"
  ))

# Translate the 'País' column from Spanish to English
country_translation <- c(
  "España" = "Spain",
  "USA" = "USA",
  "Vaticano" = "Vatican",
  "Italia" = "Italy",
  "Francia" = "France",
  "Qatar" = "Qatar",
  "Polonia" = "Poland",
  "Méjico" = "Mexico",
  "Chile" = "Chile",
  "Nueva Zelanda" = "New Zealand",
  "Cuba" = "Cuba",
  "Nicaragua" = "Nicaragua",
  "Australia" = "Australia",
  "Brasil" = "Brazil",
  "Alemania" = "Germany",
  "UK" = "UK",
  "Irlanda del Norte" = "Northern Ireland",
  "Argentina" = "Argentina",
  "Canadá" = "Canada",
  "Filipinas" = "Philippines",
  "India" = "India",
  "Bélgica" = "Belgium",
  "Suecia" = "Sweden",
  "Austria" = "Austria",
  "Arquitectura" = "Architecture",
  "Portugal" = "Portugal",
  "Suiza" = "Switzerland",
  "Países Bajos" = "Netherlands",
  "Rumanía" = "Romania",
  "Turquía" = "Turkey",
  "Liechtenstein" = "Liechtenstein",
  "Croacia" = "Croatia",
  "Serbia" = "Serbia",
  "Albania" = "Albania",
  "Eslovenia" = "Slovenia",
  "Malta" = "Malta",
  "Irlanda" = "Ireland",
  "China" = "China"
)

dfMediosOrigen <- dfMediosOrigen %>%
  mutate(País = recode(País, !!!country_translation))

# Translate the 'Tema' column from Spanish to English
topic_translation <- c(
  "Agricultura" = "Agriculture",
  "Alojamiento" = "Accommodation",
  "Anglicano" = "Anglican",
  "Antropología" = "Anthropology",
  "Arqueología" = "Archaeology",
  "Arquitectura" = "Architecture",
  "Arte" = "Art",
  "Arte urbano" = "Urban Art",
  "Artes escénicas" = "Performing Arts",
  "Aventura" = "Adventure",
  "Camino de Santiago" = "Way of St. James",
  "Casa Real" = "Royal Family",
  "Castilla y León" = "Castilla y León",
  "Católico" = "Catholic",
  "Celebridades" = "Celebrities",
  "Ciencia" = "Science",
  "Ciudades Inteligentes" = "Smart Cities",
  "Comunicación" = "Communication",
  "Construcción" = "Construction",
  "Cultura" = "Culture",
  "Deporte" = "Sports",
  "Derecho" = "Law",
  "Diseño" = "Design",
  "Ecología" = "Ecology",
  "Economía" = "Economy",
  "Educación" = "Education",
  "Emigración" = "Emigration",
  "España" = "Spain",
  "Espiritualidad" = "Spirituality",
  "Estilo de vida" = "Lifestyle",
  "Familia" = "Family",
  "Filatelía" = "Philately",
  "Fundaciones" = "Foundations",
  "Gaming" = "Gaming",
  "Gastronomía" = "Gastronomy",
  "Geografía" = "Geography",
  "Geopolítica" = "Geopolitics",
  "Gobierno" = "Government",
  "Hispanidad" = "Hispanidad",
  "Historia" = "History",
  "Hombre" = "Man",
  "Hostelería" = "Hospitality",
  "Idiomas" = "Languages",
  "Iluminación" = "Lighting",
  "Inclusión social" = "Social Inclusion",
  "Inmobiliaria" = "Real Estate",
  "Innovación" = "Innovation",
  "Inversión" = "Investment",
  "Investigación" = "Research",
  "Judaísmo" = "Judaism",
  "LGBTQ+" = "LGBTQ+",
  "Libros" = "Books",
  "Meteorología" = "Meteorology",
  "Militar" = "Military",
  "Ministerio" = "Ministry",
  "Moda" = "Fashion",
  "Moneda" = "Currency",
  "Motor" = "Motor",
  "Mujer" = "Woman",
  "Musulmán" = "Muslim",
  "Música" = "Music",
  "Naturaleza" = "Nature",
  "Ocio" = "Leisure",
  "Opinión" = "Opinion",
  "Patrimonio" = "Heritage",
  "Política" = "Politics",
  "Records" = "Records",
  "Renting" = "Renting",
  "Salud" = "Health",
  "Schengen" = "Schengen",
  "Seguros" = "Insurance",
  "Senderismo" = "Hiking",
  "Tauromaquia" = "Bullfighting",
  "Tecnología" = "Technology",
  "Tiempo" = "Weather",
  "Transporte" = "Transportation",
  "Trenes" = "Trains",
  "Turismo" = "Tourism",
  "Universidad" = "University",
  "Viajes" = "Travel",
  "Vino" = "Wine"
)


# Function to translate topics and handle NAs
translate_topics <- function(topics) {
  if (is.na(topics)) return(NA)
  topics_split <- str_split(topics, ";")[[1]]
  topics_translated <- recode(topics_split, !!!topic_translation)
  paste(topics_translated, collapse = ";")
}

dfMediosOrigen <- dfMediosOrigen %>%
  mutate(Tema = purrr::map_chr(Tema, translate_topics))

# Perform a left join with dfMediosOrigen
dfNews <- left_join(dfNews, dfMediosOrigen, by = "source")

# Group by 'source' and summarize data
dfMedios <- dfNews %>%
  group_by(source) %>%
  summarise(
    Scope = first(Ámbito),  # Get the first value of 'Ámbito'
    Country = first(País),  # Get the first value of 'País'
    Topic = first(Tema),    # Get the first value of 'Tema'
    count = n()             # Count the number of occurrences
  )

# Write the summarized data to an Excel file
write_xlsx(dfMedios, "results/media_coverage.xlsx")


####################################################
# Media distribution plots
####################################################
# Add a numeric column for the X axis
dfMedios <- dfMedios %>%
  arrange(desc(count)) %>%
  mutate(medio_num = seq(1, n(), by = 1))

# Create the bar plot
DistribucionMedios <- ggplot(dfMedios, aes(x = medio_num, y = count)) +
  geom_bar(stat = "identity") +  # Use bars to represent data
  theme_ipsum_ps() +  # Apply the Ipsump theme
  labs(title = "Number of news by media",
       x = "Number of media",  # Label for the X axis
       y = "Number of news by media") +  # Label for the Y axis
  scale_x_continuous(breaks = seq(0, nrow(dfMedios), by = 50)) +  # Adjust X axis ticks
  scale_y_continuous(breaks = seq(0, max(dfMedios$count, na.rm = TRUE), by = 50))  # Adjust Y axis ticks

# Save the bar plot as PDF and JPEG with specified dimensions and resolution
ggsave("results/DistribucionMedios.pdf", plot = DistribucionMedios, width = 7, height = 4, dpi = 600, device = cairo_pdf)
ggsave("results/DistribucionMedios.jpg", plot = DistribucionMedios, width = 7, height = 4, dpi = 600)

# Create the log-log scale plot
DistribucionMediosLogLog <- ggplot(dfMedios, aes(x = medio_num, y = count)) +
  geom_point() +  # Use points instead of bars for a log-log plot
  theme_ipsum_ps() +  # Apply the Ipsump theme
  labs(title = "Number of news by media in Log-Log Scale",
       x = "Number of media (Log)",  # Label for the X axis
       y = "Number of news by media (Log)") +  # Label for the Y axis
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +  # Log scale for X axis
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)))  # Log scale for Y axis

# Save the log-log plot as PDF and JPEG with specified dimensions and resolution
ggsave("Results/DistribucionMediosLogLog.pdf", plot = DistribucionMediosLogLog, width = 7, height = 4, dpi = 600, device = cairo_pdf)
ggsave("Results/DistribucionMediosLogLog.jpg", plot = DistribucionMediosLogLog, width = 7, height = 4, dpi = 600)


####################################################
# Topic distribution plots in specialized media
####################################################

# Processing
topic_count <- dfMedios %>%
  filter(!is.na(Topic)) %>%                    # Filter to remove NAs
  separate_rows(Topic, sep = ";") %>%          # Separate topics
  group_by(source) %>%                         # Group by the 'source' column
  mutate(n_topics = n()) %>%                   # Count topics per record
  ungroup() %>%                                # Ungroup
  mutate(weight = 1 / n_topics) %>%            # Calculate the weight of each topic
  group_by(Topic) %>%                          # Group by topic
  summarise(count = sum(weight))               # Sum the weights to get the count

filtered_topics <- topic_count %>%
  filter(count > 1.0)                          # Filter topics with count greater than 1

# Create the lollipop chart
SpecializedTopics <- ggplot(filtered_topics, aes(x=reorder(Topic, count), y=count)) +
  geom_segment(aes(xend=Topic, yend=0), color="#80b1d3", linewidth=1) +
  geom_point(color="#80b1d3", size=3) +
  geom_text(aes(label=round(count, 1)), vjust=-0.5, nudge_y=0.8) +
  coord_flip() +
  theme_ipsum_ps() +
  labs(x="Topic", y="Number of media weighted with weight greater than one")

ggsave("results/SpecializedMediaTopics.pdf", plot = SpecializedTopics, width =8, height = 11, dpi = 600, device = cairo_pdf)
ggsave("results/SpecializedMediaTopics.jpg", plot = SpecializedTopics, width = 8, height = 11, dpi = 600)


####################################################
# Summary by Country and Scope
####################################################

# General summary: group by country and summarize total media and total news
dfGeneralSummary <- dfMedios %>%
  group_by(Country) %>%
  summarise(
    total_media = n(),  # Total number of media outlets
    total_news = sum(count, na.rm = TRUE),  # Total number of news articles
    .groups = "drop"
  )

# Detailed summary: group by country and scope, then summarize media and news
dfDetailedSummary <- dfMedios %>%
  group_by(Country, Scope) %>%
  summarise(
    media = n(),  # Number of media outlets
    news = sum(count, na.rm = TRUE),  # Number of news articles
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Scope,  # Use scope names for new columns
    values_from = c(media, news),  # Use media and news values
    names_sep = "_"  # Separator for new column names
  )

# Remove the "media_NA" and "news_NA" columns if they exist
dfDetailedSummary <- dfDetailedSummary %>% 
  select(-media_NA, -news_NA)

# Merge general and detailed summaries
dfFinalSummary <- left_join(dfGeneralSummary, dfDetailedSummary, by = "Country")

# Replace NA values with 0
dfFinalSummary <- dfFinalSummary %>%
  mutate(across(starts_with("media_"), ~replace_na(., 0))) %>%
  mutate(across(starts_with("news_"), ~replace_na(., 0))) %>%
  replace_na(list(
    total_media = 0,
    total_news = 0
  ))

# Save the final summary to an Excel file
write_xlsx(dfFinalSummary, "Results/SummaryByCountryAndScope.xlsx")

#################################################################################################

# Setting the limit for categorizing as "Others"
othersLimit <- 10

# Calculate the total media by country
total_media_by_country <- dfMedios %>%
  count(Scope, Country, name = "n") %>%
  group_by(Country) %>%
  summarise(total_media = sum(n), .groups = "drop") %>%
  mutate(Country = ifelse(total_media < othersLimit, "Others", Country)) %>%
  group_by(Country) %>%
  summarise(total_media = sum(total_media), .groups = "drop")

# Count media by country and filter those with fewer than the limit
media_count_by_country <- dfMedios %>%
  group_by(Country) %>%
  summarise(total_media = n(), .groups = "drop") %>%
  filter(total_media < othersLimit)

# Replace country names with "Others" for those with fewer than the limit
dfMedios2 <- dfMedios %>%
  count(Scope, Country, name = "n") %>%
  mutate(Country = ifelse(Country %in% media_count_by_country$Country, "Others", Country)) %>%
  group_by(Scope, Country) %>%
  summarise(n = sum(n), .groups = "drop")

# Merge with the total media by country
dfMedios2 <- left_join(dfMedios2, total_media_by_country, by = "Country")

# Reorder and relevel the factor for countries
dfMedios2 <- dfMedios2 %>%
  mutate(Country = fct_reorder(Country, total_media, .desc = TRUE)) %>%
  mutate(Country = fct_relevel(Country, "Others", after = Inf))

# Filter out rows where Scope or Country is NA if they exist
dfMedios2_filtered <- dfMedios2 %>%
  filter(!is.na(Scope) & !is.na(Country))

# Create the Sankey diagram
ScopeAndCountry <- ggplot(dfMedios2_filtered, aes(axis1 = Scope, axis2 = Country, y = n)) +
  scale_x_discrete(name = "", limits = c("Scope", "Country"), labels = c("Scope", "Country")) +
  geom_alluvium(aes(fill = Scope)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_minimal() +
  ggtitle("Sankey Diagram of Scope and Country (number of media). \nOnly countries with at least 10 different media are represented individually.")

# Show the plot
print(ScopeAndCountry)

# Save the Sankey diagram as PDF and JPEG with specified dimensions and resolution
ggsave("Results/ScopeAndCountry.pdf", plot = ScopeAndCountry, width = 16, height = 11, dpi = 600, device = cairo_pdf)
ggsave("Results/ScopeAndCountry.jpg", plot = ScopeAndCountry, width = 16, height = 11, dpi = 600)


####################################################
# Relation among Google News portals
####################################################
# Specify the country columns
cols_country <- c("España", "Francia", "Irlanda", "Italia", "PaísesBajos", "Polonia", 
                  "Portugal", "Suecia", "UK", "USA", "Alemania", "Australia", "Brasil", 
                  "Canada", "Eslovenia")

# Translation vector from Spanish to English
country_translation <- c(
  "España" = "Spain",
  "Francia" = "France",
  "Irlanda" = "Ireland",
  "Italia" = "Italy",
  "PaísesBajos" = "Netherlands",
  "Polonia" = "Poland",
  "Portugal" = "Portugal",
  "Suecia" = "Sweden",
  "UK" = "UK",
  "USA" = "USA",
  "Alemania" = "Germany",
  "Australia" = "Australia",
  "Brasil" = "Brazil",
  "Canada" = "Canada",
  "Eslovenia" = "Slovenia"
)

# Assign a unique ID to each news article before transforming the dataframe
dfNews$news_ID <- seq_len(nrow(dfNews))

# Step 1: Transform Data
dfNews_long <- dfNews %>%
  pivot_longer(cols = all_of(cols_country), names_to = "Pais", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  mutate(Pais = recode(Pais, !!!country_translation))  # Translate country names

# Sum of news articles by country
nNews <- dfNews_long %>%
  group_by(Pais) %>%
  summarise(nNews = n())

# Preparation for links
news_pairs <- dfNews_long %>%
  select(news_ID, Pais) %>%
  group_by(news_ID) %>%
  summarise(Countries = list(Pais)) %>%
  filter(lengths(Countries) > 1) %>%
  unnest(Countries, .preserve = "news_ID")

# Step 2: Create Nodes
nodes <- nNews

# Preparation for links
news_pairs <- dfNews_long %>%
  select(news_ID, Pais) %>%
  group_by(news_ID) %>%
  summarise(Countries = list(Pais)) %>%
  filter(lengths(Countries) > 1) %>%
  unnest(Countries)

# Step 2: Create Nodes
nodes <- nNews

# Step 3: Create Links
# Create a list of all country pairs for each news ID
news_pairs <- news_pairs %>%
  group_by(news_ID) %>%
  summarise(Pairs = list(expand.grid(Country1 = Countries, Country2 = Countries))) %>%
  unnest(Pairs)

# Remove duplicates and pairs with the same country on both sides
links <- news_pairs %>%
  filter(Country1 != Country2) %>%
  unique()

# Count the number of times each pair appears
links <- links %>%
  group_by(Country1, Country2) %>%
  summarise(Weight = n())

# Step 4: Build the Network
graph <- graph_from_data_frame(d = links, vertices = nodes, directed = FALSE)

# Assign the number of news articles to each node
V(graph)$nNews <- nodes$nNews

# Step 5: Visualization (optional)
plot(graph)

# Export the network to a GEXF file
write_graph(graph, file = "results/GoogleNewsPortalsNetwork.graphml", format = "graphml")

####################################################
# News by Google News portal
####################################################

# Reorder the data by the number of news articles, from highest to lowest
nNews <- nNews %>%
  arrange(desc(nNews))

# Bar chart with the proportion of news articles by country
newsByCountry_barchart <- ggplot(nNews, aes(x = reorder(Pais, desc(nNews)), y = nNews, fill = Pais)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +  # Use viridis for colors
  theme_ipsum_ps() +  # Apply the ipsump theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis text
  labs(x = "Country", y = "Number of News Articles", 
       title = "News Articles Obtained by Google News from Each Country", 
       fill = "Pais")

# Display the bar chart
print(newsByCountry_barchart)

# Save the bar chart as a PDF and JPEG with specified dimensions and resolution
ggsave("results/newsByCountry_barchart.pdf", plot = newsByCountry_barchart, width = 8, height = 6, dpi = 600, device = cairo_pdf)
ggsave("results/newsByCountry_barchart.jpg", plot = newsByCountry_barchart, width = 8, height = 6, dpi = 600)


####################################################

# Create the lollipop chart
newsByCountry_lollipot <- ggplot(nNews, aes(x=reorder(Pais, nNews), y=nNews)) +
  geom_segment(aes(xend=Pais, yend=0), color="#80b1d3", linewidth=1) +  # Draw segments
  geom_point(color="#80b1d3", size=3) +  # Draw points
  geom_text(aes(label=round(nNews, 1)), vjust=-0.5, nudge_y=0.9) +  # Add text labels
  coord_flip() +  # Flip the coordinates
  theme_ipsum_ps() +  # Apply the ipsump theme
  labs(x="Country (Google News)", y="Number of News Articles")  # Label the axes

# Display the lollipop chart
print(newsByCountry_lollipot)

# Save the lollipop chart as a PDF and JPEG with specified dimensions and resolution
ggsave("results/newsByCountry_lollipotpdf", plot = newsByCountry_lollipot, width = 8, height = 5, dpi = 600, device = cairo_pdf)
ggsave("results/newsByCountry_lollipot.jpg", plot = newsByCountry_lollipot, width = 8, height = 5, dpi = 600)


####################################################
# BIC Coverage
####################################################

# Create a summary for each BIC category
bicNews <- dfNews %>%
  group_by(BIC) %>%
  summarise(
    TotalNews = n(),  # Total number of news articles
    InternationalNational = sum(Ámbito %in% c("International/National")),  # Count of International and National
    RegionalLocal = sum(Ámbito %in% c("Regional/Local")),  # Count of Regional and Local
    SpecializedThematic = sum(Ámbito == "Specialized Thematic Media")  # Count of Specialized Thematic
  )

# Save the result to an xlsx file
write_xlsx(bicNews, "results/bicNews_DetailedScope.xlsx")


####################################################
# Network map of co-mentions between cultural assets
####################################################

# Create the edge list for the network
edge_list <- dfNews %>%
  filter(!is.na(link)) %>%
  distinct(BIC, link) %>%
  group_by(link) %>%
  filter(n() > 1) %>%
  do({
    data.frame(t(combn(.$BIC, 2)), stringsAsFactors = FALSE)
  }) %>%
  dplyr::rename(BIC = X1, BIC_pair = X2) %>%
  group_by(BIC, BIC_pair) %>%
  summarise(weight = n(), .groups = 'drop')

# Create the network graph from the edge list
network <- graph_from_data_frame(d = edge_list, directed = FALSE)

# Summarize the number of news articles for each BIC
BIC_counts <- dfNews %>%
  group_by(BIC) %>%
  summarise(nNews = n())

# Ensure the node names are consistent
V(network)$name <- as.character(V(network)$name)

# Create a vector to store the nNews values
nNews_values <- numeric(length = length(V(network)))

# Assign nNews values to the nodes
for (i in seq_along(V(network))) {
  node_name <- V(network)$name[i]
  nNews_values[i] <- BIC_counts$nNews[which(BIC_counts$BIC == node_name)]
}

# Assign the nNews vector to the network
V(network)$nNews <- nNews_values

# Export the network to a GEXF file
write_graph(network, file = "results/BIC_news.graphml", format = "graphml")


####################################################
# BICs Internationalization
####################################################

dfNews <- dfNews %>% ungroup()

# Convert the country columns to numeric
dfNews <- dfNews %>%
  mutate(across(c(Francia, Irlanda, Italia, PaísesBajos, Polonia, Portugal, Suecia, UK, USA, Alemania, Australia, Brasil, Canada, Eslovenia), as.numeric))

# Add columns to identify international news
dfNews <- dfNews %>%
  mutate(Internacional_País = if_else(rowSums(select(., Francia, Irlanda, Italia, PaísesBajos, Polonia, Portugal, Suecia, UK, USA, Alemania, Australia, Brasil, Canada, Eslovenia)) > 0 , TRUE, FALSE),
         Internacional_Idioma = if_else(IdiomaConsenso != "es", TRUE, FALSE),
         Internacional_Medio = if_else(País != "Spain", TRUE, FALSE))

# Calculate metrics for each BIC
dfInternationalization <- dfNews %>%
  group_by(BIC) %>%
  summarise(InternationalNews_Country = sum(Internacional_País, na.rm = TRUE),
            InternationalNews_Language = sum(Internacional_Idioma, na.rm = TRUE),
            InternationalNews_Media = sum(Internacional_Medio, na.rm = TRUE),
            nNews = n())

# Show the result
print(dfInternationalization)

# Save the result to an xlsx file
write_xlsx(dfInternationalization, "results/BICS_Internationalization.xlsx")


####################################################
# Regressions
####################################################
library(officer)     # For creating Word documents
library(flextable)   # For creating tables in Word documents


# Perform the linear regression
regression_model <- lm(InternationalNews_Language ~ nNews, data = dfInternationalization)

# Create summary table
summary_table <- tbl_regression(regression_model)

# Convert the table to a Word-compatible format
word_table <- as_flex_table(summary_table)

# Print the table
print(word_table)

# Export the table to a Word document
doc <- read_docx() %>% 
  body_add_flextable(word_table) %>% 
  body_add_par("Table 1: Regression InternationalNews_Language", style = "heading 1")  # Add a heading to the table if needed

# Save the Word document
print(doc, target = "results/Regression_InternationalNews_Language.docx")

# Perform the linear regression
regression_model <- lm(InternationalNews_Country ~ nNews, data = dfInternationalization)

# Create summary table
summary_table <- tbl_regression(regression_model)

# Convert the table to a Word-compatible format
word_table <- as_flex_table(summary_table)

# Print the table
print(word_table)

# Export the table to a Word document
doc <- read_docx() %>% 
  body_add_flextable(word_table) %>% 
  body_add_par("Table 1: Regression InternationalNews_Country", style = "heading 1")  # Add a heading to the table if needed

# Save the Word document
print(doc, target = "results/Regression_InternationalNews_Country.docx")


# Perform the linear regression
regression_model <- lm(InternationalNews_Media ~ nNews, data = dfInternationalization)

# Create summary table
summary_table <- tbl_regression(regression_model)

# Convert the table to a Word-compatible format
word_table <- as_flex_table(summary_table)

# Print the table
print(word_table)

# Export the table to a Word document
doc <- read_docx() %>% 
  body_add_flextable(word_table) %>% 
  body_add_par("Table 1: Regression InternationalNews_Media", style = "heading 1")  # Add a heading to the table if needed

# Save the Word document
print(doc, target = "results/Regression_InternationalNews_Media.docx")


####################################################
# PCA
####################################################

# Ensure that the columns for PCA are numeric
dfInternationalization <- dfInternationalization %>%
  dplyr::rename(
    `International Source Media` = InternationalNews_Media,
    `Language Different from Spanish` = InternationalNews_Language,
    `News on Google News Portals Different from Spain` = InternationalNews_Country
  )

correlation_data <- as.data.frame(scale(dfInternationalization[, c("News on Google News Portals Different from Spain", "Language Different from Spanish", "International Source Media")]))

# Perform the PCA
res.pca <- prcomp(correlation_data, scale = TRUE)

# Extract the coordinates of the principal components
coords <- as.data.frame(res.pca$x)

# Add the BIC labels to the coordinates dataframe
coords$BIC <- dfInternationalization$BIC

# Visualize the PCA with conditional labels
biplot_1_2 <- fviz_pca_biplot(res.pca, repel = TRUE, geom = "point", 
                              col.var = "blue", col.ind = "black",
                              axes = c(1, 2)) +
  geom_text_repel(data = subset(coords, abs(PC2) >= 0.8 | abs(PC1) > 1), 
                  aes(label = BIC, x = PC1, y = PC2),
                  color = "#fb8072", max.overlaps = 100)

# Save the biplot in PDF format in the 'Results' folder
pdf("results/PCA.pdf", width = 16, height = 8)
print(biplot_1_2)
dev.off()

# Save the biplot in JPEG format in the 'Results' folder
jpeg("results/PCA.jpg", width = 16 * 600, height = 8 * 600, units = "px", res = 600)
print(biplot_1_2)
dev.off()




