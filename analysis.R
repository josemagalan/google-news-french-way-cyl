# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files
library(writexl)    # For writing Excel files
library(hrbrthemes)  # Load hrbrthemes for different themes
library(ggalluvial)  # For alluvial plots

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





