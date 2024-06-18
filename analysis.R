# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files
library(writexl)    # For writing Excel files

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
  "Irlanda" = "Ireland",
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


translate_topics <- function(topics) {
  topics_split <- str_split(topics, ";")[[1]]
  topics_translated <- recode(topics_split, !!!topic_translation)
  paste(topics_translated, collapse = "; ")
}

# Apply the translation function to the 'Tema' column
dfMediosOrigen <- dfMediosOrigen %>%
  mutate(Tema = map_chr(Tema, translate_topics))

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
