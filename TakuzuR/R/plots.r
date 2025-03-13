# Installation et chargement des bibliothèques nécessaires
if (!require(VennDiagram)) install.packages("VennDiagram")
if (!require(readr)) install.packages("readr")

library(VennDiagram)
library(readr)

# Charger le fichier de votre tableau
df <- read_csv("/home/najat/cours_s2/bill/BILL/workflow/scripts/filtered_vcf/variant_commun.csv")

# Fonction pour extraire les positions uniques pour chaque échantillon
extract_positions <- function(chrom, pos) {
  positions <- unlist(strsplit(pos, ","))
  positions <- gsub(".*:(.*)", "\\1", positions)
  paste(chrom, positions, sep = ":")
}

# Extraire les positions pour chaque échantillon
samples <- c("P15", "P30", "P50", "P65", "P90")
positions_set <- lapply(samples, function(sample) {
  df_sample <- df[grepl(sample, df$SOURCES), ]
  unique(mapply(extract_positions, df_sample$CHROM, df_sample$POS))
})

names(positions_set) <- samples

# Données totales des variants (nombres totaux fournis)
total_variants <- c(P15 = 47, P30 = 10, P50 = 50, P65 = 75, P90 = 54)

# Calculer les intersections et variantes uniques
venn_data <- list(
  P15 = positions_set[["P15"]],
  P30 = positions_set[["P30"]],
  P50 = positions_set[["P50"]],
  P65 = positions_set[["P65"]],
  P90 = positions_set[["P90"]]
)

# Créer le diagramme de Venn avec des couleurs plus contrastées
venn.plot <- venn.diagram(
  x = venn_data,
  category.names = names(venn_data),
  filename = NULL,  # Afficher le diagramme dans la console
  output = TRUE,
  main = "Diagramme de Venn des Variants Communs",  # Titre principal
  main.cex = 2,  # Taille du titre principal
  cex = 1.5,     # Taille du texte des catégories
  cex.main = 2,   # Taille du texte du titre principal
  fill = c("#FF6347", "#4682B4", "#32CD32", "#FFD700", "#8A2BE2"),  # Couleurs contrastées
  cat.cex = 1.5,  # Taille du texte des catégories
  cat.pos = 0,    # Position des étiquettes des catégories
  cat.dist = 0.07, # Espacement des étiquettes des catégories
  lwd = 2,        # Largeur de la bordure des cercles
  lty = 1,        # Type de ligne des cercles
  alpha = 0.6,    # Transparence des cercles
  col ="white",  # Couleur des bordures des cercles
  margin = 0.1,   # Marge autour du diagramme
  cat.col = c("darkred", "navy", "darkgreen", "orange", "darkviolet"), # Couleur des étiquettes des catégories
  category.names.cex = 1.5 # Taille des étiquettes des catégories
)

# Afficher le diagramme de Venn
grid.draw(venn.plot)





# Installer et charger les bibliothèques nécessaires
if (!require(readr)) install.packages("readr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(reshape2)) install.packages("reshape2")

library(readr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)

# Charger le fichier CSV
df <- read_csv("/home/najat/cours_s2/bill/BILL/workflow/scripts/filtered_vcf/variant_commun.csv")

# Créer une nouvelle colonne pour chaque échantillon (P15, P50, P65, P90)
# Chaque colonne indiquera la présence (1) ou l'absence (0) d'un variant dans un échantillon
df$P15 <- ifelse(grepl("P15", df$SOURCES), 1, 0)
df$P50 <- ifelse(grepl("P50", df$SOURCES), 1, 0)
df$P65 <- ifelse(grepl("P65", df$SOURCES), 1, 0)
df$P90 <- ifelse(grepl("P90", df$SOURCES), 1, 0)

# Sélectionner uniquement les colonnes nécessaires pour la heatmap
upset_data <- df[, c("P15", "P50", "P65", "P90")]

# Convertir le tableau en format numérique
upset_data_numeric <- as.data.frame(lapply(upset_data, as.numeric))

# Créer la heatmap
# Nous utiliserons ggplot2 pour personnaliser la heatmap

# Convertir les données en format long pour ggplot2
heatmap_data <- melt(upset_data_numeric, variable.name = "Echantillon", value.name = "Presence")

# Ajouter une colonne pour l'index des variantes (l'index des lignes)
heatmap_data$Variant <- rep(1:nrow(upset_data_numeric), times = ncol(upset_data_numeric))

# Créer une palette de couleurs plus riche
# Pour les valeurs de Presence, on peut utiliser une palette à 3 couleurs, par exemple.
ggplot(heatmap_data, aes(x = Echantillon, y = Variant, fill = factor(Presence))) +
  geom_tile() +
  scale_fill_manual(values = c("red", "lightblue", "darkblue")) +  # Plusieurs nuances de bleu pour plus de visibilité
  theme_minimal() +
  labs(title = "Heatmap des Variants Communs", x = "Echantillon", y = "Variants", fill = "Présence") +
  theme(axis.text.y = element_text(size = 5))



# Installer et charger les bibliothèques nécessaires
if (!require(readr)) install.packages("readr")
if (!require(VennDiagram)) install.packages("VennDiagram")

library(readr)
library(VennDiagram)

# Charger le fichier CSV
df <- read_csv("/home/najat/cours_s2/bill/BILL/workflow/scripts/filtered_vcf/variant_commun.csv")

# Créer une nouvelle colonne pour chaque échantillon (P15, P50, P65, P90)
# Chaque colonne indiquera la présence (1) ou l'absence (0) d'un variant dans un échantillon
df$P15 <- ifelse(grepl("P15", df$SOURCES), 1, 0)
df$P50 <- ifelse(grepl("P50", df$SOURCES), 1, 0)
df$P65 <- ifelse(grepl("P65", df$SOURCES), 1, 0)
df$P90 <- ifelse(grepl("P90", df$SOURCES), 1, 0)

# Sélectionner uniquement les colonnes nécessaires pour le diagramme de Venn
upset_data <- df[, c("P15", "P50", "P65", "P90")]

# Extraire les indices des variants pour chaque échantillon (en utilisant les positions uniques)
variants_P15 <- unique(which(upset_data$P15 == 1))
variants_P50 <- unique(which(upset_data$P50 == 1))
variants_P65 <- unique(which(upset_data$P65 == 1))
variants_P90 <- unique(which(upset_data$P90 == 1))

# Créer un diagramme de Venn pour les variantes communes
venn.plot <- venn.diagram(
  x = list(
    P15 = variants_P15,
    P50 = variants_P50,
    P65 = variants_P65,
    P90 = variants_P90
  ),
  category.names = c("P15", "P50", "P65", "P90"),
  filename = NULL,  # Afficher dans la fenêtre graphique
  output = TRUE,
  main = "Diagramme de Venn des Variants Communs",
  fill = c("lightblue", "lightgreen", "lightyellow", "lightpink"),
  cat.cex = 1.5,
  cat.pos = 0,
  cat.dist = 0.05
)

# Afficher le diagramme de Venn
grid.draw(venn.plot)

