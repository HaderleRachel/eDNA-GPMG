####MAP####
# LIBRARIES ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(ggspatial)
library(ggnewscale)
library(tidyterra)
library(terra)
library(patchwork)

# DATA ----

# Guadeloupe shapefile (already transformed into sf)
Guad_sf <- read_sf("Guad_sf.gpkg")

# GPX
gpx_files <- list.files("GPS", pattern = "\\.gpx$", full.names = TRUE)
gpx_list <- lapply(gpx_files, function(f) {
  st_read(dsn = f, layer = "tracks", quiet = TRUE) %>%
    mutate(file_name = basename(f))
})
gpx_all <- do.call(rbind, gpx_list)

# Transect centroids
# Load centroids from a CSV
centroids_df <- read.csv("centroïdes_transects.csv")

# Check column names (adapt if needed)
head(centroids_df)

# Create an sf object from CSV
centroids <- st_as_sf(centroids_df, coords = c("lon", "lat"), crs = 4326)

# Add a clean "label" column (adapt according to your CSV values)
centroids$label <- factor(centroids$nom_fichier,
                          levels = unique(centroids$nom_fichier),
                          labels = c("Har_mang", "Seagrass", "Coch_mang", "Har_core"))  # adapt if needed


# Satellite background for GPMG area
bbox <- st_bbox(c(xmin = -61.6, xmax = -61.5, ymin = 16.2, ymax = 16.28), crs = 4326)
library(maptiles)
basemap <- get_tiles(bbox, provider = "Esri.WorldImagery", crop = TRUE, zoom = 15)
basemap_df <- as.data.frame(basemap, xy = TRUE)
library(scales)  # for rgb()
basemap_df$fill <- rgb(basemap_df$red, basemap_df$green, basemap_df$blue, maxColorValue = 255)


# RECTANGLES AS SPATIAL OBJECTS ----
# Zoom rectangle (GPMG)
zoom_rect <- st_as_sfc(st_bbox(c(xmin = -61.56, xmax = -61.53, ymin = 16.21, ymax = 16.26), crs = st_crs(4326)))
# CSLV rectangle (another area)
cslv_rect <- st_as_sfc(st_bbox(c(xmin = -61.849, xmax = -61.833, ymin = 16.081, ymax = 16.125), crs = st_crs(4326)))

# OVERVIEW MAP ----
# Load bathymetry raster
bathy <- rast("~/Documents/R/THESE/ADNe Guadeloupe R/DONNEES/MNT_ANTS100m_HOMONIM_WGS84_PBMA_ZNEG.asc")

# Convert to data.frame for ggplot
bathy_df <- as.data.frame(bathy, xy = TRUE)
names(bathy_df)[3] <- "depth"
bathy_df <- na.omit(bathy_df)

# Overview map without axes but with a frame
overview_map <- ggplot() +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = depth)) +
  scale_fill_viridis_c(name = "Bathymetry (m)", option = "mako", direction = 1) +
  
  geom_sf(data = Guad_sf, fill = "grey95", color = "grey40", linewidth = 0.3) +
  geom_sf(data = zoom_rect, fill = NA, color = "darkred", linewidth = 0.7) +
  geom_sf(data = cslv_rect, fill = NA, color = "black", linewidth = 0.7) +
  
  annotate("text", x = -61.525, y = 16.18, label = "GPMG-UME",
           color = "darkred", size = 4, fontface = "bold", hjust = 0) +
  annotate("text", x = -61.843, y = 16.14, label = "LwC",
           color = "black", size = 4, fontface = "bold", hjust = 1) +
  
  coord_sf(xlim = c(-62, -61), ylim = c(15.8, 16.6), expand = FALSE) +
  
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "none"
  )

overview_map

# MAIN MAP ----
main_map <- ggplot() +
  geom_raster(data = basemap_df, aes(x = x, y = y, fill = fill)) +
  scale_fill_identity() +
  ggnewscale::new_scale_fill() +
  geom_sf(data = gpx_all, color = "red", size = 0.8) +
  geom_sf_text(data = centroids, aes(label = label),
               color = "white", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(xlim = c(-61.56, -61.53), ylim = c(16.21, 16.26), expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "darkred", fill = NA, linewidth = 5),
    axis.title = element_blank()
  )

# COMBINATION WITH ZOOM ARROW ----
final_map <- overview_map + main_map +
  plot_layout(ncol = 2, widths = c(1, 1.4)) +
  plot_annotation(
    title = NULL,
    subtitle = NULL,
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )
final_map

library(grid)
grid.draw(segmentsGrob(
  x0 = unit(0.18, "npc"), y0 = unit(0.55, "npc"),
  x1 = unit(0.54, "npc"), y1 = unit(0.65, "npc"),
  gp = gpar(col = "darkred", lwd = 2),
  arrow = arrow(length = unit(0.2, "cm"))
))


### Data loading ####
#### Port data ####
data_port <- read.csv2(file="input_data.csv")

# Relevant columns
spy_cols <- c("SPY221581", "SPY235475", 
              "SPY235471", "SPY235476",
              "SPY235472", "SPY235474",
              "SPY233837", "SPY235469")

# Robust cleaning function
clean_numeric <- function(x) {
  x <- gsub("[\u00A0\u202F ]", "", x)  # remove normal, non-breaking, and thin non-breaking spaces
  x[x == ""] <- NA                    # replace empty strings with NA
  as.numeric(x)
}

# Apply to all SPY* columns
data_port[spy_cols] <- lapply(data_port[spy_cols], clean_numeric)


# Step 2: Create grouped columns
data_port$HC <- rowSums(data_port[, c("SPY221581", "SPY235475")], na.rm = TRUE)
data_port$CM <- rowSums(data_port[, c("SPY235471", "SPY235476")], na.rm = TRUE)
data_port$HM <- rowSums(data_port[, c("SPY235472", "SPY235474")], na.rm = TRUE)
data_port$SM <- rowSums(data_port[, c("SPY233837", "SPY235469")], na.rm = TRUE)

# Step 3: Convert to presence/absence
data_port$HC <- ifelse(data_port$HC > 0, 1, 0)
data_port$CM <- ifelse(data_port$CM > 0, 1, 0)
data_port$HM <- ifelse(data_port$HM > 0, 1, 0)
data_port$SM <- ifelse(data_port$SM > 0, 1, 0)

data_port <- data_port[, !(names(data_port) %in% spy_cols)]
data_port <- data_port[-147,]

colSums(data_port[, (ncol(data_port)-3):ncol(data_port)] == 1)

data_port


data_port_fb <- rfishbase::species(species_list = data_port$scientificName, fields = c("Species", "Importance", "DemersPelag"))
data_port_fb <- left_join(data_port, data_port_fb, by = c("scientificName" = "Species"))

write.xlsx(data_port_fb, "published_taxa_table.xlsx")

### Bargraph & Venn diagram ###
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvenn)
library(patchwork)

# ---- 1. Barplot by class per site ----
data_port <- read.csv2("tableau taxons motus.csv")

taxon_counts <- data_port %>%
  dplyr::select(TAXON, HC, CM, HM, SM) %>%
  pivot_longer(cols = c(HC, CM, HM, SM), names_to = "site", values_to = "presence") %>%
  filter(presence == 1) %>%
  distinct(site, TAXON) %>%
  count(site, name = "n_taxa")

print(taxon_counts)

total_taxa <- data_port %>%
  filter(HC == 1 | CM == 1 | HM == 1 | SM == 1) %>%
  distinct(TAXON) %>%
  count()

print(total_taxa)

# Convert to long format
data_long <- data_port %>%
  dplyr::select(TAXON, class, HC, CM, HM, SM) %>%
  pivot_longer(cols = c(HC, CM, HM, SM), names_to = "site", values_to = "presence") %>%
  filter(presence == 1) %>%
  distinct(site, class, TAXON)

# Count unique MOTUs per site and class
motu_counts <- data_long %>%
  group_by(site, class) %>%
  summarise(n_motus = n(), .groups = "drop")

# Ensure all site x class combinations exist (even with 0)
all_combos <- expand.grid(
  site = unique(motu_counts$site),
  class = unique(motu_counts$class)
)

motu_counts <- left_join(all_combos, motu_counts, by = c("site", "class")) %>%
  mutate(n_motus = ifelse(is.na(n_motus), 0, n_motus))

# Barplot
# Relevel the factor to define site order
motu_counts$site <- factor(motu_counts$site,
                           levels = c("HM", "HC", "SM", "CM"))

# Barplot
barplot <- ggplot(motu_counts, aes(x = site, y = n_motus, fill = class)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = n_motus),
            position = position_dodge(width = 0.6),
            vjust = -0.3, size = 4) +
  scale_x_discrete(labels = c(
    "HC" = "Har_core",
    "HM" = "Har_mang",
    "CM" = "Coch_mang",
    "SM" = "Seagrass"
  )) +
  labs(
    x = "Site",
    y = "Number of taxa",
    fill = "Class",
    title = NULL
  ) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

barplot

# ---- 2. Venn diagram with ggvenn ----
library(dplyr)
library(ggvenn)
library(patchwork)
library(ggplot2)

# Filter data_port to keep only Teleostei with non-NA Class
data_port_filtered <- data_port %>%
  filter(class == "Teleostei")

data_port_filtered <- data_port %>%
  filter(class == "Teleostei") %>%
  group_by(TAXON) %>%
  summarise(across(tail(names(.), 4), ~ sum(.), .names = "{col}")) %>%
  ungroup() %>%
  mutate(across(tail(names(.), 4), ~ ifelse(. > 1, 1, .)))


# 1. Venn diagram by TAXA
venn_list_taxa <- list(
  Har_core = data_port_filtered$TAXON[data_port_filtered$HC == 1],
  Coch_mang = data_port_filtered$TAXON[data_port_filtered$CM == 1],
  Har_mang = data_port_filtered$TAXON[data_port_filtered$HM == 1],
  Seagrass = data_port_filtered$TAXON[data_port_filtered$SM == 1]
)

venn_plot_taxa <- ggvenn(venn_list_taxa, show_percentage = FALSE,
                         fill_color = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
                         stroke_size = 0.3, set_name_size = 4, text_size = 4) +
  ggtitle("TAXA")

# Filter data_port to keep only Teleostei with non-NA Class
data_port_filtered <- data_port %>%
  filter(!is.na(class), class == "Teleostei")

data_port_filtered <- data_port %>%
  filter(!is.na(class), class == "Teleostei")%>%
  group_by(TAXON, family) %>%
  summarise(across(tail(names(.), 4), ~ sum(.), .names = "{col}")) %>%
  ungroup()%>%
  mutate(across(tail(names(.), 4), ~ ifelse(. > 1, 1, .)))

data_port_filtered <- data_port_filtered[data_port_filtered$family != "", ]

n_families <- n_distinct(data_port_filtered$family)
n_families


# 2. Venn diagram by FAMILY
venn_list_family <- list(
  Har_core = data_port_filtered$family[data_port_filtered$HC == 1],
  Coch_mang = data_port_filtered$family[data_port_filtered$CM == 1],
  Har_mang = data_port_filtered$family[data_port_filtered$HM == 1],
  Seagrass = data_port_filtered$family[data_port_filtered$SM == 1]
)

venn_plot_family <- ggvenn(venn_list_family, show_percentage = FALSE,
                           fill_color = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"),
                           stroke_size = 0.3, set_name_size = 4, text_size = 4) +
  ggtitle("FAMILIES")

# 3. Combine with centered main title
final_plot <-  venn_plot_family + venn_plot_taxa +
  plot_layout(ncol = 2, widths = c(1.1, 1)) +
  plot_annotation(
    title = "Comparison of Shared Families and Taxa Across Sites",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
  )

# Display
print(final_plot)

# ---- 3. Venn details extraction ----
library(dplyr)
library(tibble)
library(purrr)
library(combinat)

get_venn_details <- function(venn_list) {
  sites <- names(venn_list)
  all_taxa <- unique(unlist(venn_list))
  
  # For each taxon/family, determine in how many (and which) sites it appears
  venn_df <- map_dfr(all_taxa, function(taxon) {
    present_in <- names(venn_list)[map_lgl(venn_list, ~ taxon %in% .x)]
    tibble(
      Element = taxon,
      Sites = paste(sort(present_in), collapse = ","),
      N_sites = length(present_in)
    )
  }) %>% arrange(desc(N_sites))
  
  return(venn_df)
}

# Details for TAXA
venn_details_taxa <- get_venn_details(venn_list_taxa)
cat("=== TAXA DETAILS ===\n")
print(venn_details_taxa)

# Details for FAMILIES
venn_details_family <- get_venn_details(venn_list_family)
cat("=== FAMILY DETAILS ===\n")
print(venn_details_family)


# Number of elements per exact site combination
summary_taxa <- venn_details_taxa %>%
  count(Sites, name = "N_taxa") %>%
  arrange(desc(N_taxa))

print(summary_taxa)

summary_family <- venn_details_family %>%
  count(Sites, name = "N_families") %>%
  arrange(desc(N_families))

print(summary_family)

# Export details to CSV files
write.csv(venn_details_taxa, "venn_details_taxa.csv", row.names = FALSE)
write.csv(venn_details_family, "venn_details_family.csv", row.names = FALSE)

#### CSLV ####
data_cslv = read.csv('fiche code 1.csv', row.names = NULL, stringsAsFactors = FALSE)

# Step 1: Rename columns to facilitate the join
colnames(data_cslv)[colnames(data_cslv) == "Taxonomie"] <- "scientificName"
colnames(data_cslv)[colnames(data_cslv) == "Liste.E"] <- "identificationRemarks"
colnames(data_cslv)[colnames(data_cslv) == "Class"] <- "class"
colnames(data_cslv)[colnames(data_cslv) == "Order"] <- "order"
colnames(data_cslv)[colnames(data_cslv) == "Family"] <- "family"
colnames(data_cslv)[colnames(data_cslv) == "Genus"] <- "genus"
colnames(data_cslv)[colnames(data_cslv) == "Species"] <- "specificEpithet"

crypto_families = c("Tripterygiidae", "Grammatidae", "Creediidae", "Aploactinidae", "Gobiidae", 
                    "Chaenopsidae", "Gobiesocidae", "Labrisomidae", "Pseudochromidae", "Bythitidae", 
                    "Plesiopidae", "Blenniidae", "Apogonidae", "Callionymidae", "Opistognathidae", "Syngnathidae")


# Check which families are present in data_cslv
present_families <- intersect(crypto_families, unique(data_cslv$family))
present_families <- intersect(crypto_families, unique(data_port$family))
present_families


# Step 2: Merge the two tables on the scientific name
data_merged_full <- merge(data_port, data_cslv, by = c("scientificName", "identificationRemarks", "class", "order", "family", "genus", "specificEpithet"), all = TRUE)

data_merged_full <- data_merged_full[,-c(8:11, 17, 18)]

data_merged_full

#### beta-div taxonomic ####
library(dplyr)
library(tidyr)
library(adespatial)

# Step 1: keep only the necessary columns
data_subset <- data_merged_full %>%
  dplyr::select(scientificName, HC, CM, HM, SM, X06_06_2021, X06_07_2021, X06_08_2021, X06_09_2021, X02_10_2022, X02_11_2022)

# Step 2: identify duplicates and rename them
data_subset <- data_subset %>%
  group_by(scientificName) %>%
  mutate(scientificName = make.unique(scientificName, sep = "_")) %>%
  ungroup()

# Step 3: pivot to long format (sites as rows)
data_long <- data_subset %>%
  pivot_longer(cols = HC:X02_11_2022, names_to = "Site", values_to = "Presence")

# Step 4: pivot to wide format (one scientific name per column)
data_wide <- data_long %>%
  pivot_wider(names_from = scientificName, values_from = Presence, values_fill = 0)

data_wide[is.na(data_wide)] <- 0

data_matrix_wide <- as.data.frame(data_wide)

rownames(data_matrix_wide) <- data_matrix_wide$Site
data_matrix_wide$Site <- NULL

jaccard <- data_matrix_wide
jaccard[,] <- lapply(jaccard[,], as.numeric)

# Create a vector of years from rownames
annees <- ifelse(grepl("2021", rownames(jaccard)), "2021",
                 ifelse(grepl("2022", rownames(jaccard)), "2022", rownames(jaccard)))

# Aggregate by year
jaccard_par_annee <- aggregate(jaccard, by = list(annee = annees), FUN = function(x) as.numeric(any(x > 0)))

rownames(jaccard_par_annee) <- jaccard_par_annee$annee

jaccard_par_annee[,] <- lapply(jaccard_par_annee[,], as.numeric)
jaccard_par_annee[is.na(jaccard_par_annee)] <- 0

# Calculate beta diversity
JAC <- beta.div.comp(jaccard_par_annee, quant = FALSE, save.abc = FALSE)
JAC

JACD <- JAC$D
JACD

JAC_sim <- 1 - JACD
JACturn <- JAC$repl
JACnest <- JAC$rich

#### beta-div phylogenetic ####
library(dplyr)
library(tidyr)

# Start from data_merged_full, pivot longer on site columns
sites <- c("HC", "CM", "HM", "SM", "X06_06_2021", "X06_07_2021", "X06_08_2021", 
           "X06_09_2021", "X02_10_2022", "X02_11_2022")

data_long <- data_merged_full %>%
  pivot_longer(
    cols = all_of(sites),
    names_to = "site",
    values_to = "occurrence"
  ) %>%
  # Keep only positive occurrences (presence)
  filter(!is.na(occurrence) & occurrence != 0) %>%
  # Keep only one row per scientificName + site
  distinct(scientificName, site, .keep_all = TRUE)

data_annee <- data_long %>%
  mutate(
    annee = case_when(
      grepl("2021", site) ~ "2021",
      grepl("2022", site) ~ "2022",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(scientificName, annee) %>%
  summarise(
    Presence = as.numeric(any(occurrence > 0)),
    across(where(~ !is.list(.)), ~ first(.)),  # keep non-list columns, first value
    .groups = "drop"
  )

taxa_list = tibble::tibble(
  species = data_annee$scientificName,
  genus = data_annee$genus,
  family = data_annee$family,
  order = data_annee$order
)

sp.list <- taxa_list %>%
  dplyr::select(species, genus, family, order) %>%
  dplyr::mutate(
    species.relative = NA,
    genus.relative = NA
  )

library(ape)

# Load megatrees for different vertebrate groups
megatree_fish <- read.tree("phylogeny/fish_megatree.tre")
megatree_mm <- read.tree("phylogeny/mammal_megatree.tre")
megatree_oi <- read.tree("phylogeny/bird_megatree.tre")
megatree_amp <- read.tree("phylogeny/amphibian_megatree.tre")
megatree_rep <- read.tree("phylogeny/reptile_megatree.tre")

# Combine trees into one
combined_tree <- bind.tree(megatree_fish, megatree_mm)
combined_tree <- bind.tree(combined_tree, megatree_oi)
combined_tree <- bind.tree(combined_tree, megatree_amp)

# Load genus lists
gen.list_fish <- read.csv("phylogeny/fish_genus_list.csv")
gen.list_mm <- read.csv("phylogeny/mammal_genus_list.csv")
gen.list_oi <- read.csv("phylogeny/bird_genus_list.csv")
gen.list_amp <- read.csv("phylogeny/amphibian_genus_list.csv")

# Merge genus lists
gen.list <- rbind(gen.list_fish, gen.list_mm, gen.list_oi, gen.list_amp)

# Generate phylogeny from species list
library(U.PhyloMaker)
result <- phylo.maker(sp.list, combined_tree, gen.list, nodes.type = 1, scenario = 3)
result

phylo <- result$phylo
phylo
plot(phylo)

library(phangorn)

phylo_rooted <- midpoint(phylo)
plot(phylo_rooted, show.tip.label = FALSE)
library(ggtree)
ggtree(phylo_rooted, layout = "circular") + geom_tiplab(size = 2)

colnames(jaccard_par_annee) <- gsub(" ", "_", colnames(jaccard_par_annee))

# Column names in your dissimilarity matrix
colnames(jaccard_par_annee)

# Tip labels in your tree
phylo$tip.label

library(picante)
unifrac <- unifrac(jaccard_par_annee, phylo_rooted)
unifrac

unifrac_sim <- 1 - unifrac

library(betapart)
jaccard_par_annee <- jaccard_par_annee[ ,-1]
phylobeta <- phylo.beta.pair(jaccard_par_annee, phylo_rooted, index.family = "jaccard")

phylobeta

phylobetaturn <- phylobeta$phylo.beta.jtu
phylobetanest <- phylobeta$phylo.beta.jne

# Global sums for turnover, nestedness, and total dissimilarity
total_turnover <- sum(phylobetaturn)  # Total turnover contribution
total_nestedness <- sum(phylobetanest)  # Total nestedness contribution
total_dissimilarity <- total_turnover + total_nestedness  # Total dissimilarity

# Calculate percentages
percent_turnover <- (total_turnover / total_dissimilarity) * 100
percent_nestedness <- (total_nestedness / total_dissimilarity) * 100

# Display results
cat("Global percentage of turnover (phylo.beta.jtu):", round(percent_turnover, 2), "%\n")
cat("Global percentage of nestedness (phylo.beta.jne):", round(percent_nestedness, 2), "%\n")


#### PCA beta-div ####
# Function to calculate mean pairwise β-diversity for each station
mean_pairwise_beta <- function(mat) {
  mat <- as.matrix(mat)
  station_means <- rowMeans(mat, na.rm = TRUE)  # Mean of each row
  return(station_means)
}

# Apply the function to each β-diversity matrix
Bjne_Phyl <- mean_pairwise_beta(phylobetanest)

Bjtu_Phyl <- mean_pairwise_beta(phylobetaturn)

Bjtu_Tax <- mean_pairwise_beta(JACturn)

Bjne_Tax <- mean_pairwise_beta(JACnest)

# Combine the mean β-diversity matrices into a single matrix
combined_matrix <- cbind(Bjne_Phyl, Bjtu_Phyl, Bjtu_Tax, Bjne_Tax)

combined_matrix
rownames(combined_matrix) <- c(
  "LwC_2021",
  "LwC_2022",
  "Coch_mang",
  "Har_core",
  "Har_mang",
  "Seagrass"
)

# Station names
statnam <- c("HC", "CM", "HM", "SM","X06_06_2021", "X06_07_2021", "X06_08_2021", "X06_09_2021", "X02_10_2022", "X02_11_2022")
statnam <- c("Har_core", "Coch_mang", "Har_mang", "Seagrass","LwC_2021", "LwC_2022")

# PCA
res.pca <- prcomp(combined_matrix)
library(factoextra)
RES.PCA <- fviz_pca_biplot(res.pca, habillage = statnam, labelsize = 5, repel = TRUE, legend.title = "Sample", title = NULL) +
  theme(text = element_text(size = 15))
RES.PCA

#### Turnover/nestedness matrix ####
library(pheatmap)

# Example: JAC$repl and JAC$rich are your triangular matrices

# 1. Convert to matrices
jac_turn <- as.matrix(JAC$repl)
jac_nest <- as.matrix(JAC$rich)

# 2. Complete each matrix to make them symmetric
complete_symmetric <- function(mat) {
  full <- mat
  full[upper.tri(full)] <- t(mat)[upper.tri(mat)]
  diag(full) <- 0  # Optional: or NA if preferred
  return(full)
}

jac_turn_full <- complete_symmetric(jac_turn)
jac_nest_full <- complete_symmetric(jac_nest)

# 3. Function to combine turnover and nestedness into a single matrix
combine_beta_matrices <- function(turn_mat, nest_mat) {
  stopifnot(all(dim(turn_mat) == dim(nest_mat)))
  stopifnot(all(rownames(turn_mat) == rownames(nest_mat)))
  n <- nrow(turn_mat)
  combined <- matrix(NA, n, n)
  rownames(combined) <- rownames(turn_mat)
  colnames(combined) <- colnames(turn_mat)
  
  combined[upper.tri(combined)] <- turn_mat[upper.tri(turn_mat)]
  combined[lower.tri(combined)] <- nest_mat[lower.tri(nest_mat)]
  diag(combined) <- NA  # or 0 if you want to display the diagonal
  
  return(combined)
}

# 4. Apply the function
beta_taxo_combined <- combine_beta_matrices(jac_turn_full, jac_nest_full)

# 5. Visualization (optional but publication-ready)
pheatmap(
  beta_taxo_combined,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  main = "Taxonomic β-diversity",
  fontsize = 12
)

phyl_turn_full <- complete_symmetric(as.matrix(phylobetaturn))
phyl_nest_full <- complete_symmetric(as.matrix(phylobetanest))

beta_phylo_combined <- combine_beta_matrices(phyl_turn_full, phyl_nest_full)

pheatmap(
  beta_phylo_combined,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  display_numbers = TRUE,
  number_format = "%.2f",
  main = "Phylogenetic β-diversity",
  fontsize = 12
)

# Load required libraries
library(factoextra)
library(pheatmap)
library(grid)
library(patchwork)  # for visual layout

# === 1. Generate heatmaps ===

# Create symmetric matrices
complete_symmetric <- function(mat) {
  full <- mat
  full[upper.tri(full)] <- t(mat)[upper.tri(mat)]
  diag(full) <- NA
  return(full)
}

combine_beta_matrices <- function(turn_mat, nest_mat) {
  stopifnot(all(dim(turn_mat) == dim(nest_mat)))
  n <- nrow(turn_mat)
  combined <- matrix(NA, n, n)
  rownames(combined) <- rownames(turn_mat)
  colnames(combined) <- colnames(turn_mat)
  combined[upper.tri(combined)] <- turn_mat[upper.tri(turn_mat)]
  combined[lower.tri(combined)] <- nest_mat[lower.tri(nest_mat)]
  diag(combined) <- NA
  return(combined)
}

# Taxonomic matrices
jac_turn_full <- complete_symmetric(as.matrix(JAC$repl))
jac_nest_full <- complete_symmetric(as.matrix(JAC$rich))
beta_taxo_combined <- combine_beta_matrices(jac_turn_full, jac_nest_full)

# Phylogenetic matrices
phyl_turn_full <- complete_symmetric(as.matrix(phylobetaturn))
phyl_nest_full <- complete_symmetric(as.matrix(phylobetanest))

beta_phylo_combined <- combine_beta_matrices(phyl_turn_full, phyl_nest_full)

# Generate heatmaps as grob objects (grid graphics)
grob_taxo <- grid::grid.grabExpr(
  pheatmap(
    beta_taxo_combined,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    display_numbers = TRUE,
    number_format = "%.2f",
    main = "Taxonomic β-diversity",
    fontsize = 10
  )
)

grob_phylo <- grid::grid.grabExpr(
  pheatmap(
    beta_phylo_combined,
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    display_numbers = TRUE,
    number_format = "%.2f",
    main = "Phylogenetic β-diversity",
    fontsize = 10
  )
)

# === 2. Generate PCA ===

# PCA (already in your code)
res.pca <- prcomp(combined_matrix, scale. = TRUE)
statnam <- rownames(combined_matrix)

RES.PCA <- fviz_pca_biplot(
  res.pca,
  habillage = statnam,
  labelsize = 5,
  repel = TRUE,
  legend.title = "Sample",
  title = NULL
) + 
  theme_minimal(base_size = 15)

RES.PCA

# === 3. Combine the three plots ===

# Convert grobs to patchwork-compatible objects
wrap_grob <- function(grob) {
  ggplot() + theme_void() + annotation_custom(grob)
}

final_plot <- RES.PCA / (wrap_grob(grob_taxo) | wrap_grob(grob_phylo)) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Visual summary of β-diversity between sites")

# Display
print(final_plot)

RES.PCA

# Generate clean biplot, minimal dependencies
RES.PCA <- fviz_pca_biplot(
  res.pca,
  habillage = statnam,        # Group color
  label = "all",              # Show variables only (set to "ind" for individuals)
  repel = TRUE,               # Avoid overlapping labels
  addEllipses = TRUE,         # Add 95% ellipses
  ellipse.level = 0.95,
  pointsize = 3,              # Point size (if individuals shown)
  labelsize = 4,              # Text size
  legend.title = "Sample",    # Legend title
  title = NULL
) +
  scale_color_viridis_d(option = "viridis") +  # Clean colors
  theme_minimal(base_size = 16) +             # Clean theme
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    axis.line = element_line(color = "black")
  )

RES.PCA <- fviz_pca_biplot(
  res.pca,
  label = "none",         # Remove all labels (variables and individuals)
  habillage = statnam,    # Group color (remains for points)
  repel = TRUE,           # Avoid overlap (not needed if no labels)
  addEllipses = TRUE,
  ellipse.level = 0.95,
  pointsize = 3,
  title = NULL
) +
  scale_color_viridis_d(option = "viridis") +  
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "none",           # Remove legend
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.7),
    axis.line = element_line(color = "black")
  )

RES.PCA

# Export to PNG (high-resolution raster)
ggsave("RES_PCA.png", RES.PCA, width = 7, height = 5, dpi = 300)


#### Faith's PD and SES ####
#### Calculation of PD

rownames(jaccard_par_annee) <- jaccard_par_annee$annee

jaccard_par_annee[,] <- lapply(jaccard_par_annee[,], as.numeric)
jaccard_par_annee[is.na(jaccard_par_annee)] <- 0

colnames(jaccard_par_annee) <- gsub(" ", "_", colnames(jaccard_par_annee))
rownames(jaccard_par_annee) <- jaccard_par_annee$annee

pd.result <- pd(jaccard_par_annee, phylo_rooted, include.root = TRUE)

# Check results
print(pd.result)

phydist <- cophenetic(phylo)

# Extract common species
species_common <- intersect(colnames(jaccard_par_annee), phylo$tip.label)

# Filter jaccard_par_annee and phydist to keep only these species
jaccard_par_annee_filt <- jaccard_par_annee[, species_common]
phydist_filt <- cophenetic(phylo)[species_common, species_common]

# Re-run calculation
ses.mpd.result <- ses.mpd(
  jaccard_par_annee_filt,
  phydist_filt,
  null.model = "taxa.labels",
  abundance.weighted = FALSE,
  runs = 1000
)
ses.mpd.result

# Calculate the standardized effect size of phylogenetic diversity for fish
ses.pd.result <- ses.pd(jaccard_par_annee, phylo_rooted, null.model = "taxa.labels", run = 1000)
ses.pd.result

library(picante)
library(ggplot2)
library(dplyr)
library(tidyr)

rownames(jaccard_par_annee) <- jaccard_par_annee$annee
jaccard_par_annee[,] <- lapply(jaccard_par_annee[,], as.numeric)
jaccard_par_annee[is.na(jaccard_par_annee)] <- 0

# --- Clean column and row names ---
colnames(jaccard_par_annee) <- gsub(" ", "_", colnames(jaccard_par_annee))
rownames(jaccard_par_annee) <- jaccard_par_annee$annee

# --- Check species match ---
species_common <- intersect(colnames(jaccard_par_annee), phylo_rooted$tip.label)

# Filter presence/absence matrix and tree
jaccard_par_annee_filt <- jaccard_par_annee[, species_common]
phydist_filt <- cophenetic(phylo_rooted)[species_common, species_common]

# --- Calculate indices ---
# PD
pd.result <- pd(jaccard_par_annee_filt, phylo_rooted, include.root = TRUE)

# SES PD
ses.pd.result <- ses.pd(
  jaccard_par_annee_filt,
  phylo_rooted,
  null.model = "taxa.labels",
  runs = 1000
)
ses.pd.result

# SES MPD
ses.mpd.result <- ses.mpd(
  jaccard_par_annee_filt,
  phydist_filt,
  null.model = "taxa.labels",
  abundance.weighted = FALSE,
  runs = 1000
)

# SES MNTD
ses.mntd.result <- ses.mntd(
  jaccard_par_annee_filt,
  phydist_filt,
  null.model = "taxa.labels",
  abundance.weighted = FALSE,
  runs = 1000
)

# --- Merge results into a single table ---
# Convert results to data.frame with a 'year' column
pd_df <- pd.result %>%
  as.data.frame() %>%
  mutate(annee = rownames(.))

ses_pd_df <- ses.pd.result %>%
  as.data.frame() %>%
  mutate(annee = rownames(.)) %>%
  select(annee, pd.obs.z)

ses_mpd_df <- ses.mpd.result %>%
  as.data.frame() %>%
  mutate(annee = rownames(.)) %>%
  select(annee, mpd.obs.z)

ses_mntd_df <- ses.mntd.result %>%
  as.data.frame() %>%
  mutate(annee = rownames(.)) %>%
  select(annee, mntd.obs.z)

# Merge
recap <- pd_df %>%
  left_join(ses_pd_df, by = "annee") %>%
  left_join(ses_mpd_df, by = "annee") %>%
  left_join(ses_mntd_df, by = "annee")

colnames(recap) <- c("PD", "SR", "annee", "SES_PD", "SES_MPD", "SES_MNTD")
print(recap)

# --- Format for figure ---
recap_long <- recap %>%
  pivot_longer(cols = c(PD, SES_PD, SES_MPD, SES_MNTD), names_to = "Index", values_to = "Value")
recap_long

library(tidyverse)
library(ggpubr)

# --- Prepare observed values ---
obs_mpd_df <- ses.mpd.result %>%
  as.data.frame() %>%
  mutate(Site = rownames(.)) %>%
  select(Site, mpd.obs)

obs_mntd_df <- ses.mntd.result %>%
  as.data.frame() %>%
  mutate(Site = rownames(.)) %>%
  select(Site, mntd.obs)

recap_obs <- recap %>%
  rename(Site = annee) %>%
  left_join(obs_mpd_df, by = "Site") %>%
  left_join(obs_mntd_df, by = "Site") %>%
  select(Site, PD, mpd.obs, mntd.obs)

recap_obs <- recap %>%
  rename(Site = annee) %>%
  left_join(obs_mpd_df, by = "Site") %>%
  left_join(obs_mntd_df, by = "Site") %>%
  select(Site, SR, PD, mpd.obs, mntd.obs)

recap_obs_long <- recap_obs %>%
  pivot_longer(cols = c(PD, mpd.obs),
               names_to = "Metric", values_to = "Observed") %>%
  mutate(Metric = recode(Metric,
                         "PD" = "PD",
                         "mpd.obs" = "MPD"))

# --- Long data: SR, PD, MPD ---
recap_obs_long <- recap_obs %>%
  pivot_longer(cols = c(SR, PD, mpd.obs),
               names_to = "Metric", values_to = "Observed") %>%
  mutate(Metric = recode(Metric,
                         "SR" = "SR",
                         "PD" = "PD",
                         "mpd.obs" = "MPD"),
         Metric = factor(Metric, levels = c("SR", "PD", "MPD")))

# --- Prepare SES data with significance ---
ses_pd_df <- ses.pd.result %>%
  as.data.frame() %>%
  mutate(Site = rownames(.)) %>%
  select(Site, z = pd.obs.z, p = pd.obs.p) %>%
  mutate(Metric = "PD")

ses_mpd_df <- ses.mpd.result %>%
  as.data.frame() %>%
  mutate(Site = rownames(.)) %>%
  select(Site, z = mpd.obs.z, p = mpd.obs.p) %>%
  mutate(Metric = "MPD")

ses_mntd_df <- ses.mntd.result %>%
  as.data.frame() %>%
  mutate(Site = rownames(.)) %>%
  select(Site, z = mntd.obs.z, p = mntd.obs.p) %>%
  mutate(Metric = "MNTD")

recap_ses_long <- bind_rows(ses_pd_df, ses_mpd_df) %>%
  mutate(Significant = ifelse(p < 0.05 | p > 0.95, "Yes", "No"))

site_names <- c(
  "HC" = "Har_core",
  "CM" = "Coch_mang",
  "HM" = "Har_mang",
  "SM" = "Seagrass",
  "2021" = "LwC_2021",
  "2022" = "LwC_2022"
)

# Apply to observed values
recap_obs_long <- recap_obs_long %>%
  mutate(Site = recode(Site, !!!site_names))

# Apply to SES
recap_ses_long <- recap_ses_long %>%
  mutate(Site = recode(Site, !!!site_names))

# --- Panel A: Barplot of observed values with facets ---
plot_obs <- ggplot(recap_obs_long, aes(x = Site, y = Observed, fill = Metric)) +
  geom_col() +
  facet_wrap(~ Metric, scales = "free_x") +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(title = "Observed phylogenetic metrics",
       x = "Site", y = "Observed value") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# --- Panel B: SES scores with significance and thresholds ---
library(ggplot2)

plot_ses <- ggplot(recap_ses_long, aes(x = reorder(Site, z), y = z, color = Metric)) +
  geom_hline(yintercept = c(-1.96, 1.96), linetype = "dashed", color = "grey40") +
  geom_point(aes(shape = Significant), size = 3, position = position_dodge(width = 0.6)) +
  coord_flip() +
  theme_classic(base_size = 14) +
  labs(title = "Standardized Effect Sizes (SES)",
       x = "Site", y = "SES (z-score)") +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = c("Yes" = 17, "No" = 16)) +
  theme(legend.position = "top")
plot_ses

# --- Combine panels ---
library(patchwork)

final_plot <- plot_obs + plot_ses + 
  plot_layout(ncol = 2, widths = c(1.3, 1.6)) +
  plot_annotation(tag_levels = "A")

final_plot



#### Phylogenetic tree with colors ####
# --- LIBRARIES ---
library(ggtree)
library(ggplot2)
library(dplyr)
library(patchwork)
library(ape)

# --- 1. Prepare the data ---
df_long <- data_annee %>%
  mutate(label = gsub(" ", "_", scientificName)) %>%
  filter(label %in% phylo_rooted$tip.label,
         occurrence == 1) %>%
  distinct(label, site) %>%
  mutate(site_grouped = case_when(
    grepl("2021", site) ~ "2021",
    grepl("2022", site) ~ "2022",
    TRUE ~ site
  ))

# Color palette by site
station_colors <- setNames(
  hue_pal()(length(unique(df_long$site_grouped))),
  unique(df_long$site_grouped)
)

phylo_rooted$edge.length[phylo_rooted$edge.length < 0] <- 0
ggtree(phylo_rooted, layout = "circular") +
  geom_tiplab(size = 2)

# --- 2. Function for mini circular tree ---
df_long <- df_long %>%
  mutate(site_grouped = factor(site_grouped, levels = c("2021", "2022", "CM", "HC", "HM", "SM")))

plot_tree_for_site <- function(site_name) {
  # Taxa detected for this site
  df_site <- df_long %>% filter(site_grouped == site_name)
  detected_tips <- df_site$label
  
  # Full rectangular tree
  p <- ggtree(phylo_rooted, layout = "rectangular", ladderize = FALSE)
  
  # Add only colored points for detected taxa
  p <- p +
    geom_tippoint(
      data = function(x) dplyr::filter(x, label %in% detected_tips),
      color = station_colors[site_name],
      size = 3
    ) +
    ggtitle(site_name) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  return(p)
}

# --- 3. Generate all mini trees and assemble ---
plots_list <- lapply(levels(df_long$site_grouped), plot_tree_for_site)
final_plot <- wrap_plots(plots_list, ncol = 3)
print(final_plot)

library(ggtree)
library(dplyr)
library(ggplot2)
library(patchwork)

# --- 1. Prepare a table with label and order ---
df_orders <- data_annee %>%
  mutate(label = gsub(" ", "_", scientificName)) %>%
  dplyr::select(label, order) %>%
  distinct()

# --- 2. Function for mini tree with detected order annotation ---
plot_tree_for_site <- function(site_name) {
  # Taxa detected for this site
  df_site <- df_long %>% filter(site_grouped == site_name)
  detected_tips <- df_site$label
  
  # Detected orders
  detected_orders <- df_orders %>% 
    filter(label %in% detected_tips) %>% 
    distinct(order) %>%
    pull(order)
  
  # Full tree
  p <- ggtree(phylo_rooted, layout = "rectangular", ladderize = FALSE)
  
  # Colored points for detected taxa
  p <- p +
    geom_tippoint(
      data = function(x) dplyr::filter(x, label %in% detected_tips),
      color = station_colors[site_name],
      size = 3
    ) +
    ggtitle(site_name) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Attach orders to the tree
  p <- p %<+% df_orders
  
  # Add detected order names
  for (ord in detected_orders) {
    tips_ord <- df_orders %>% filter(order == ord, label %in% detected_tips) %>% pull(label)
    mrca_ord <- getMRCA(phylo_rooted, tips_ord) # MRCA node of the group
    
    if (!is.null(mrca_ord)) {
      p <- p + geom_cladelab(
        node = mrca_ord,
        label = ord,
        align = TRUE,
        offset = 1.5,       # Increase distance to avoid cut text
        barsize = 0.6,      # Clade bar size
        fontsize = 3.5,     # Text size
        hjust = 0           # Left alignment
      )
    }
  }
  
  return(p)
}

# --- 3. Generate all mini trees and assemble ---
plots_list <- lapply(levels(df_long$site_grouped), plot_tree_for_site)
final_plot <- wrap_plots(plots_list, ncol = 3)
print(final_plot)

# --- 1. Prepare data with CSLV / GPMG grouping ---
library(scales)
df_long <- data_annee %>%
  mutate(
    label = gsub(" ", "_", scientificName),
    site_grouped = ifelse(grepl("20", site), "CSLV", "GPMG")
  ) %>%
  filter(label %in% phylo_rooted$tip.label,
         occurrence == 1) %>%
  distinct(label, site_grouped)

# Color palette by group
station_colors <- setNames(
  hue_pal()(length(unique(df_long$site_grouped))),
  unique(df_long$site_grouped)
)

# --- 2. Prepare table with label and order ---
df_orders <- data_annee %>%
  mutate(label = gsub(" ", "_", scientificName)) %>%
  select(label, order) %>%
  distinct()

# --- 3. Function for mini tree with detected order annotation ---
plot_tree_for_site <- function(site_name) {
  # Taxa detected for this site
  df_site <- df_long %>% filter(site_grouped == site_name)
  detected_tips <- df_site$label
  
  # Detected orders
  detected_orders <- df_orders %>% 
    filter(label %in% detected_tips) %>% 
    distinct(order) %>%
    pull(order)
  
  # Full tree
  p <- ggtree(phylo_rooted, layout = "rectangular", ladderize = FALSE)
  
  # Colored points for detected taxa
  p <- p +
    geom_tippoint(
      data = function(x) dplyr::filter(x, label %in% detected_tips),
      color = station_colors[site_name],
      size = 3
    ) +
    ggtitle(site_name) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  # Attach orders to the tree
  p <- p %<+% df_orders
  
  # Add detected order names
  for (ord in detected_orders) {
    tips_ord <- df_orders %>% filter(order == ord, label %in% detected_tips) %>% pull(label)
    mrca_ord <- getMRCA(phylo_rooted, tips_ord) # MRCA node of the group
    
    if (!is.null(mrca_ord)) {
      p <- p + geom_cladelab(
        node = mrca_ord,
        label = ord,
        align = TRUE,
        offset = 3,     # larger for more space
        barsize = 0.6,
        fontsize = 3.5,
        hjust = 0
      )
    }
  }
  
  return(p)
}

# --- 4. Generate all mini trees and assemble ---
plots_list <- lapply(unique(df_long$site_grouped), plot_tree_for_site)
final_plot <- wrap_plots(plots_list, ncol = 2)
print(final_plot)

library(dplyr)
library(ggtree)
library(ggplot2)

# 1. Create a clean presence_type column
df_presence <- df_long %>%
  group_by(label) %>%
  summarise(sites_list = list(unique(site_grouped)), .groups = "drop") %>%
  mutate(presence_type = sapply(sites_list, function(x) {
    if(all(c("CSLV", "GPMG") %in% x)) {
      "Both"
    } else if("CSLV" %in% x) {
      "CSLV"
    } else if("GPMG" %in% x) {
      "GPMG"
    } else {
      NA_character_
    }
  }))

# 2. Join presence_type to df_orders
df_orders_presence <- df_orders %>%
  left_join(df_presence %>% dplyr::select(label, presence_type), by = "label") %>%
  filter(!is.na(presence_type))

# 3. Fixed 3-color palette
presence_colors <- c(
  "CSLV" = "lightblue",
  "GPMG" = "blue",
  "Both" = "orange"
)

p <- ggtree(phylo_rooted, layout = "rectangular", ladderize = FALSE) %<+% df_orders_presence

p <- p +
  geom_tippoint(aes(color = presence_type), size = 2) +
  scale_color_manual(values = presence_colors, name = "Presence") +
  guides(color = "none")+
  theme_void() +
  theme(
    legend.position = "right",
    plot.margin = margin(10, 100, 10, 10),
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_cartesian(clip = "off")

# 1. Find MRCA and y position for each order
ord_positions <- lapply(unique(df_orders_presence$order), function(ord) {
  tips_ord_all <- df_orders_presence %>%
    filter(order == ord) %>%
    pull(label)
  tips_ord <- tips_ord_all[tips_ord_all %in% phylo_rooted$tip.label]
  if (length(tips_ord) > 1) {
    mrca <- getMRCA(phylo_rooted, tips_ord)
    if (!is.null(mrca)) {
      y_pos <- p$data$y[p$data$node == mrca]
      return(data.frame(order = ord, node = mrca, y = y_pos))
    }
  }
  return(NULL)
})

ord_positions_df <- do.call(rbind, ord_positions)

# 2. Sort by y (vertical position)
ord_positions_df <- ord_positions_df[order(ord_positions_df$y, decreasing = TRUE), ]

# 3. Exclude certain orders
orders_to_exclude <- c("Perciformes", "Eupercaria incertae sedis")
ord_positions_df <- ord_positions_df[!ord_positions_df$order %in% orders_to_exclude, ]

# 4. Add brackets with progressive offset
offset_base <- 3.5
offset_step <- 0.3

for (i in seq_len(nrow(ord_positions_df))) {
  row <- ord_positions_df[i, ]
  p <- p + geom_cladelab(
    node = row$node,
    label = row$order,
    align = TRUE,
    offset = offset_base + (i - 1) * offset_step,
    barsize = 0.8,
    fontsize = 3.5,
    hjust = 0,
    extend = 0
  )
}

print(p)

# Output file name
output_file <- "phylo_tree.png"

# Export as PNG, 300 dpi
ggsave(
  filename = output_file,
  plot = p,
  width = 12,       # width in inches
  height = 12,      # height in inches
  dpi = 300         # resolution
)
