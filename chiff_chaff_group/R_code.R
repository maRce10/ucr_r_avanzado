
# read data
annotations <- read_excel(path = "./chiff_chaff_group/selections_chiff_chaff.xlsx")

annotations$start <- as.numeric(annotations$start)

annotations$start <- ifelse(annotations$start > 10000, annotations$start / 1000000000, annotations$start)

annotations$end <- as.numeric(annotations$end)

annotations$end <- ifelse(annotations$end > 10000, annotations$end / 1000000000, annotations$end)

annotations$bottom.freq <- annotations$bottom.freq / 1000000 
annotations$top.freq <- annotations$top.freq / 1000000 

# check data
head(annotations)
View(annotations)

# for (i in unique(annotations$`xc-id`)){
#   print(i)
#   if (!file.exists(paste0("./chiff_chaff_group/Phylloscopus-collybita-", i, ".mp3"))){
#    
# try(query_xc(qword = paste0("nr:", i), download = TRUE, path = "./chiff_chaff_group/", pb = FALSE), silent = TRUE)
#   }
#   }


sound_file_path <- "./chiff_chaff_group/"

cs <- check_sels(annotations, path = sound_file_path)

### this is for chiffs
chiffs <- annotations[annotations$Annotation == "Chiff", ]

sp <- spectro_analysis(X = chiffs, path = sound_file_path)


# run excluding sound file and selec columns
pca <- prcomp(sp[, -c(1, 2)])


# add first 2 PCs to sound file and selec columns
pca_data <- cbind(sp[, c(1, 2)], pca$x[, 1:2])

# read XC metadata
chiffs_mtdt <- read.csv("./chiff_chaff_group/metadata.csv")

# create a column with the file name in the metadata
chiffs_mtdt$sound.files <- paste0(chiffs_mtdt$Genus, "-", chiffs_mtdt$Specific_epithet, "-", chiffs_mtdt$Recording_ID, ".mp3")

# and merge based on sound files and any metadata column we need
pca_data_md <- merge(pca_data, chiffs_mtdt[, c("sound.files", "Country", "Latitude", "Longitude")])

# plot
ggplot(data = pca_data_md, aes(x = PC1, y = PC2, color = Country, shape = Country)) +
  ggtitle("Chiffs") +
  geom_point(size = 3) + 
  scale_color_viridis_d()

# plot
ggplot(data = pca_data_md, aes(x = PC1, y = PC2, color = Longitude, shape = Country)) +
  ggtitle("Chiffs") +
  geom_point(size = 3) +
  scale_color_viridis_c()


# create geographic and acoustic distance matrices
geo_dist <- dist(pca_data_md[, c("Latitude", "Longitude")])
acoust_dist <- dist(pca_data_md[, c("PC1", "PC2")])

# install.packages("vegan")
library(vegan)

# run test
mantel(geo_dist, acoust_dist)


### this is for chaffs ####
chaffs <- annotations[annotations$Annotation == "Chaff", ]

sp <- spectro_analysis(X = chaffs, path = sound_file_path)


# run excluding sound file and selec columns
pca <- prcomp(sp[, -c(1, 2)])


# add first 2 PCs to sound file and selec columns
pca_data <- cbind(sp[, c(1, 2)], pca$x[, 1:2])

# read XC metadata
chaffs_mtdt <- read.csv("./chiff_chaff_group/metadata.csv")

# create a column with the file name in the metadata
chaffs_mtdt$sound.files <- paste0(chaffs_mtdt$Genus, "-", chaffs_mtdt$Specific_epithet, "-", chaffs_mtdt$Recording_ID, ".mp3")

# and merge based on sound files and any metadata column we need
pca_data_md <- merge(pca_data, chaffs_mtdt[, c("sound.files", "Country", "Latitude", "Longitude")])

# install.packages("ggplot2")
library(ggplot2)

# install.packages("viridis")
library(viridis)

# plot
ggplot(data = pca_data_md, aes(x = PC1, y = PC2, color = Country, shape = Country)) +
  ggtitle("Chaffs") +
  geom_point(size = 3) + 
  scale_color_viridis_d()

# plot
ggplot(data = pca_data_md, aes(x = PC1, y = PC2, color = Longitude, shape = Country)) +
  ggtitle("Chaffs") +
  geom_point(size = 3) +
  scale_color_viridis_c()


# create geographic and acoustic distance matrices
geo_dist <- dist(pca_data_md[, c("Latitude", "Longitude")])
acoust_dist <- dist(pca_data_md[, c("PC1", "PC2")])

# install.packages("vegan")
library(vegan)

# run test
mantel(geo_dist, acoust_dist)

