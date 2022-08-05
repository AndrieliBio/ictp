#biodiversity databases
#Andrea Sánchez-Tapia
#27.07.22

library(rgbif)
library(Taxonstand)
library(CoordinateCleaner)
library(maps)
library(dplyr)

#Getting the data
species <- "Myrsine coriacea"
occs <- occ_search(scientificName = species,
                   limit = 100000)
names(occs)
myrsine.data <- occs$data
colnames(myrsine.data)

#Exporting raw data
#dir.create("data/raw/", recursive = TRUE)
write.csv(myrsine.data,
          "data/raw/myrsine_data.csv",
          row.names = FALSE)

#Checking speceis taxonomy
sort(unique(myrsine.data$scientificName))

table(myrsine.data$taxonomicStatus)
#Check the names accepted and the synonyms
table(myrsine.data$scientificName, myrsine.data$taxonomicStatus)

# generate a list with unique species names and combine it to the data.
#function TPL from from package taxonstand to check if the taxonomic updates
#in the gbif data are correct
species.names <- unique(myrsine.data$scientificName)
dim(species.names)
tax.check <- TPL(species.names)

# creating new object w/ original and new names after TPL
new.tax <- data.frame(scientificName = species.names,
                      genus.new.TPL = tax.check$New.Genus,
                      species.new.TPL = tax.check$New.Species,
                      status.TPL = tax.check$Taxonomic.status,
                      scientificName.new.TPL = paste(tax.check$New.Genus,
                                                     tax.check$New.Species))
# now we are merging raw data and checked data
myrsine.new.tax <- merge(myrsine.data, new.tax, by = "scientificName")


dir.create("data/processed/", recursive = TRUE)
write.csv(myrsine.new.tax,
          "data/processed/data_taxonomy_check.csv",
          row.names = FALSE)

#Checking species'coodinates
#"asp" altere the aexes

plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)

#This function checks for common errors in coordinates such as institutional
#coordinates, sea coordinates, outliers, zeros, centroids, etc.

#But first we have to remove the NA
myrsine.coord <- myrsine.data[!is.na(myrsine.data$decimalLatitude)
                              & !is.na(myrsine.data$decimalLongitude),]

# output w/ only potential correct coordinates
geo.clean <- clean_coordinates(x = myrsine.coord,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               species = "species",
                               value = "clean")

#Let’s plot the output of the clean data.

par(mfrow = c(1, 2))
plot(decimalLatitude ~ decimalLongitude, data = myrsine.data, asp = 1)
map(, , , add = TRUE)
plot(decimalLatitude ~ decimalLongitude, data = geo.clean, asp = 1)
maps::map(, , , add = TRUE)
par(mfrow = c(1, 1))

