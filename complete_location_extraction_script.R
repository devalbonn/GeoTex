##############################################################################################################
# complete script for extraction of place names, geocoding and subsequent matching with black and white list #
##############################################################################################################



# required libraries and functions to execute the script
# readtext for importing text files 
library(readtext)
# spacyr to use spacy for pre processing texts
library(spacyr)
# cld2 to do laguage detection of texts
library(cld2)
# tmaptools to send queries to open street map for geocoding
library(tmaptools)
# geospehere to calculate distances between locations and document locations centroid during clustering algorithm
library(geosphere)
# function for extracting locatiosn from text using spaCy NER
source("functions/extract_locations_from_text.R")
# function to extract tags from spacy parsed object
source("functions/extract_tags.R")
# function to apply kmeans clustering algorithm on location distances to location centroid to determine confidence
source("functions/location_clustering_for_confidence.R")

##################################################################################
# import the text files that are located in the directory @import_file_directory #
##################################################################################

# set @import_file_directory; default="files_to_be_imported"
import_file_directory<-"files_to_be_imported"

# get all files in import directory
import_file_paths<-list.files(path = import_file_directory, full.names = TRUE)

# create a list with the length of files to be imported, containing the imported texts and the file names
imported_files<-vector("list",length(import_file_paths))
# loop over all import files; set file_name, text (using readtext package) and update progress bar @pb
for(input_file_index in 1:length(import_file_paths)){
  imported_files[[input_file_index]]$file_name<-import_file_paths[input_file_index]
  imported_files[[input_file_index]]$text<-readtext(file = imported_files[[input_file_index]]$file_name)$text
}
print("finished importing files to R")


#############################################################################################
# language detection of texts in order to choose the correct spaCy model for pre-processing #
#############################################################################################

# get languages for every imported text and add  it to each list with element name language
for(i in 1:length(imported_files)){
  imported_files[[i]]$language<-cld2::detect_language_mixed(imported_files[[i]]$text)$classificaton
}
print("finished language detection for all files")


#########################################################
# extract locations from texts; use spacy NER locations #
#########################################################
#@ imported_files[[index]]locations: a dataframe of found locations for corresponding import file
for(i in 1:length(imported_files)){
  imported_files[[i]]$locations<-extract_locations_from_text(text = imported_files[[i]]$text,language = imported_files[[i]]$language,threshold_for_multiple_models = 0.3)
}
print("finished extraction all location names using spaCy")



############################################################
# get coordinates for found locations from open street map #
############################################################

for(i in 1:length(imported_files)){
  print(i)
  imported_files[[i]]$coordinates<-tmaptools::geocode_OSM(q = as.character(imported_files[[1]]$locations$location),
                                                          server ="https://nominatim.openstreetmap.org",details = T,as.data.frame = T)
  imported_files[[i]]$coordinates<-merge(x = imported_files[[i]]$coocdinates,y = imported_files[[i]]$locations,by.x = "query", by.y =  "location")
  imported_files[[i]]$coordinates<-imported_files[[i]]$coocdinates[,c("query","frequency","lat","lon","class","type","importance")]
}


##################################################################################################
# filter certain location with specific values in metadata field type and class; E.g. shopping.. #
##################################################################################################







#########################################################################
# apply clustering algorithm over all documents to calculate confidence #
#########################################################################

for(i in 1:length(imported_files)){
  print(i)
  confidence<-apply_clustering_algorithm_on_locations_to_get_confidence_scores(names = imported_files[[i]]$coordinates$query,
                                                                               frequencies = imported_files[[i]]$coordinates$frequency,
                                                                               lat = imported_files[[i]]$coordinates$lat,
                                                                               lon = imported_files[[i]]$coordinates$lon)
  imported_files[[i]]$coordinates<-cbind(imported_files[[i]]$coordinates,confidence)

}



#############################
# comparison with blacklist #
#############################




###################################
# create/append whitelist entries #
###################################




########################################
# extraction of metadata using regexps #
########################################













