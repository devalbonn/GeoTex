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
# rworldmap sp to convert coordiantes of locations to country and continent meta informations
library(rworldmap)
library(sp)
# function for extracting locatiosn from text using spaCy NER
source("functions/extract_locations_from_text.R")
# function to extract tags from spacy parsed object
source("functions/extract_tags.R")
# function to apply kmeans clustering algorithm on location distances to location centroid to determine confidence
source("functions/location_clustering_for_confidence.R")
# function to filter irrelevant locations from analysis; often false positive locations
source("functions/filter_irrelevant_locations.R")
# function to assign country and continent nformation to found locations absed on coordiantes
source("functions/coords_to_country_and_continent.R")
# function to condense whitelist by either name or coordiantes to have only unique locations left; confidence scores are beeing averaged
source("functions/condense_whitelist.R")

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
# for geocoding OpenSteetMap is used
# Google Geonames can no longer be used free of charge
# @pb progress bar for for loop
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0,style=3) 
for(i in 1:length(imported_files)){
  imported_files[[i]]$coordinates<-tmaptools::geocode_OSM(q = as.character(imported_files[[i]]$locations$location),
                                                          server ="https://nominatim.openstreetmap.org",details = T,as.data.frame = T)
  # merge frequency information to coordiantes dataframe
  imported_files[[i]]$coordinates<-merge(x = imported_files[[i]]$coordinates,y = imported_files[[i]]$locations,by.x = "query", by.y =  "location")
  # narrow down on necessary columns
  imported_files[[i]]$coordinates<-imported_files[[i]]$coordinates[,c("query","frequency","lat","lon","class","type","importance")]
  # update Progress bar
  setTxtProgressBar(pb,i)
}


##################################################################################################
# filter certain location with specific values in metadata field type and class; E.g. shopping.. #
##################################################################################################
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0,style=3) 
for(i in 1:length(imported_files)){
  # more types and classes can be added
  # explanations for classes and types can be found on: https://wiki.openstreetmap.org/wiki/Map_Features
  # add "all" if one of the two fields does not matter; e.g. c("tourism","all") will remove all locations that have the class "tourism" no matter the type
  remove_pairs<-data.frame(class="shop",type="all",stringsAsFactors = F)
  remove_pairs<-rbind(remove_pairs,c("tourism","all"))
  remove_pairs<-rbind(remove_pairs,c("highway","all"))
  remove_pairs<-rbind(remove_pairs,c("amenity","all"))
  remove_pairs<-rbind(remove_pairs,c("landuse","all"))
  remove_pairs<-rbind(remove_pairs,c("office","all"))
  remove_pairs<-rbind(remove_pairs,c("tourism","all"))
  remove_pairs<-rbind(remove_pairs,c("waterway","fuel"))
  imported_files[[i]]$coordinates<-filter_irreleavant_locations(coordinates = imported_files[[i]]$coordinates,
                                                                removal_pairs = remove_pairs)

  setTxtProgressBar(pb,i)
}



#########################################################################
# apply clustering algorithm over all documents to calculate confidence #
#########################################################################
# @pb progress bar for for loop
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0) 
for(i in 1:length(imported_files)){
  confidence<-apply_clustering_algorithm_on_locations_to_get_confidence_scores(names = imported_files[[i]]$coordinates$query,
                                                                               frequencies = imported_files[[i]]$coordinates$frequency,
                                                                               lat = imported_files[[i]]$coordinates$lat,
                                                                               lon = imported_files[[i]]$coordinates$lon)
  imported_files[[i]]$coordinates<-cbind(imported_files[[i]]$coordinates,confidence)
  setTxtProgressBar(pb,i)
}



#############################
# comparison with blacklist #
#############################
# @blacklist data.frame containing false positives and/or names of locations, that should be excluded from further analysis
# ensure that the column names of blacklsit object are c("location","lat","lon")
# load blacklist from @blacklist_path; RDS or csv can be imported
blacklist_path<-"external_information/blacklist.rds"
blacklist<-readRDS(file = blacklist_path)
# also CSV format possible, which can be read in with:
# blacklist_path<-"external_information/blacklist.csv"
# blacklist<-read.csv(file = "blacklist.csv")
#filter locations with blacklist
for(i in 1:length(imported_files)){
  to_remove<-which(imported_files[[i]]$coordinates$query%in%blacklist$location)
  if(length(to_remove)>0){
  print(paste0("Due to blacklsit match, in document ",i," these locations will be removed: ",
               paste0(imported_files[[i]]$coordinates$query[to_remove],cllapse=", ")))
  imported_files[[i]]$coordinates<-imported_files[[i]]$coordinates[-to_remove,]
  }
  else{
    print(paste0("In document ",i," no blacklist match was found."))
  }
}


##############################################################
# enrich found locations with Country and Continent metadata #
##############################################################

# @pb progress bar for for loop
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0,type=3) 
for(i in 1:length(imported_files)){
  
  country_and_continents<-coords2country_and_continent(points = data.frame(lon=imported_files[[i]]$coordinates$lon,
                                                                          lat=imported_files[[i]]$coordinates$lat))
  imported_files[[i]]$coordinates<-cbind(imported_files[[i]]$coordinates,country_and_continents)
  setTxtProgressBar(pb,i)
}




###################################
# create/append whitelist entries #
###################################
# if a whitelist is already present and should be combined with current whitelist from input you can read it in as rds or csv file
# whitelist_path<-"external_information/whitelist.rds"
# whitelist<-readRDS(file = whitelist_path)
# also CSV format possible, which can be read in with:
# whitelist_path<-"external_information/whitelist.csv"

# create whitelist from imported data with aggregated confidence scores
whitelist_from_current_import<-data.frame(location=NULL,frequency=NULL,lat=NULL,lon=NULL,class=NULL,type=NULL,importance=NULL,country=NULL,continent=NULL,confidence=NULL,stringsAsFactors = F)
for(i in 1:length(imported_files)){
  print(nrow(imported_files[[i]]$coordinates))
  whitelist_from_current_import<-rbind(whitelist_from_current_import,data.frame(location=imported_files[[i]]$coordinates$query,
                                                                                frequency=imported_files[[i]]$coordinates$frequency,
                                                                                lat=imported_files[[i]]$coordinates$lat,
                                                                                lon=imported_files[[i]]$coordinates$lon,
                                                                                class=imported_files[[i]]$coordinates$class,
                                                                                type=imported_files[[i]]$coordinates$type,
                                                                                importance=imported_files[[i]]$coordinates$importance,
                                                                                country=imported_files[[i]]$coordinates$country,
                                                                                continent=imported_files[[i]]$coordinates$continent,
                                                                                confidence=imported_files[[i]]$coordinates$confidence,
                                                                                stringsAsFactors = F))
  
}





######################
# condense whitelist #
######################
# condense by name or coords
# by name:
whitelist_from_current_import<-condense_whitelist(whitelist=whitelist_from_current_import,mode="name")
# by coordiantes:
# whitelist_from_current_import<-condense_whitelist(whitelist=whitelist_from_current_import,mode="coords")


##############################
# save and extract whitelist #
##############################

# as rds
saveRDS(whitelist_from_current_import,file="external_information/whitelist.rds")

# as csv
write.csv(x = whitelist_from_current_import,file="external_information/whitelist.csv",row.names = F)








########################################
# extraction of metadata using regexps #
########################################













