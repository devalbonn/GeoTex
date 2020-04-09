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
# function to extract metadata from texts using a defined vector of regular expressions
source("functions/extract_metadata_from_text_using_regexps.R")
# check if location cache exists; if not cerate an empty location cache file
if(!file.exists("external_information/location_cache.RData")){
  location_cache<-data.frame(query=NULL, lat=NULL, lon=NULL, lat_min=NULL, lat_max=NULL, lon_min=NULL, lon_max=NULL, place_id=NULL, osm_type=NULL,
                             osm_id=NULL, place_rank=NULL, display_name=NULL, class=NULL, type=NULL, importance=NULL, icon=NULL)
  save(location_cache,file = "external_information/location_cache.RData")
}


#####
# 1 #
#####
##################################################################################
# import the text files that are located in the directory @import_file_directory #
##################################################################################

print("importing files to R")
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


#####
# 2 #
#####
#############################################################################################
# language detection of texts in order to choose the correct spaCy model for pre-processing #
#############################################################################################

# get languages for every imported text and add  it to each list with element name language
print("detection of languages used in texts")
for(i in 1:length(imported_files)){
  imported_files[[i]]$language<-cld2::detect_language_mixed(imported_files[[i]]$text)$classificaton
}
print("finished language detection for all files")


#####
# 3 #
#####
#########################################################
# extract locations from texts; use spacy NER locations #
#########################################################
#@ imported_files[[index]]locations: a dataframe of found locations for corresponding import file
print("extraction of locations using spaCy NER-Tags")
for(i in 1:length(imported_files)){
  imported_files[[i]]$locations<-extract_locations_from_text(text = imported_files[[i]]$text,language = imported_files[[i]]$language,threshold_for_multiple_models = 0.3)
}
print("finished extraction all location names using spaCy")


#####
# 4 #
#####
############################################################
# get coordinates for found locations from open street map #
############################################################
# for geocoding OpenSteetMap is used
# Google Geonames can no longer be used free of charge
print("Geocoding using OpenStreetMap")
# @pb progress bar for for loop
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0,style=3) 
for(i in 1:length(imported_files)){
  # import location cache; cache is needed to minimize the number of requests to OSM --> lower risk of bandwidth issues
  load("external_information/location_cache.RData")
  # remove brackets for geocode_OSM() to work properly
  imported_files[[i]]$locations$location <- stringr::str_remove_all(string = imported_files[[i]]$locations$location, pattern = "[{}]+")
  locations_for_geocoding <- as.character(imported_files[[i]]$locations$location)
  
  # comparison with cache
  imported_files[[i]]$coordinates<-location_cache[which(location_cache$query%in%locations_for_geocoding),]
  # send unknown locations to OSM
  known_location_ids<-which(locations_for_geocoding%in%location_cache$query)
  if(length(known_location_ids)>0){
    unknown_locations<-locations_for_geocoding[-known_location_ids]
  }
  else{
    unknown_locations<-locations_for_geocoding
  }
  if(length(unknown_locations)>0){
    new_coordinates<-NULL
    try({
      new_coordinates<-tmaptools::geocode_OSM(q = c(unknown_locations,"Berlin"),
                                              server ="https://nominatim.openstreetmap.org",details = T,as.data.frame = T)
    })
    # if new coordinated could have been extracted, bind them to the known coordinates 
    if(length(new_coordinates)>0){
      imported_files[[i]]$coordinates<-rbind(imported_files[[i]]$coordinates,new_coordinates)
    }                                  
  }
  # update location cache
  location_cache<-unique(rbind(location_cache,imported_files[[i]]$coordinates))
  save(location_cache,file = "external_information/location_cache.RData")
  # merge frequency information to coordiantes dataframe
  imported_files[[i]]$coordinates<-merge(x = imported_files[[i]]$coordinates,y = imported_files[[i]]$locations,by.x = "query", by.y =  "location")
  # narrow down on necessary columns
  imported_files[[i]]$coordinates<-imported_files[[i]]$coordinates[,c("query","frequency","lat","lon","class","type","importance")]
  # update Progress bar
  setTxtProgressBar(pb,i)
}
print("finished geocoding")

#####
# 5 #
#####
##################################################################################################
# filter certain location with specific values in metadata field type and class; E.g. shopping.. #
##################################################################################################
print("Filtering for certain locations classes/types")
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
print("finished filtering locations using classes/types")


#####
# 6 #
#####
#########################################################################
# apply clustering algorithm over all documents to calculate confidence #
#########################################################################
# @pb progress bar for for loop
print("Clustering of locations to calculate confidence scores")
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0) 
for(i in 1:length(imported_files)){
  confidence<-location_clustering_for_confidence(names = imported_files[[i]]$coordinates$query,
                                                                               frequencies = imported_files[[i]]$coordinates$frequency,
                                                                               lat = imported_files[[i]]$coordinates$lat,
                                                                               lon = imported_files[[i]]$coordinates$lon)
  imported_files[[i]]$coordinates<-cbind(imported_files[[i]]$coordinates,confidence)
  setTxtProgressBar(pb,i)
}
print("finished clustering")


#####
# 7 #
#####
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
# filter locations with blacklist
print("filtering locations using a blacklist")
for(i in 1:length(imported_files)){
  to_remove<-which(imported_files[[i]]$coordinates$query%in%blacklist[,"location"])
  if(length(to_remove)>0){
    print(paste0("Due to blacklsit match, in document ",i," these locations will be removed: ",
                 paste0(imported_files[[i]]$coordinates$query[to_remove],collapse=", ")))
    imported_files[[i]]$coordinates<-imported_files[[i]]$coordinates[-to_remove,]
  }
  else{
    print(paste0("In document ",i," no blacklist match was found."))
  }
}
print("finished filtering locations using blacklist")


#####
# 8 #
#####
##############################################################
# enrich found locations with Country and Continent metadata #
##############################################################
print("enriching locations with metadata country and continent")
# @pb progress bar for for loop
pb = txtProgressBar(min = 0, max = length(imported_files), initial = 0,style=3) 
for(i in 1:length(imported_files)){
  
  country_and_continents<-coords2country_and_continent(points = data.frame(lon=imported_files[[i]]$coordinates$lon,
                                                                           lat=imported_files[[i]]$coordinates$lat))
  imported_files[[i]]$coordinates<-cbind(imported_files[[i]]$coordinates,country_and_continents)
  setTxtProgressBar(pb,i)
}
print("finished enriching locations")
  

#######
# 9.1 #
#######
###################################
# create/append whitelist entries #
###################################
# if a whitelist is already present and should be combined with current whitelist from input you can read it in as rds or csv file
# whitelist_path<-"external_information/whitelist.rds"
# whitelist<-readRDS(file = whitelist_path)
# also CSV format possible, which can be read in with:
# whitelist_path<-"external_information/whitelist.csv"
print("creating whitelist for imported data")
# create whitelist from imported data with aggregated confidence scores
whitelist_from_current_import<-data.frame(location=NULL,frequency=NULL,lat=NULL,lon=NULL,class=NULL,type=NULL,importance=NULL,country=NULL,continent=NULL,confidence=NULL,stringsAsFactors = F)
for(i in 1:length(imported_files)){
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
print("finished creation of whitelist")


#######
# 9.2 #
#######
######################
# condense whitelist #
######################
# condense by name or coords
# by name:
whitelist_from_current_import<-condense_whitelist(whitelist=whitelist_from_current_import,mode="name")
# by coordiantes:
# whitelist_from_current_import<-condense_whitelist(whitelist=whitelist_from_current_import,mode="coords")


#######
# 9.3 #
#######
##############################
# save and extract whitelist #
##############################
# as rds
saveRDS(whitelist_from_current_import,file="external_information/whitelist.rds")
# as csv
write.csv(x = whitelist_from_current_import,file="external_information/whitelist.csv",row.names = F)



######
# 10 #
######
########################################
# extraction of metadata using regexps #
########################################
# define a set of regular expressions for each metadata to be extracted; first matching reg exp will be used --> specify order of regexps with caution
# feel free to add or modify theese expressions
regexps_grade <- c("Note[\\:]?[ ]?\\d", "rating[\\:]?[ ]?\\d", "Grade[\\:]?[ ]?\\d", "note[\\:]?[ ]?\\d", "Stufe[\\:]?[ ]?\\d")
regexps_funding_volume <- c("[0-9,\\.]{2,15}[ ]?Mio. E[ ]?U[ ]?R",  "E[ ]?U[ ]?R [0-9,\\.]{2,4}[ ]?Mio.", " D[ ]?M[ ]? [0-9,\\.]{2,4}[ ]?Mio.")
regexps_date <- c("[\\d]{1,2}\\.[\\d]{1,2}\\.[\\d]{4}")
print("extraction of metadata using regular expresions")
for(i in 1:length(imported_files)){
  imported_files[[i]]$meta$grade <- extract_metadata_from_text_using_regexps(text = imported_files[[i]]$text,regexps = regexps_grade)
  imported_files[[i]]$meta$funding_volume <- extract_metadata_from_text_using_regexps(text = imported_files[[i]]$text,regexps = regexps_funding_volume)
  imported_files[[i]]$meta$date <- extract_metadata_from_text_using_regexps(text = imported_files[[i]]$text,regexps = regexps_date)
}
print("finished extraction of metadata")




#########################
# Sample access to data #
#########################

i=2
# get coordinates for document i
imported_files[[i]]$coordinates

# get original text of document i
substr(imported_files[[i]]$text,1,1000)

# get language distribution for document i
imported_files[[i]]$language

# check metadata for document i
imported_files[[i]]$meta



# get all locations
head(whitelist_from_current_import)


# get all metadata
meta<- lapply(X = imported_files,FUN = `[[`,"meta")
#  all grades:
lapply(X = meta,FUN = `[[`,"grade")



