# Location-Extraction
script-based extraction of place names including geocoding


## Preconditions
* required R libraries:
  + readtext, spacyr, cld2, tmaptool, geosphere, worldmapm, sp
* spaCy with models "de" and "en" ([install spaCy](https://spacy.io/usage/))

## Folder structure
* `complete_location_extraction_script.R`: contains all necessary steps to extract locations from given texts in directory `files_to_be_imported/` 
* directory `files_to_be_imported/`: place the pdf files to use in this directory
* directory `functions/` contains all necessary functions for the applied substeps
* directory `external_information/`: blacklist, whitelist and location-cache files are located in this directory

## Substeps
In order to complete the whole task, 10 substeps need to be carried out.
### 1. Import texts
Import all text files located in **files_to_be_imported** directory. The import utilizes the package readtext. The warning *PDF error: Invalid Font Weight* can be unattended.
### 2. Language detection
In the documents sometimes multiple languages are present. In order to achieve a high recall in the location extraction, we apply multiple spaCy models, if the corresponding langauge has a higher probabilty than @threshold_for_multiple_models (default 0.3). 

### 3. spaCy Parsing and location extraction
Locations are extracted based on the extracted Named Entities with the Tags LOC (Location) and GPE (Geo-Poloitcal-Entity).
If more than one language has a probability > @threshold_for_multiple_models, the location extraction is based on multiple parsed texts.

### 4. GeoCoding
In this substep, we assign coordiantes and certain metadata to the found locations. For this purpose, we initially check if the locations are already present in the location cache. We use a cache in order to minimize the queries to OpenStreepMap(OSM) to minimize the risk of getting bandwith issues. Google Maps geonames API is not free of charge anymore. Hence we switched to OSM.

### 5. Filtering by Type/Class
OSM returns the metadata "Type" and "Class" for each retrieved location. We can use these tags to filter for likely false positive entries. Certain combinations of Type and Class can be used for filtering. If the corresonding "Type" for a certain "Class" doesn't matter use the setting: "all".

### 6. Clustering fpr confidence
To calculate a confidence for the locations we apply a clustering algorithm. For this purpose we initailly calculate a centroid for each document based on the given locations (location frequency is considered). Next, we calculate the distances from all locations to the centroid. Based on theese distances we apply k means clustering. The best k is determined iterative. The cluster whose, elements (locations) have the shortest distance to the centroid in average, is assumed to be the "correct" cluster. All locations belongig to this cluster get an confidence score of 1. All other locations get a score of 0.
The last step then is to, average these confidence scores for all found locations over all documents. The higher the confidence is, the more likely the locations is correct.

### 7. Filtering against blacklist
A blacklist can be used. It needs to be placed inside **external_information**. Then the found locations will be compared with the entries in the blacklist. If a location matches, it will be removed and not used anymore in further steps.

### 8. Enrich locations with external meta information
Based on the coordiantes we enrich the locations with the information Country and Continent.

### 9. Create, condense and save whitelist
Here we create a dataframe containing the whitelist based on the currently imported files. In the step of condensing the whitelist, the confidence scores are averaged.

### 10. Extraction of Metadata from texts
In the last step, we try to extract metadata from inside the texts. For this purpose we defince a vector of regular expressions for each metadata type. So far the meta data types: date, funding_volume and grade are integrated. More meta data types can be added accordingly. 

