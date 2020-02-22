#' create a data.frame containing the found locations from a text based on whitelist comparison and spacy Named Entity Recognition
#' 
#' @param text text containing locations
#' @param language either a string specifying the spacy model to use or a result from cld2::detect_language_mixed()$classificaton if multiple languages are present in texts
#' @return dataframe containing found locations in \code{text}
#' @examples
#' extract_locations_from_text(text="In Berlin ist es schöner als in Hamburg",language="de")
extract_locations_from_text <- function(text,language,threshold_for_multiple_models=0.4){
  # if only a single language is used
  if(class(language)=="character"){
    #initialize spacy with model corresponding to chosen language
    spacyr::spacy_initialize(model = language)
    # parse texts
    parsed_text<-spacyr::spacy_parse(x = text, lemma = TRUE, entity = TRUE)
    # extract location tags from parsed text
    locations<-extract_tags(parsed_text=parsed_text,tags=c("LOC","GPE"))
    spacyr::spacy_finalize()
  }
  # if several languages are available
  else{
    # get relevant languages which have a proportion >= @threshold_for_multiple_models
   language<-language[which(language$proportion>=threshold_for_multiple_models),"code"]
   #initialize spacy with model corresponding to chosen language
   spacyr::spacy_initialize(model = language[1])
   # parse texts
   parsed_text<-spacyr::spacy_parse(x = text, lemma = TRUE, entity = TRUE)
   # extract location tags from parsed text
   locations<-extract_tags(parsed_text=parsed_text,tags=c("LOC","GPE"))
   spacyr::spacy_finalize()
   # if more than one language had a higher proportion than @threshold_for_multiple_models
   if(length(language)>1){
     for( i in 2:length(language)){
       #initialize spacy with model corresponding to chosen language
       spacyr::spacy_initialize(model = language[i])
       # parse texts
       parsed_text_additional<-spacyr::spacy_parse(x = text, lemma = TRUE, entity = TRUE)
       # extract location tags from parsed text
       locations_additional<-extract_tags(parsed_text=parsed_text_additional,tags=c("LOC","GPE"))
       spacyr::spacy_finalize()
       # add "new" locations from locations_additional to locations
       locations<-rbind(locations,locations_additional[-which(locations_additional[,"token"]%in%unique(locations$token)),])
     }
   }
  }
 # replace newlines inside of words
  locations$token<-stringr::str_replace_all(string = locations$token,pattern = "\n",replacement = " ")
  locations$lemma<-stringr::str_replace_all(string = locations$lemma,pattern = "\n",replacement = " ")
  locations$lemma<-stringr::str_replace_all(string= locations$lemma, pattern = "_", replacement = " ")
  # trim whitespaces
  locations$lemma <- trimws(locations$lemma)
  locations$lemma <- stringr::str_replace_all(string = locations$lemma, pattern = "[ ]+",replacement = " ")
  # replace locations with only 1 character (most likely false positive)
  locations<-locations[which(nchar(locations$lemma)>1),]
  # use of lemma 
  locations<-data.frame(table(locations$lemma),stringsAsFactors = F)
  locations<-locations[order(locations[,2],decreasing=T),]
  colnames(locations)<-c("location","frequency")
  return(locations)
}

