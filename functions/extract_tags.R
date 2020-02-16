#' extract Tags from a spacy parsed text object
#' @param parsed_text spacyr_parsed data.frame
#' @param tags character vector of Tags to extract from \code{parsed_text}
#' @return dataframe containing matches for specified \code{tags} in \code{parsed_text}
extract_tags <- function(parsed_text, tags){
  #consolidate entites
  parsed_text<-spacyr::entity_consolidate(parsed_text)
  locations<-parsed_text[which(parsed_text[,"entity_type"]%in%tags),]
  return(locations)
}