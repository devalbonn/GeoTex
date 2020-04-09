#' condense whitelist either by location name or coordiantes
#' @param whitelist whitelist data.frame with columns:location, frequency, lat, lon, class, type, importance, country, continent, confidence
#' @param mode ccharacter either "name" or "coords"
#' @return condensed whitelist
condense_whitelist <- function(whitelist,mode){
  if(mode=="name"){
    # aggregate confidence score over all documents
    confidence_table<-aggregate(whitelist[,"confidence"],by=list(whitelist[,"location"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})
    colnames(confidence_table)<-c("location","confidence")
    condensed_whitelist<-aggregate(as.numeric(whitelist[,"frequency"]),by=list(whitelist[,"id"],whitelist[,"location"]),FUN=sum)
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"lat"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"lon"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"class"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"type"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"importance"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"country"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"continent"],by=list(whitelist[,"id"],whitelist[,"location"]),FUN=function(x){return(x[1])})[,3])
    colnames(condensed_whitelist)<-c("id","location","frequency","lat","lon","class","type","importance","country","continent")
    # merge confidence scores to locations by location name
    condensed_whitelist<-merge(x=condensed_whitelist,y=confidence_table,by.x="location",by.y="location")
  }
  if(mode=="coords"){
    confidence_table<-aggregate(whitelist[,"confidence"],by=list(whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(mean(as.numeric(as.character(x))))})
    colnames(confidence_table)<-c("lat","lon","confidence")
    # use most frequent location name with identical coordiantes
    condensed_whitelist<-aggregate((whitelist[,"location"]),by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(names(sort(table(as.character(x)),decreasing=T))[1])})
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(as.numeric(whitelist[,"frequency"]),by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=sum)[,4])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"class"],by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,4])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"type"],by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,4])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"importance"],by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,4])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"country"],by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,4])
    condensed_whitelist<-cbind(condensed_whitelist,aggregate(whitelist[,"continent"],by=list(whitelist[,"id"],whitelist[,"lat"],whitelist[,"lon"]),FUN=function(x){return(x[1])})[,4])
    colnames(condensed_whitelist)<-c("id","lat","lon","location","frequency","class","type","importance","country","continent")
    # merge confidence scores to locations by location name
    condensed_whitelist<-merge(x=condensed_whitelist,y=confidence_table,by.x=c("lat","lon"),by.y=c("lat","lon"))
  }
  #order columns
  condensed_whitelist<-condensed_whitelist[,colnames(whitelist)]
  return(condensed_whitelist)
}