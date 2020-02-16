#' filter output from OSM using metadata class and type
#' 
#' @param coordiantes data.frame output from tmaptools::geocode_OSM()
#' @param remove_pairs data.frame with n times 2 dimensions; every row represents a pair of (class, type); if class or type value="all", all rows matching the other value will be removed
#' @return filtered data.frame where the rows matching the given @remove_pairs
#' @examples
#' filter_irreleavant_locations(tmaptools::geocode_OSM("berlin","Auswertiges Amt"),data.frame(class="office",type="all"))
filter_irreleavant_locations <- function(coordinates,removal_pairs){
  # @remove_index store the rows that need to be removed
  remove_index<-NULL
  for(i in 1:nrow(removal_pairs)){
    # if class=="all
    if(removal_pairs[i,1]=="all"){
      remove_index<-c(remove_index,which(coordinates$type==removal_pairs[i,2]))
      next
    }
    # if type== "all"
    if(removal_pairs[i,2]=="all"){
      remove_index<-c(remove_index,which(coordinates$class==removal_pairs[i,1]))
      next
    } 
    # if neither of both is "all"
    if(removal_pairs[i,1]!="all" && removal_pairs[i,2]!="all"){
      remove_index<-c(remove_index,intersect(which(coordinates$class==removal_pairs[i,1]),which(coordinates$type==removal_pairs[i,2])))
      next
    }
  }
  if(length(remove_index)>0){
    coordinates_reduced<-coordinates[-remove_index,]
  }
  else{
    coordinates_reduced<-coordinates
  }
  return(coordinates_reduced)
}