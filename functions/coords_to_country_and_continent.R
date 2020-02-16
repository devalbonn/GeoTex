#' get country and continent for coordinates
#' @param coordiantes data.frame in which: 
#'  - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
#' @return data.frame containing the countires and continents for the given points
#' @examples
#' coords2country_and_continent(points=data.frame(lon=c(9.993682,8.806422),lat=c(53.551086,53.073635)))
coords2country_and_continent = function(points)
{  
  countriesSP <- rworldmap::getMap(resolution='low')
  #setting CRS directly to that from rworldmap
  pointsSP = sp::SpatialPoints(points, proj4string=CRS(sp::proj4string(countriesSP)))  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  return(data.frame(country=indices$ADMIN,continent=indices$REGION))
}