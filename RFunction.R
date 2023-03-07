library(move)
library(sp)
library(adehabitatHR)
library(sf)
library(ggplot2)

##data is the movestack input
##type should be one between individual and population
##points should be number of random points you want to generate

rFunction <-function(data, type, points =10, background_area="mcp")
{
  #data <- data[!is.na(data$location_lat) & !is.na(data$location_long),]
  data_df <-as.data.frame(data)
  
  #functions to calculate the mcp
  mcpsf <- function(x, percent=95){
    centroid <- sf::st_centroid(sf::st_union(x))
    dist <- as.numeric(sf::st_distance(x, centroid))
    within_percentile_range <- dist <= quantile(dist, percent/100)
    x_filter <- st_union(x[within_percentile_range,])
    st_convex_hull(x_filter)
  }
  
  if (type == "population"){
    dat_sf <-st_as_sf(data_df, coords=c("location.long", "location.lat"), crs=4326)
    pop_hr_mcp<- mcpsf(dat_sf, percent=100)
    #pop_rand_pnt <- spsample(pop_hr_mcp, n= nrow(data)*points, "random")
    pop_rand_pnt <- st_sample(pop_hr_mcp, size = nrow(data)*points, type = "random")
    final_dat <-as.data.frame(rbind(coordinates(data), st_coordinates(pop_rand_pnt)))
    final_dat$case <-rep(c(1,0), c(nrow(data),length(pop_rand_pnt)))
    #final_dat$timestamp <-rep(data$timestamp, (points+1))
    final_dat$timestamp <-seq(from= min(data$timestamp, na.rm = T),to=  max(data$timestamp, na.rm=T), 
                              length.out= nrow(final_dat))
    final_dat$trackId <- rep(data_df$trackId, (points+1))
    }
  else 
  {
    data1 <- as.data.frame(moveStack(data[[which(n.locs(data)>=5)]]))
    data_fltr <- data1[, c("trackId", "location.lat", "location.long", "timestamp")]
    #data_fltr$id <-data_fltr$trackId
    
    indv_pnt_cmbnd <-list()
    uid <-unique(data_fltr$trackId)
    for(i in 1:length(uid))
    {
            indv_data_fltr <- data_fltr[data_fltr$trackId==uid[i],]
            coordinates(indv_data_fltr) <-c("location.long", "location.lat")
            indv_dat_sf <-st_as_sf(indv_data_fltr, coords=c("location.long", "location.lat"), crs=4326)
            indv_hr_mcp <- mcpsf(indv_dat_sf, percent = 100)
            #indv_rand_pnt <- spsample(indv_hr_mcp, n= nrow(indv_data_fltr)*points, "random")
            indv_rand_pnt <- st_sample(indv_hr_mcp, size= nrow(indv_data_fltr)*points, type = "random")
            indv_pnt_sep <-as.data.frame(rbind(coordinates(indv_data_fltr), st_coordinates(indv_rand_pnt)))
            indv_pnt_sep$case <-rep(c(1,0), c(length(indv_data_fltr),length(indv_rand_pnt)))
            #indv_pnt_sep$timestamp <-rep(indv_data_fltr$timestamp, (points+1))
            indv_pnt_sep$timestamp <- seq(from= min(indv_data_fltr$timestamp, na.rm = T),to=  max(indv_data_fltr$timestamp, na.rm=T), 
                                          length.out= nrow(indv_pnt_sep))
            indv_pnt_sep$trackId <-rep(indv_data_fltr$trackId, (points+1))
            indv_pnt_cmbnd[[i]]<-indv_pnt_sep
    }
    
    final_dat <- do.call(rbind, indv_pnt_cmbnd)
  }
  
  final_dat <- final_dat[order(final_dat$trackId, final_dat$timestamp), ]
  colnames(final_dat)[1:2] <-c("location.long", "location.lat")
  
  ### plot the presence and random points to check
  plot <-ggplot(final_dat, aes(x= location.long, y= location.lat))+
   geom_point(aes(col= as.factor(case)), alpha=0.5)+
    labs(x= "Longitude", y= "Latitude", col ="Data type")+
    theme_bw()
  
  ggsave(plot, #file= "Presence_random_locations.jpeg",
         file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Presence_random_locations.jpeg"),
         width=10, height=8, dpi=200, units= "in")
  
  
  ### Convert the data.frame to movestack
  final_dat_move <-move(x= as.numeric(final_dat$location.long), y= as.numeric(final_dat$location.lat),
                        time=as.POSIXct(final_dat$timestamp,format="%Y-%m-%d %H:%M:%S"),
                        data=final_dat, proj=CRS("+proj=longlat +ellps=WGS84"),
                        animal= final_dat$trackId)
  return(final_dat_move)
}