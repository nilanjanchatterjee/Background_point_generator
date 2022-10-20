library(move)
library(sp)
library(adehabitatHR)
library(sf)

##data is the movestack input
##type should be one between individual and population
##points should be number of random points you want to generate

RFunction <-function(data, type, points)
{
  data <- data[!is.na(data$location_lat) & !is.na(data$location_long),]
  
  if (type == "population"){
    pop_hr_mcp<- mcp(data, percent=100)
    pop_rand_pnt <- spsample(pop_hr_mcp, n= nrow(data)*10, "random")
    pop_pnt_cmbnd <-as.data.frame(rbind(coordinates(data), coordinates(pop_rand_pnt)))
    pop_pnt_cmbnd$case <-rep(c(1,0), c(nrow(data),length(pop_rand_pnt)))
    }
  else 
  {
    data1 <- as.data.frame(moveStack(data[[which(n.locs(data)>=5)]]))
    data_fltr <- data1[, c("tag_local_identifier", "location_lat", "location_long")]
    #data_fltr$id <-data_fltr$trackId
    
    indv_pnt_cmbnd <-list()
    uid <-unique(data_fltr$tag_local_identifier)
    for(i in 1:n.indiv(data))
    {
            indv_data_fltr <- data_fltr[data_fltr$tag_local_identifier==uid[i],]
            coordinates(indv_data_fltr) <-c("location_long", "location_lat")
            indv_hr_mcp <- mcp(indv_data_fltr, percent = 100)
            indv_rand_pnt <- spsample(indv_hr_mcp, n= nrow(indv_data_fltr)*10, "random")
            indv_pnt_sep <-as.data.frame(rbind(coordinates(indv_data_fltr), coordinates(indv_rand_pnt)))
            indv_pnt_sep$case <-rep(c(1,0), c(length(indv_data_fltr),length(indv_rand_pnt)))
            indv_pnt_cmbnd[[i]]<-indv_pnt_sep
    }
    
    indv_pnt_all <- do.call(rbind, indv_pnt_cmbnd)
  }
}