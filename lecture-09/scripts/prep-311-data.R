##########################
## PREP 311 DATA FOR HCA##
##########################
  
#Set working directory
  setwd("/Users/jeff/Documents/Github/textbook/chapter10/data")

#Read in data 
  df <- read.csv("311_Service_Requests_from_2016.csv")
  
#Round Lat/Lon to 2-digits
  df$lat <- round(df$Latitude/0.005)*0.005
  df$lon <- round(df$Longitude/0.005)*0.005

#Clean the Complaint Types
  df$type <- gsub(" ", ".",tolower(df$Complaint.Type))
  df$type <- gsub("/", ".",tolower(df$type))
  df$type <- gsub("-", ".",tolower(df$type))
  df$type[grep("tree", df$type)] <- "tree"
  df$type[grep("noise", df$type)] <- "noise"
  df$type[grep("collection", df$type)] <- "missed.collection"
  df$type[grep("unsanitary", df$type)] <- "unsanitary.condition"
  df$type[grep("sweeping", df$type)] <- "sweeping.missed"
  df$type[grep("overflowing.litter.baskets", df$type)] <- "overflowing.basket"
  df$type[grep("overflowing.recycling.baskets", df$type)] <- "overflowing.basket"
  df$type[grep("highway.sign", df$type)] <- "highway.sign.condition"
  df$type[grep("street.sign", df$type)] <- "street.sign.condition"
  df$type[grep("public.assembly...temporary", df$type)] <- "public.assembly"
  df$type[grep("homeless.person.assistance", df$type)] <- "homeless.person"
  df$type[grep("homeless.encampment", df$type)] <- "homeless.person"
  df$type[grep("fire.alarm", df$type)] <- "fire.alarm.issue"
  df$type[grep("electrical", df$type)] <- "electric"
  df$type[grep("dof.property", df$type)] <- "dof.property.issue"
  df$type[grep("dof.parking", df$type)] <- "dof.parking.issue"
  df$type[grep("derelict.vehicle", df$type)] <- "derelict.vehicle"
  df$type[grep("broken.parking.meter", df$type)] <- "broken.muni.meter"
  df$type[grep("illegal.animal.sold", df$type)] <- "illegal.animal.issue"
  df$type[grep("illegal.animal.kept.as.pet", df$type)] <- "illegal.animal.issue"
  
  df$type[grep("home.delivered.meal...missed.delivery", df$type)] <- "home.delivered.meal.issue"
  df$type[grep("home.delivered.meal.complaint", df$type)] <- "home.delivered.meal.issue"
  df$type[grep("literature.request", df$type)] <- "literature.request"
  
  
#Drop any missing coordinates
  df <- df[!is.na(df$lat), ]
  
#Aggregate up
  df2 <- aggregate(df[,"lat"], 
                   by = list(lat = df$lat, lon = df$lon, type = df$type), 
                   FUN = length)
  
#Reshape wide
  df3 <- reshape(df2, 
                 timevar = "type",
                 idvar = c("lat","lon"),
                 direction = "wide")

#Keep complaint types with more than n = 100
  for(k in ncol(df3):3 ){
    if(sum(df3[,k], na.rm=T) < 1000){
      df3[,k] <- NULL
    } else {
      df3[,k][is.na(df3[,k])] <- 0
    }
    
  }
  dim(df3)


#Drop missing records
  nyc311 <- df3[!is.na(df3[,3]) & !is.nan(df3[,3]),]
  colnames(nyc311) <- gsub("x.","", colnames(nyc311))
  rm(df, df2, df3)

#Save
  save(nyc311, file = "nyc311.Rda")
