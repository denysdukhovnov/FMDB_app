# library(shiny)
# library(shinydashboard)
# library(leaflet)
# library(RColorBrewer)
# library(rgeos)
#library(sp)
library(sf)
#library(rgdal)
# library(ggplot2)
# library(plotly)
# library(htmltools)
# library(DT)


dept <- as.character(c(1:97))

dept.names <- list("Ain","Aisne","Allier","Alpes (Basses)","Alpes (Hautes)",
                   "Alpes Maritimes","Ard?che","Ardennes","Ari?ge","Aube",
                   "Aude","Aveyron","Bouches du Rhone","Calvados","Cantal",
                   "Charente","Charente Maritime","Cher","Corr?ze","Corse",
                   "C?te d'Or","C?tes du Nord","Creuse","Dordogne","Doubs",
                   "Dr?me","Eure","Eure et Loir","Finist?re","Gard","Garonne (Haute)",
                   "Gers","Gironde","H?rault","Ille et Vilaine","Indre","Indre et Loire",
                   "Is?re","Jura","Landes","Loir et Cher","Loire","Loire (Haute)",
                   "Loire Inf?rieure","Loiret","Lot","Lot et Garonne","Loz?re",
                   "Maine et Loire","Manche","Marne","Marne (Haute)","Mayenne",
                   "Meurthe et Moselle","Meuse","Morbihan","Moselle","Ni?vre",
                   "Nord","Oise","Orne","Pas de Calais","Puy de Dome","Pyr?n?es (Basses)",
                   "Pyr?n?es (Hautes)","Pyr?n?es Orientales","Rhin (Bas)","Rhin (Haut)",
                   "Rh?ne","Sa?ne (Haute)","Sa?ne et Loire","Sarthe","Savoie",
                   "Savoie (Haute)","Seine","Seine Inf?rieure","Seine et Marne",
                   "Seine et Oise","S?vres (Deux)","Somme","Tarn","Tarn et Garonne",
                   "Var","Vaucluse","Vend?e","Vienne","Vienne (Haute)","Vosges",
                   "Yonne","Territoire de Belfort","Essonne","Hauts-de-Seine",
                   "Seine-Saint-Denis","Val-de-Marne","Val-d'Oise","Paris","Yvelines")

dept.id <- as.character(sprintf("%02d", c(1:97)), length = 2)


# Read in the life tables for each sex in every state
# BLT <- do.call(rbind, lapply(states, function(x) {
#   read.csv(paste0("USHMD/", paste0(x, "_bltper_1x1.csv")), sep = ",", 
#            header = FALSE, stringsAsFactors = FALSE, skip = 1)
# }))

MLT <- do.call(rbind, lapply(dept, function(x) {
  read.csv(paste0("LT/", paste0(x, "H.csv")), sep = ";", 
           header = FALSE, stringsAsFactors = FALSE, skip = 1)
}))

FLT <- do.call(rbind, lapply(dept, function(x) {
  read.csv(paste0("LT/", paste0(x, "F.csv")), sep = ";", 
           header = FALSE, stringsAsFactors = FALSE, skip = 1)
}))
#colnames(BLT) <- 
colnames(MLT) <- colnames(FLT) <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
MLT <- cbind(rep(dept.id, each = 106*(max(MLT$Year) - min(MLT$Year) + 1)), MLT)
FLT <- cbind(rep(dept.id, each = 106*(max(FLT$Year) - min(FLT$Year) + 1)), FLT)
colnames(MLT) <- colnames(FLT) <- c("PopName","Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
MLT$PopName <- as.character(MLT$PopName)
FLT$PopName <- as.character(FLT$PopName)
# # Quick check 
# plot(seq(sort(unique(BLT$Year[!is.na(BLT$Year)]), decreasing = FALSE)[1],
#          max(sort(unique(BLT$Year[!is.na(BLT$Year)]), decreasing = FALSE)), 1),
#      BLT.REG$mx[BLT.REG$Age == "110+" & BLT.REG$PopName == "Reg1"], type = "l", col = "red",
#      xlab = "Year", ylab = "Mortality Rate", main = "Both sex mortaility rate in Northeast region, Age 110+")
# lines(seq(sort(unique(BLT$Year[!is.na(BLT$Year)]), decreasing = FALSE)[1],
#           max(sort(unique(BLT$Year[!is.na(BLT$Year)]), decreasing = FALSE)), 1),
#       BLT.REG$mx[BLT.REG$Age == "110+" & BLT.REG$PopName == "Reg2"], type = "l", col = "blue")


# dept.before.1968.shp <- readOGR("shapes/Dept_before_1968.shp", layer = "Dept_before_1968", stringsAsFactors = F)
# dept.before.1968.shp <- dept.before.1968.shp[dept.before.1968.shp$code_dept %in% dept.id, ]
# 
# dept.after.1968.shp <- readOGR("shapes/Dept_after_1968.shp", layer = "Dept_after_1968", stringsAsFactors = F)
# dept.after.1968.shp <- dept.after.1968.shp[dept.after.1968.shp$code_dept %in% dept.id, ]


## sf shape read
dept.before.1968.shp <- st_read("shapes/Dept_before_1968.shp", layer = "Dept_before_1968", stringsAsFactors = F)
dept.before.1968.shp <- dept.before.1968.shp[dept.before.1968.shp$code_dept %in% dept.id, ]

dept.after.1968.shp <- st_read("shapes/Dept_after_1968.shp", layer = "Dept_after_1968", stringsAsFactors = F)
dept.after.1968.shp <- dept.after.1968.shp[dept.after.1968.shp$code_dept %in% dept.id, ]

# dept.before.1968.shp$periode <- 1
# dept.after.1968.shp$periode <- 2
# 
# combined.shp <- rbind(dept.before.1968.shp, dept.after.1968.shp)
# colnames(combined.shp)[2] <- "PopName"

## Only for decreasing geometric detail in sp objects
# old.polys <- SpatialPolygons(state.shp@polygons)
# new.polys <- gSimplify(old.polys, tol = 0.05, topologyPreserve = TRUE)
# state.shp <- SpatialPolygonsDataFrame(new.polys, data = state.shp@data, match.ID = FALSE)


#BLT$mx <- BLT$mx * 1000
MLT$mx <- MLT$mx * 1000
FLT$mx <- FLT$mx * 1000


## Combined LT if necessary
# LT <- rbind(MLT, FLT)
# LT$Sex <- rep(c("m","f"), each = nrow(MLT))
# 
# LT.lite <- LT[, c("PopName","Year","Age","Sex","mx","ex")]
#object.size(LT.lite)


#######
years <- sort(unique(MLT[, "Year"]))
# Create a set of life tables by year for all geographies
for (i in 1:length(years)) {
  #assign(paste0("BLT.", years[i]), BLT[BLT$Year == years[i], c("PopName", "Age", "mx", "ex")])
  assign(paste0("MLT.", years[i]), MLT[MLT$Year == years[i], c("PopName", "Age", "mx", "ex")])
  assign(paste0("FLT.", years[i]), FLT[FLT$Year == years[i], c("PopName", "Age", "mx", "ex")])
}

# tmp.lst[i] <- merge(state.shp, year.data, by.x = "STUSPS", by.y = "PopName")
# 
# sex.lst <- list("BLT.", "MLT.", "FLT.")
# spdf.lst <- lapply(sex.lst, function(x) {
# 
#     tmp.lst <- list()
#     for (i in 1:(max(years)-years[1])) {
# 
#         year.data <- get(paste0(x, years[i]))
#         year.data.age <- df[df$Age == isolate(as.character(input$age)), ]
# 
#     }
#     return(tmp.lst)
# })

library(dplyr)


saveRDS(map.dept, "./map_dept.rds")
