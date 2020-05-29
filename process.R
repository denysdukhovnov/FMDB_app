# May 28, 2020
# Author: Denys Dukhovnov

#----------------------------------------
# This file processes all the information into a single input required for the FMDB Shiny app to work.
# This file should be modified and/or run ONLY IF there are substantive changes to the data (including labels) (e.g. new annual update)
#----------------------------------------

library(sf)

dept <- as.character(c(1:98))

dept.names <-    c("Ain","Aisne","Allier","Alpes (Basses)","Alpes (Hautes)",
                   "Alpes Maritimes","Ardèche","Ardennes","Ariège","Aube",
                   "Aude","Aveyron","Bouches du Rhone","Calvados","Cantal",
                   "Charente","Charente Maritime","Cher","Corrèze","Corse",
                   "Côte d'Or","Côtes du Nord","Creuse","Dordogne","Doubs",
                   "Drôme","Eure","Eure et Loir","Finistère","Gard","Garonne (Haute)",
                   "Gers","Gironde","Hérault","Ille et Vilaine","Indre","Indre et Loire",
                   "Isère","Jura","Landes","Loir et Cher","Loire","Loire (Haute)",
                   "Loire Inférieure","Loiret","Lot","Lot et Garonne","Lozère",
                   "Maine et Loire","Manche","Marne","Marne (Haute)","Mayenne",
                   "Meurthe et Moselle","Meuse","Morbihan","Moselle","Nièvre",
                   "Nord","Oise","Orne","Pas de Calais","Puy de Dome","Pyrénées (Basses)",
                   "Pyrénées (Hautes)","Pyrénées Orientales","Rhin (Bas)","Rhin (Haut)",
                   "Rhône","Saône (Haute)","Saône et Loire","Sarthe","Savoie",
                   "Savoie (Haute)","Seine","Seine Inférieure","Seine et Marne",
                   "Seine et Oise","Sèvres (Deux)","Somme","Tarn","Tarn et Garonne",
                   "Var","Vaucluse","Vendée","Vienne","Vienne (Haute)","Vosges",
                   "Yonne","Territoire de Belfort","Essonne","Hauts-de-Seine",
                   "Seine-Saint-Denis","Val-de-Marne","Val-d'Oise","Paris","Yvelines","France")

dept.id <- as.character(sprintf("%02d", c(1:98)), length = 2)


# Read in the life tables for each sex in every department
MLT <- do.call(rbind, lapply(dept, function(x) {
  read.csv(paste0("LT/", paste0(x, "H.csv")), sep = ";", 
           header = FALSE, stringsAsFactors = FALSE, skip = 1)
}))

FLT <- do.call(rbind, lapply(dept, function(x) {
  read.csv(paste0("LT/", paste0(x, "F.csv")), sep = ";", 
           header = FALSE, stringsAsFactors = FALSE, skip = 1)
}))

colnames(MLT) <- colnames(FLT) <- c("Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
MLT <- cbind(rep(dept.id, each = 106*(max(MLT$Year) - min(MLT$Year) + 1)), MLT)
FLT <- cbind(rep(dept.id, each = 106*(max(FLT$Year) - min(FLT$Year) + 1)), FLT)
colnames(MLT) <- colnames(FLT) <- c("PopName","Year","Age","mx","qx","ax","lx","dx","Lx","Tx","ex")
MLT$PopName <- as.character(MLT$PopName)
FLT$PopName <- as.character(FLT$PopName)

## sf shape read, sort in alphabetical order of the department codes, and rearrange the rownames to match the alphabetical order
dept.before.1968.shp <- st_read("shapes/Dept_before_1968.shp", layer = "Dept_before_1968", stringsAsFactors = F)
dept.before.1968.shp <- dept.before.1968.shp[dept.before.1968.shp$code_dept %in% dept.id, ]
dept.before.1968.shp <- dept.before.1968.shp[order(dept.before.1968.shp$nom_dept),]
rownames(dept.before.1968.shp) <- 1:nrow(dept.before.1968.shp)

dept.after.1968.shp <- st_read("shapes/Dept_after_1968.shp", layer = "Dept_after_1968", stringsAsFactors = F)
dept.after.1968.shp <- dept.after.1968.shp[dept.after.1968.shp$code_dept %in% dept.id, ]
dept.after.1968.shp <- dept.after.1968.shp[order(dept.after.1968.shp$nom_dept),]
rownames(dept.after.1968.shp) <- 1:nrow(dept.after.1968.shp)

MLT$mx <- MLT$mx * 1000
FLT$mx <- FLT$mx * 1000


#######
years <- sort(unique(MLT[, "Year"]))
# Create a set of life tables by year for all geographies
for (i in 1:length(years)) {
  assign(paste0("MLT.", years[i]), MLT[MLT$Year == years[i], c("PopName", "Age", "mx", "ex")])
  assign(paste0("FLT.", years[i]), FLT[FLT$Year == years[i], c("PopName", "Age", "mx", "ex")])
}

# Creating a matching dictionary for rows and name-sorted departments in each period for geographic units
# (required for interactive use with DataTable within the app)
table.data.before.1968 <- st_set_geometry(dept.before.1968.shp[, c("code_dept", "nom_dept")], NULL)
match.sel.row.dept.before.1968 <- cbind(as.numeric(rownames(table.data.before.1968[order(table.data.before.1968$code_dept, decreasing = FALSE),])), 
                                        table.data.before.1968[order(table.data.before.1968$code_dept, decreasing = FALSE),])
colnames(match.sel.row.dept.before.1968) <- c("tableRow", "code_dept", "nom_dept")
match.sel.row.dept.before.1968 <- match.sel.row.dept.before.1968[order(match.sel.row.dept.before.1968$tableRow),]


table.data.after.1968 <- st_set_geometry(dept.after.1968.shp[, c("code_dept", "nom_dept")], NULL)
match.sel.row.dept.after.1968 <- cbind(as.numeric(rownames(table.data.after.1968[order(table.data.after.1968$code_dept, decreasing = FALSE),])), 
                                       table.data.after.1968[order(table.data.after.1968$code_dept, decreasing = FALSE),])
colnames(match.sel.row.dept.after.1968) <- c("tableRow", "code_dept", "nom_dept")
match.sel.row.dept.after.1968 <- match.sel.row.dept.after.1968[order(match.sel.row.dept.after.1968$tableRow),]

rm(table.data.before.1968, table.data.after.1968)

# Save processed files stored in the environment for faster processing
save.image("./map_dept.RData")
