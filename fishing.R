
#options(warn=-1) #remove warnings
options(warn=0) #print warnings

#Collapse — Alt+L
#Expand — Shift+Alt+L
#Collapse All — Alt+O
#Expand All — Shift+Alt+O
#Unactivate/activate — Shift+Alt+C

#to add or remove commenting for multiple lines of code: ctl+shift+c 

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)      
library(ggplot2)
library(ggbreak)
library(lubridate)
library(hms)
library(chron)
library(janitor)
library(apyramid)
library(FactoMineR)
library(factoextra)
library(fmsb)
library(stringi)
library(nnet)
library(MASS)
library(car)
library(tidyverse)
library(knitr)
library(kableExtra)
library(broom)
library(gtsummary)
library(ggeffects)
library(marginaleffects)
library(rpart)
library(rpart.plot)
library(writexl)
library(forecast)
library(animation)
library(gganimate)
library(magick)

`%notin%` <- Negate(`%in%`)


# import DBs and work on DBs ####

# survey

#NB among corrections to brought to the excel file prior importation, I had to correct for date, cfr in the excel file %d-%m-%y; however, some ":" as separator, empty " " space before the date etc. So in any case check for these for further data importation. 

Meta.survey <- read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "Metha_enquete_terrain_publique", col_names = T, range = "A14:D218")
Meta.survey <- arrange(Meta.survey, Libellé)
Meta.survey$Libellé
survey <- (read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "BDD_enquete_terrain_publique", col_names = T))[ , order(names(read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "BDD_enquete_terrain_publique", col_names = T)))]
names(survey)
sapply(survey, class)
# quid format hh:mm excel => imported as POSIXct ; if you want to force R to import it as character, it doen't work, so keep the automatic importation format. 
head(survey$date) ; class(survey$date)
head(survey$Temps_peche_effectif) ; class(survey$Temps_peche_effectif)
head(survey$h_debut) ; class(survey$h_debut)

unique(Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Binaire", "numeric", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Texte", "character", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Nombre", "character", Meta.survey$Format) #was numeric before, but some var supposed to be numeric must be imported as character cfr e.g. age variable
Meta.survey$Format <- ifelse(Meta.survey$Format == "Alphanumérique", "character", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Date", "date", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Horaire", "POSIXct", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "-", "character", Meta.survey$Format)
Meta.survey$Format <- ifelse(Meta.survey$Format == "Réel", "character", Meta.survey$Format) #was numeric before, but some var supposed to be numric must be imported as character cfr age variable
unique(Meta.survey$Format)

# issue with début & fin enquête var. importation
table(survey$deb_sortie)
survey$deb_sortie <- ifelse(survey$deb_sortie == "NA", NA, survey$deb_sortie)
table(survey$fin_sortie)
survey$fin_sortie <- ifelse(survey$fin_sortie == "NA", NA, survey$fin_sortie)
# transform them now in time format for below loop
survey$deb_sortie <- chron::times(as.numeric(survey$deb_sortie))
survey$fin_sortie <- chron::times(as.numeric(survey$fin_sortie))
survey$deb_sortie <- as.POSIXct(ifelse(is.na(survey$deb_sortie), NA, paste0("1899-12-31 ", as.character(survey$deb_sortie))))
survey$fin_sortie <- as.POSIXct(ifelse(is.na(survey$fin_sortie), NA, paste0("1899-12-31 ", as.character(survey$fin_sortie))))

intersect(unique(Meta.survey[,c("Libellé","Unité")])[["Libellé"]], unique(names(survey)))
length(unique(names(survey)))
length(names(survey))
survey[ , names(survey) %in% intersect(unique(Meta.survey[,c("Libellé","Unité")])[["Libellé"]], unique(names(survey)))]

for (i in 1:length(survey)) {
  
  #i=36
  names(survey[,i])
  typeof(unlist(survey[,i]))
  fmt. <- unique(filter(Meta.survey, Libellé == names(survey[,i]))[["Format"]])
  
  fmt. <- ifelse(length(fmt.) == 0, "null", fmt.)
  
  if (fmt. == "date") {
    survey[,i] <- as.Date(as.numeric(unlist(survey[,i])), origin = "1899-12-30", format = "%Y-%m-%d")
  } else if (fmt. == "POSIXct") {
    survey[,i] <- as.POSIXct(unlist(survey[,i]), origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
  } else if (fmt. == "numeric") {
    survey[,i] <- as.numeric(unlist(survey[,i]))
  } else if (fmt. == "character") {
    survey[,i] <- as.character(unlist(survey[,i]))
  } else {
    survey[,i]
  }
  
  rm(fmt.,i)
  
}

head(survey$date)
head(survey$Temps_peche_effectif) ; class(survey$Temps_peche_effectif)
head(survey$h_debut) ; class(survey$h_debut)

setdiff(names(survey), Meta.survey$Libellé)
survey[,setdiff(names(survey), Meta.survey$Libellé)[[1]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[1]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[2]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[2]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[3]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[3]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[4]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[4]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[5]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[5]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[6]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[6]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[7]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[7]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[8]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[8]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[9]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[9]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[10]]] <- as.character(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[10]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[11]]] <- as.character(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[11]]]))
# the two variables "Temps_peche_effectif" & "Temps_peche_estime" were automatically imported as POSIXct variable so don't change anything here
#survey[,setdiff(names(survey), Meta.survey$Libellé)[[12]]] <- as.POSIXct(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[12]]]), origin = "19700-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
#survey[,setdiff(names(survey), Meta.survey$Libellé)[[13]]] <- as.POSIXct(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[13]]]), origin = "1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
survey[,setdiff(names(survey), Meta.survey$Libellé)[[14]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[14]]]))
survey[,setdiff(names(survey), Meta.survey$Libellé)[[15]]] <- as.numeric(unlist(survey[,setdiff(names(survey), Meta.survey$Libellé)[[15]]]))

unique(survey$h_debut) ; survey$h_debut <- as_hms(survey$h_debut)
unique(survey$h_fin) ; survey$h_fin <- as_hms(survey$h_fin)
unique(survey$heure_enq) ; survey$heure_enq <- as_hms(survey$heure_enq)
unique(survey$Temps_peche_estime) ; survey$Temps_peche_estime <- as_hms(survey$Temps_peche_estime)
unique(survey$Temps_peche_effectif) ; survey$Temps_peche_effectif <- as_hms(survey$Temps_peche_effectif)
unique(survey$deb_sortie) ; survey$deb_sortie <- as_hms(survey$deb_sortie)
unique(survey$fin_sortie) ; survey$fin_sortie <- as_hms(survey$fin_sortie)

head(survey$date) ; class(survey$date)
head(survey$Temps_peche_effectif) ; class(survey$Temps_peche_effectif)
head(survey$h_debut) ; class(survey$h_debut)

Meta.survey <- read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "Metha_enquete_terrain_publique", col_names = T, range = "A14:D218")
Meta.survey$Libellé
union(names(survey),Meta.survey$Libellé)
setdiff(names(survey),Meta.survey$Libellé)
survey <- survey[,c(intersect(Meta.survey$Libellé,names(survey)), setdiff(names(survey),Meta.survey$Libellé))]

names. <- names(read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "BDD_enquete_terrain_publique", col_names = T))
survey <- survey[,names.]
rm(names.)

survey <- add_column(survey, BD = "PNMCA", .before = "mode_eqt")

# fishing data

fishing.Stareso <- read_excel("Enquetes_Peche_Loisir_PNMCCA_Chabrier_2023 (1).xlsx", sheet = "BD_Stareso", col_names = T, col_types = c(
  "numeric",
  "text",
  "text",
  "date",
  "text",
  "numeric",
  "numeric",
  "text",
  "text",
  "text",
  "text",
  "text",
  "numeric",
  "numeric",
  "text",
  "text",
  "numeric",
  "numeric",
  "date",
  "date",
  "date",
  "date",
  "text",
  "text",
  "text",
  "text", #many geographical zone
  "text", #many sites
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "text",
  "numeric",
  "numeric",
  "text"))
fishing.Stareso <- filter(fishing.Stareso, !is.na(fiche_n))
head(fishing.Stareso)
names(fishing.Stareso)

fishing.Stareso$heure_deb <- as_hms(fishing.Stareso$heure_deb)
fishing.Stareso$heure_enq <- as_hms(fishing.Stareso$heure_enq)
fishing.Stareso$heure_fin <- as_hms(fishing.Stareso$heure_fin)
fishing.Stareso$temps_pech <- as_hms(fishing.Stareso$temps_pech)

fishing.PNMCA <- read_excel("BDD_peche_loisir_2024_04_18.xlsx", sheet = "BDD_donnees_peche_du_jour", col_names = T, col_types = c(
  "numeric",
  "text",
  "text",
  "date",
  "text",
  "numeric",
  "numeric",
  "text",
  "text",
  "text",
  "text",
  "text",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "text", #lat not in the metadata
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "text", #long not in the metadata
  "text",
  "numeric",
  "numeric",
  "date",
  "date",
  "date",
  "date",
  "date",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text",
  "text", #cfr nom_scien not numeric but text, error in the metadata
  "numeric",
  "numeric",
  "numeric",
  "numeric", #I add to remove "g" and "-" in pds_sor var 168, 169, 185, 190, 253
  "text",
  "numeric",
  "numeric",
  "text"))
fishing.PNMCA <- filter(fishing.PNMCA, !is.na(fiche_n))
head(fishing.PNMCA)
names(fishing.PNMCA) # in contrast to the metadata file, the var id_enquete does not appear in the DB; in contrast to the DB, the var lat & long do not appear in the metadata

fishing.PNMCA$heure_deb <- as_hms(fishing.PNMCA$heure_deb)
fishing.PNMCA$heure_enq <- as_hms(fishing.PNMCA$heure_enq)
fishing.PNMCA$heure_fin <- as_hms(fishing.PNMCA$heure_fin)
fishing.PNMCA$temps_pech_effectif <- as_hms(fishing.PNMCA$temps_pech_effectif)
fishing.PNMCA$temps_peche_estime <- as_hms(fishing.PNMCA$temps_peche_estime)

# variables in common and diff

# common var
intersect(names(fishing.Stareso), names(fishing.PNMCA))
# var in PNMCA but not in Stareso
setdiff(names(fishing.PNMCA), names(fishing.Stareso))
# var in Stareso but not in PNMCA
setdiff(names(fishing.Stareso), names(fishing.PNMCA))
# union
union(setdiff(names(fishing.PNMCA), names(fishing.Stareso)), setdiff(names(fishing.Stareso), names(fishing.PNMCA)))

#some var obviously are identical, although with diff names: 
# "y_lat_DD" vs "lat (°N)"
# "x_lon_DD" vs "long ((°E)" 
# "temps_pech_effectif" vs "temps_pech" 

head(fishing.PNMCA$y_lat_DD)
head(fishing.Stareso$`lat (°N)`)
names(fishing.Stareso)[names(fishing.Stareso) == "lat (°N)"] <- "y_lat_DD"

head(fishing.PNMCA$x_lon_DD)
head(fishing.Stareso$`long (°E)`)
names(fishing.Stareso)[names(fishing.Stareso) == "long (°E)"] <- "x_lon_DD"

head(fishing.PNMCA$temps_peche_estime) 
head(fishing.PNMCA$temps_pech_effectif) 
head(fishing.Stareso$temps_pech) #should I consider Stareso temps_pech as PNMCA temps_pech_effectif or temps_peche_estime ? temps_pech_effectif seems to be more logical cfr field observation and only one temps_peche considered in Stareso DB.  
names(fishing.Stareso)[names(fishing.Stareso) == "temps_pech"] <- "temps_pech_effectif"

# merge PNMCA & Stareso dfs and add BD variable to all dfs.

fishing.PNMCA <- add_column(fishing.PNMCA, BD = "PNMCA", .before = "fiche_n")
fishing.Stareso <- add_column(fishing.Stareso, BD = "Stareso", .before = "fiche_n")
#fishing.Sta.PNMCA <- bind_rows(fishing.PNMCA, fishing.Stareso)
fishing <- bind_rows(fishing.PNMCA, fishing.Stareso)

fishing <- fishing[, c(c(intersect(names(fishing.Stareso), names(fishing.PNMCA))), c(setdiff(names(fishing.PNMCA), names(fishing.Stareso))), c(setdiff(names(fishing.Stareso), names(fishing.PNMCA))))]

# In fishing DB there is an issue in the calcution of the CPUE, cfr formula in excel with the wrong temps de peche effectif from some observations on. And also for CPUE total as a matter of fact (these variable is filled with NAs, could be recalculated if needed)

fishing$`CPUE total` <- NA
fishing$CPUE <- NA
fishing$CPUE <- fishing$poids_g/((as.numeric(times(fishing$temps_pech_effectif)))*24)/fishing$nb_pecheur

# compa DB

names(fishing)
names(survey)
setdiff(names(fishing), names(survey))
intersect(names(fishing), names(survey))

#some variable do not coincide between fishing and survey dfs; we have to make names matching for comparison purpose between fishing vs survey! 

class(fishing$mod_pech) ; class(survey$mod_peche) 
names(fishing)[names(fishing) == "mod_pech"] <- "mod_peche"

class(fishing$lieu_residence) ; class(survey$commune) 
names(fishing)[names(fishing) == "lieu_residence"] <- "commune"

class(fishing$x_lon) ; class(survey$x_degres) 
names(fishing)[names(fishing) == "long (°E)"] <- "x_degres"
#NB: the var survey$x_degres makes no sense

class(fishing$y_lat) ; class(survey$y_degres) 
names(fishing)[names(fishing) == "lat (°N)"] <- "y_degres"
#NB: the var survey$y_degres makes no sense

class(fishing$app_autre) ; class(survey$autre_app) 
names(fishing)[names(fishing) == "app_autre"] <- "autre_app"

class(fishing$fond_rech) ; class(survey$fonds_rech) 
names(fishing)[names(fishing) == "fond_rech"] <- "fonds_rech"

class(fishing$heure_enq) ; class(survey$heure_enq) 
names(fishing)[names(fishing) == "heure_enq"] <- "heure_enq"

class(fishing$temps_pech_effectif) ; class(survey$Temps_peche_effectif) 
names(fishing)[names(fishing) == "temps_pech_effectif"] <- "Temps_peche_effectif"

class(fishing$heure_fin) ; class(survey$h_fin) 
names(fishing)[names(fishing) == "heure_fin"] <- "h_fin"

class(fishing$heure_deb) ; class(survey$h_debut) 
names(fishing)[names(fishing) == "heure_deb"] <- "h_debut"

class(fishing$prof_m) ; class(survey$prof_m_moyen) 
names(fishing)[names(fishing) == "prof_m"] <- "prof_m_moyen"

class(fishing$nebulosite) ; class(survey$neb) 
#variable neb in survet DB can be changed to character cfr xx-xx% in fishing DB. Change we'll be made in case of DB fusion.
names(fishing)[names(fishing) == "nebulosite"] <- "neb"

class(fishing$force_vent) ; class(survey$force_v) 
# format diff cfr not only num for fishing
names(fishing)[names(fishing) == "force_vent"] <- "force_v"

class(fishing$saisie) ; class(survey$Saisie) 
names(fishing)[names(fishing) == "saisie"] <- "Saisie"


# some more step by step work on the data according to below code of analysis ####

head(survey$date)
survey %>% separate(date, into = c("a", "m", "j"), remove = F) -> survey
head(fishing$date)
fishing %>% separate(date, into = c("a", "m", "j"), remove = F) -> fishing

unique(survey$sexe)
survey$sexe <- ifelse(survey$sexe == "Homme", "homme", survey$sexe)

survey$age_moyen <- as.numeric(survey$age_moyen)

unique(fishing$mod_peche)
fishing$mod_peche <- ifelse(fishing$mod_peche == "PDB", "pdb", fishing$mod_peche)
fishing$mod_peche <- ifelse(fishing$mod_peche == "PE", "pe", fishing$mod_peche)
fishing$mod_peche <- ifelse(fishing$mod_peche == "CSM", "csm", fishing$mod_peche)

#remove "NA" & NA rows preencoded in the excel db
ifelse(survey$fiche_n == "NA", "", survey$fiche_n) -> survey$fiche_n
filter(survey, fiche_n != "") -> survey 

saveRDS(survey, "Raw data_mined/enquetes.rds")
write.csv2(survey, "Raw data_mined/enquetes.csv")
saveRDS(fishing, "Raw data_mined/terrain.rds")
write.csv2(fishing, "Raw data_mined/terrain.csv")

 
# correct individual data based on Michel and Quentin corrections

survey <- readRDS("Raw data_mined/enquetes.rds")
fishing <- readRDS("Raw data_mined/terrain.rds")

# upload terrain data corrected by Michel directly in the initial compiled terrain csv and opened in excel
fishing.new <- read_excel("Raw data_mined/BDD_peche_loisir_2024_09_27.xlsx", sheet = "Pêche du jour", col_names = T
                          #, col_types = c(
#  "text",
#  "numeric",
#  "text",
#  "text",
#  "text",
#  "numeric",
#  "text",
#  "numeric",
#  "numeric",
#  "text",
#  "numeric",
#  "text",
#  "numeric",
#  "text",
#  "numeric",
#  "numeric",
#  "numeric",
#  "numeric",
#  "numeric")
)
names(fishing)
names(fishing.new)
# fishing.new$Temps_peche_effectif <- as_hms(fishing.new$Temps_peche_effectif)
# #columns_to_remove <- c("a","y_lat_DD", "x_lon_DD") #cfr format diff. when reimported
# #fishing.new <- fishing.new %>% select(-all_of(columns_to_remove))
# 
# head(fishing$date); class(fishing$date) 
# head(fishing.new$date); class(fishing$date) 
# #as.POSIXct(fishing.new$date, origin = "1970-01-01", format = "%Y-%m-%d")
# fishing.new$date <- as.POSIXct(as.integer(fishing.new$date) * 86400, origin = "1899-12-30", tz = "UTC")
# 
# head(fishing$y_lat_DD) ; class(fishing$y_lat_DD) 
# fishing$y_lat_DD <- round(fishing$y_lat_DD, digits = 5)
# head(fishing.new$y_lat_DD) ; class(fishing.new$y_lat_DD) 
# fishing.new$y_lat_DD <- round(fishing.new$y_lat_DD, digits = 5)
# 
# head(fishing$x_lon_DD) ; class(fishing$x_lon_DD) 
# fishing$x_lon_DD <- round(fishing$x_lon_DD, digits = 5)
# head(fishing.new$x_lon_DD) ; class(fishing.new$x_lon_DD)
# fishing.new$x_lon_DD <- as.numeric(fishing.new$x_lon_DD)
# fishing.new$x_lon_DD <- round(fishing.new$x_lon_DD, digits = 5)
# 
# names(fishing)
# names(fishing.new)
# intersect(names(fishing), names(fishing.new))
# 
# fishing <- add_column(fishing, join.var = paste0(
#   #fishing$BD, "_", fishing$fiche_n, "_", fishing$id_sortie, "_", fishing$id_obs, "_", 
#   fishing$date, "_", fishing$Saisie, "_", fishing$mod_peche, "_", fishing$nb_pecheur, "_", fishing$Temps_peche_effectif
#   , "_", fishing$y_lat_DD, "_", fishing$x_lon_DD
#                                                  ), .before = "BD")
# fishing.new <- add_column(fishing.new, join.var = paste0(
#   #fishing.new$BD, "_", fishing.new$fiche_n, "_", fishing.new$id_sortie, "_", fishing.new$id_obs, "_", 
#   fishing.new$date, "_", fishing.new$Saisie, "_", fishing.new$mod_peche, "_", fishing.new$nb_pecheur, "_", fishing.new$Temps_peche_effectif
#   , "_", fishing.new$y_lat_DD, "_", fishing.new$x_lon_DD
# ), .before = "BD")
# 
# # Find the common columns
# #common_vars <- intersect(names(fishing), names(fishing.new))
# # Arrange both dataframes by the common variables
# #fishing <- fishing %>% arrange(across(all_of(common_vars)))
# #fishing.new <- fishing.new %>% arrange(across(all_of(common_vars)))
# fishing <- fishing %>% arrange(join.var)
# fishing.new <- fishing.new %>% arrange(join.var)
# #intersect(fishing$join.var, fishing.new$join.var)
# 
# # => impossible to create a unique id to merge fishing and fishing.bis dataframe because of data mixing issue in fishing.bis, cfr Michel. 
# #result <- full_join(fishing, fishing.new, by = "join.var")
# 
# fishing <- fishing %>% select(-join.var)
# fishing.new <- fishing.new %>% select(-join.var)

# unique species names to create the df for correction purpose by Michel and Quentin

#fishing <- readRDS("Raw data_mined/terrain.rds")

# upload terrain data corrected by Michel directly in the initial compiled terrain csv and opened in excel
#fishing.new <- read_excel("Raw data_mined/BDD_peche_loisir_2024_09_27.xlsx", sheet = "Pêche du jour", col_names = T)
#names(fishing)
#names(fishing.new)

fishing.new$Temps_peche_effectif <- as_hms(fishing.new$Temps_peche_effectif)

head(fishing$date); class(fishing$date) 
head(fishing.new$date); class(fishing$date) 
#as.POSIXct(fishing.new$date, origin = "1970-01-01", format = "%Y-%m-%d")
fishing.new$date <- as.POSIXct(as.integer(fishing.new$date) * 86400, origin = "1899-12-30", tz = "UTC")

fishing.new$tail_cm <- as.numeric(fishing.new$tail_cm)
head(fishing$y_lat_DD) ; class(fishing$y_lat_DD) 
fishing$y_lat_DD <- round(fishing$y_lat_DD, digits = 5)
head(fishing.new$y_lat_DD) ; class(fishing.new$y_lat_DD) 
fishing.new$y_lat_DD <- round(fishing.new$y_lat_DD, digits = 5)

head(fishing$x_lon_DD) ; class(fishing$x_lon_DD) 
fishing$x_lon_DD <- round(fishing$x_lon_DD, digits = 5)
head(fishing.new$x_lon_DD) ; class(fishing.new$x_lon_DD)
fishing.new$x_lon_DD <- as.numeric(fishing.new$x_lon_DD)
fishing.new$x_lon_DD <- round(fishing.new$x_lon_DD, digits = 5)

names(fishing)
names(fishing.new)
intersect(names(fishing), names(fishing.new))

sort(unique(fishing$nom_scien))
sort(unique(fishing.new$nom_scien))
intersect(sort(unique(fishing$nom_scien)), sort(unique(fishing.new$nom_scien)))
length(sort(unique(fishing$nom_scien))) ;  length(sort(unique(fishing.new$nom_scien))) ; length(intersect(sort(unique(fishing$nom_scien)), sort(unique(fishing.new$nom_scien))))

sort(setdiff(as.vector(unique(fishing$nom_scien)), as.vector(unique(fishing.new$nom_scien))))
sp. <- as.vector(sort(unique(fishing$nom_scien)))
sp.
sp.new <- as.vector(sort(unique(fishing.new$nom_scien)))
sp.new

fishing$nom_scien <- ifelse(fishing$nom_scien %in% c("Liligo sp.", "Loligo spp", "Loligo ssp"), "Loligo sp.", fishing$nom_scien)
fishing$nom_scien <- ifelse(fishing$nom_scien == "Mugil sp", "Mugil sp.", fishing$nom_scien)
fishing$nom_scien <- ifelse(fishing$nom_scien == "Scorpaena scofa", "Scorpaena scrofa", fishing$nom_scien)
fishing$nom_scien <- ifelse(fishing$nom_scien == "Seriola dumerilii", "Seriola dumerili", fishing$nom_scien)
fishing$nom_scien <- ifelse(fishing$nom_scien == "serranus sp.", "Serranus sp.", fishing$nom_scien)
fishing$nom_scien <- ifelse(fishing$nom_scien %in% c("Spondiliosomas cantharus", "Spondyliosoma cantharus "), "Spondyliosoma cantharus", fishing$nom_scien)

# Michel check
fishing
sort(unique(fishing$nom_scien))
filter(fishing, nom_scien == "calamar rouge") -> check #supprimer
filter(fishing, nom_scien != "calamar rouge") -> fishing
filter(fishing, nom_scien == "Conger conger") -> check #garder ; et faire relation taille poids cfr identifié à l'espèce.
filter(fishing, nom_scien == "crabes") -> check #supprimer
filter(fishing, nom_scien != "crabes") -> fishing
filter(fishing, nom_scien == "Diplodus sp.") -> check #garder
filter(fishing, nom_scien == "gobie") -> check #supprimer
filter(fishing, nom_scien != "gobie") -> fishing
filter(fishing, nom_scien == "Mullus spp") -> check #remplacer par Mullus sp. 
ifelse(fishing$nom_scien == "Mullus spp", "Mullus sp.", fishing$nom_scien) -> fishing$nom_scien
filter(fishing, nom_scien == "Sepia officinalis") -> check #garder
filter(fishing, nom_scien == "Serranus sp.") -> check #garder
filter(fishing, nom_scien == "Sphyraena barracuda") -> check #garder ; et faire relation taille poids cfr identifié à l'espèce (sauf pr 1 individu pas la taille)
filter(fishing, nom_scien == "Sphyraena sp.") -> check #garder
filter(fishing, nom_scien == "Thalassoma pavo") -> check  #garder ; et faire relation taille poids cfr identifié à l'espèce

unique(fishing$nom_scien)
filter(fishing, nom_scien %in% c("0", "NA")) -> check
filter(fishing, nom_scien %in% c("NA")) -> check #trasher cette observation qui n'a pas de sens
filter(fishing, nom_scien != "NA") -> fishing
filter(fishing, nom_scien %in% c("0")) 
#filter(fishing.Sta.PNMCA, nom_scien %in% c("0", "NA")) -> check
#filter(fishing.Sta.PNMCA, nom_scien %in% c("NA")) 
#filter(fishing.Sta.PNMCA, nom_scien %in% c("0")) 

fishing.saved <- fishing 
#fishing <- fishing.saved 
survey.saved <- survey

# Now remove aberrant data, after adding a new variable corresponding to row number for colleagues to make their corrections.

add_column(fishing, obs.nb = c(1:nrow(fishing)), .before = "BD") -> fishing
add_column(survey, obs.nb = c(1:nrow(survey)), .before = "BD") -> survey

filter(fishing, obs.nb == 325)
fishing[325,c("tail_cm", "poids_g", "poids_sor")]
fishing[325,c("tail_cm", "poids_g", "poids_sor")] <- NA
fishing[325,c("tail_cm", "poids_g", "poids_sor")]

filter(fishing, obs.nb %in% c(798:803))
fishing[c(798:803),c("nb_ind", "tail_cm")]
tail_cm <- as.vector(fishing[c(798:803),c("nb_ind")])$nb_ind
nb_ind <- as.vector(fishing[c(798:803),c("tail_cm")])$tail_cm
fishing[c(798:803),c("nb_ind")] <- nb_ind
fishing[c(798:803),c("tail_cm")] <- tail_cm
fishing[c(798:803),c("nb_ind", "tail_cm")]

cor.qf <- c(
79,
80,
109,
141,
143,
144,
145,
176,
177,
178,
180,
181,
182,
183,
184,
185,
186,
188,
189,
190,
248,
249,
335,
336,
340,
355,
356,
357,
362,
363,
364,
365,
366,
367,
368,
369,
370,
371,
372,
373,
374,
388,
389,
390,
391,
392,
393,
394,
395,
396,
397,
398,
399,
400,
401,
402,
403,
410,
411,
412,
417,
418,
420,
421,
422,
428,
429,
477,
478,
479,
486
)

coo <- fishing[cor.qf, c("y_lat_DD", "x_lon_DD", "y_lat", "y_degre", "y_min_dec", "x_lon", "x_degre", "x_min_dec")]
rm(coo)
fishing[cor.qf, c("y_lat_DD", "x_lon_DD", "y_lat", "y_degre", "y_min_dec", "x_lon", "x_degre", "x_min_dec")] <- NA
fishing[cor.qf, c("y_lat_DD", "x_lon_DD", "y_lat", "y_degre", "y_min_dec", "x_lon", "x_degre", "x_min_dec")]

# save final results

fishing <- fishing[,2:ncol(fishing)]
survey <- survey[,2:ncol(survey)]

saveRDS(fishing, "fishing.rds")
write.csv2(fishing, "fishing.csv")
write_xlsx(fishing, "fishing.xlsx")

saveRDS(survey, "survey.rds")
write.csv2(survey, "survey.csv")
write_xlsx(survey, "survey.xlsx")


# update plot & analysis report 2022 (load rds data) ####


# plot fishers vs sex  ####

unique(survey$sexe)
unique(survey$a)
df. <- na.omit(survey[, c("a", "sexe")])
unique(df.$a)

df. %>%  filter(a %in% c("2020","2021","2022", "2023", "2024")) %>% group_by(sexe) %>% count() -> data
data <- data.frame(data)
data$fraction = (data$n/sum(data$n))*100
data

ggplot(data, aes(x="", y=fraction, fill=sexe)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2024 (n=", sum(data$n), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5))  # Ajouter les pourcentages

ggsave("Figs/Sex/sex_2020-2024.png", width = 5, height = 4)

df. %>%  filter(a %in% c("2020","2021","2022")) %>% group_by(sexe) %>% count() -> data.1
data.1 <- data.frame(data.1)
data.1$fraction = (data.1$n/sum(data.1$n))*100
data.1

ggplot(data.1, aes(x="", y=fraction, fill=sexe)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2022 (n=", sum(data.1$n), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        #plot.background=element_blank()
        ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5))  # Ajouter les pourcentages

ggsave("Figs/Sex/sex_2020-2022.png", width = 5, height = 4)

df. %>%  filter(a %in% c("2023", "2024")) %>% group_by(sexe) %>% count() -> data.2
data.2 <- data.frame(data.2)
data.2$fraction = (data.2$n/sum(data.2$n))*100
data.2

ggplot(data.2, aes(x="", y=fraction, fill=sexe)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2023-2024 (n=", sum(data.2$n), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5))  # Ajouter les pourcentages

ggsave("Figs/Sex/sex_2023-2024.png", width = 5, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# plot age pyramid by sex ####

unique(survey$sexe)
unique(survey$a)
sort(unique(survey$age_moyen))
df. <- survey[, c("a", "sexe", "age_moyen")]
#df. <- na.omit(survey[, c("a", "sexe", "age_moyen")])
df.$sexe <- factor(df.$sexe, levels = c("femme", "homme"))

df. <- add_column(df., age_classe = NA, .after = "age_moyen")
df.$age_classe <- as.character(df.$age_classe)
df. <- data.frame(df.)
df.$age_classe <- ifelse(df.$age_moyen >= 70, "70 et plus", df.$age_classe) 
df.$age_classe <- ifelse(df.$age_moyen >= 60 & df.$age_moyen < 70, "60-69", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 50 & df.$age_moyen < 60, "50-59", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 40 & df.$age_moyen < 50, "40-49", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 25 & df.$age_moyen < 40, "25-39", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen < 25, "13-24", df.$age_classe)
unique(df.$age_classe)

df.$age_classe <- as.factor(df.$age_classe)

df. %>% tabyl(age_classe, sexe)
df. %>% tabyl(age_classe, sexe, a) #we have no info about age_moyen in 2023 & 2024 so not possible to plot the age pyramid!

apyramid::age_pyramid(data = na.omit(df.),
                      age_group = "age_classe",
                      split_by = "sexe",
                      proportional = TRUE,
                      show_midpoint = FALSE)

na.omit(df.) %>%  filter(a %in% c("2020","2021","2022")) -> data.1

# create dataset with proportion of total
pyramid_data <- data.1 %>%
  count(age_classe,
        sexe,
        name = "counts") %>% 
  #ungroup() %>%                 # ungroup so percents are not by group
  group_by(sexe) %>%             # group by sex so percents are not by group
  mutate(percent = round(100*(counts / sum(counts, na.rm=T)), digits = 1), 
         percent = case_when(
           sexe == "homme" ~ percent,
           sexe == "femme" ~ -percent,     # convert male to negative
           TRUE            ~ NA_real_))    # NA val must by numeric as well

max_per <- max(pyramid_data$percent, na.rm=T)
min_per <- min(pyramid_data$percent, na.rm=T)

# begin ggplot
ggplot()+  # default x-axis is age in years;
  
  # case data graph
  geom_col(data = pyramid_data,
           mapping = aes(
             x = age_classe,
             y = percent,
             fill = sexe),         
           colour = "white")+       # white around each bar
  
  # flip the X and Y axes to make pyramid vertical
  coord_flip()+
  
  # adjust the axes scales
  # scale_x_continuous(breaks = seq(0,100,5), labels = seq(0,100,5)) +
  scale_y_continuous(
    limits = c(#min_per, max_per)
             -50,50),
    breaks = seq(from = -50 #floor(min_per)
                 ,                # sequence of values, by 2s
                 to = 50#ceiling(max_per)
                 ,
                 by = 10),
    labels = paste0(abs(seq(from = -50 #floor(min_per)
                            ,     # sequence of absolute values, by 2s, with "%"
                            to = 50 #ceiling(max_per)
                            ,
                            by = 10)),
                    "%"))+  
  
  # designate colors and legend labels manually
  scale_fill_manual(
    values = c("femme" = "#F8766D",
               "homme" = "#00BFC4"),
    labels = c(paste0("femme (n=", sum((filter(pyramid_data, sexe=="femme"))[,"counts"], na.rm = T), ")"), paste0("homme (n=", sum((filter(pyramid_data, sexe=="homme"))[,"counts"], na.rm = T), ")")))+
  
  # label values (remember X and Y flipped now)
  labs(
    title = "2020-2022",
    x = "Classes d'âge",
    y = "Pourcentage par sexe",
    fill = NULL#,
    #caption = stringr::str_glue("Data are from data.1 \nn = {nrow(data.1)} (age or sex missing for {sum(is.na(survey$sexe) | is.na(survey$age_moyen))} cases) \nData plot as of: {format(Sys.Date(), '%d %b %Y')}")
    )+
  
  # display themes
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust=0, size=11, face = "italic")
  )

ggsave("Figs/pyramid.age_2020-2022.png", width = 7, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# boxplot age vs fishing mode ####

unique(survey$mod_peche)
table(survey$mod_peche)
unique(survey$a)
df. <- survey[, c("age_moyen", "a", "mod_peche")]
unique(df.$a) 
table(df.[, c("a", "mod_peche")] %>% group_by(a, mod_peche))
table(df.[, c("a", "age_moyen", "mod_peche")] %>% group_by(a, age_moyen, mod_peche))

df. %>% filter(mod_peche != "pdb, csm") -> df.
table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée", "peche à l'oursin"))

table(df.$mod_peche) #we have no info about age_moyen in 2023 & 2024 so not possible to plot the age pyramid!

df. %>% filter(a %in% c("2020","2021","2022") & mod_peche != "peche à l'oursin") -> data.1
droplevels(data.1) -> data.1
data.1 %>% group_by(mod_peche) %>% summarize(age.moy. = mean(age_moyen, na.rm = T))

p. <- ggplot(filter(data.1, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=age_moyen)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("âge") +
  ggtitle("2020-2022") +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = age_moyen)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = mean(unclass((filter(data.1, mod_peche == "peche à l'oursin")["age_moyen"]))$age_moyen)), shape=20, size=5, color="red", fill="red")
p.
plot_data <- ggplot_build(p.)$data

ggsave("Figs/mode.peche.vs.age_2020-2022.png", width = 5, height = 4)
  
rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# plot proportion mod_pech ####

unique(survey$mod_peche)
table(survey$mod_peche)
unique(survey$a)
df. <- survey[, c("age_moyen", "a", "mod_peche")]
unique(df.$a) 

table(df.[, c("a", "mod_peche")] %>% group_by(a, mod_peche))

df. %>% filter(mod_peche != "pdb, csm") -> df.
table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée", "peche à l'oursin"))
table(df.$mod_peche)

df. %>%  filter(a %in% c("2020","2021","2022", "2023", "2024") & mod_peche != "peche à l'oursin") %>% group_by(mod_peche) %>% count() -> data
data <- data.frame(data)
data$fraction = (data$n/sum(data$n))*100
data
names(data) <- c("technique", "n", "fraction")

position_jitter_stack <- function(vjust = 1, reverse = FALSE, 
                                  jitter.width = 1, jitter.height = 1,
                                  jitter.seed = NULL, offset = NULL) {
  ggproto(NULL, PositionJitterStack, vjust = vjust, reverse = reverse, 
          jitter.width = jitter.width, jitter.height = jitter.height,
          jitter.seed = jitter.seed, offset = offset)
}

PositionJitterStack <- ggproto("PositionJitterStack", PositionStack,
                               type = NULL,
                               vjust = 1,
                               fill = FALSE,
                               reverse = FALSE,
                               jitter.height = .5,
                               jitter.width = .5,
                               jitter.seed = NULL,
                               offset = 1,
                               
                               setup_params = function(self, data) {
                                 list(
                                   var = self$var %||% ggplot2:::stack_var(data),
                                   fill = self$fill,
                                   vjust = self$vjust,
                                   reverse = self$reverse,
                                   jitter.height = self$jitter.height,
                                   jitter.width = self$jitter.width,
                                   jitter.seed = self$jitter.seed,
                                   offset = self$offset
                                 )
                               },
                               
                               setup_data = function(self, data, params) {
                                 data <- PositionStack$setup_data(data, params)
                                 if (!is.null(params$offset)) {
                                   data$to_jitter <- sapply(seq(nrow(data)), function(i) {
                                     any(abs(data$y[-i] - data$y[i]) <= params$offset)
                                   })
                                 } else {
                                   data$to_jitter <- TRUE
                                 }
                                 data
                               },
                               
                               compute_panel = function(data, params, scales) {
                                 data <- PositionStack$compute_panel(data, params, scales)
                                 
                                 jitter_df <- data.frame(width = params$jitter.width,
                                                         height = params$jitter.height)
                                 
                                 if (!is.null(params$jitter.seed)) jitter_df$seed = params$jitter.seed
                                 jitter_positions <- PositionJitter$compute_layer(
                                   data[data$to_jitter, c("x", "y")],
                                   jitter_df
                                 )
                                 
                                 data$x[data$to_jitter] <- jitter_positions$x
                                 data$y[data$to_jitter] <- jitter_positions$y
                                 
                                 data
                               }
)

ggplot(data, aes(x="", y=fraction, fill=technique)) + #y=n
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2024 (n=", sum(data$n,na.rm = T), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) + 
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #geom_text(aes(label = n), position = position_stack(vjust = 0.5))
  #geom_text(aes(label = scales::percent(fraction/100)), position = position_jitter_stack(vjust = 0.5)) # Ajouter les pourcentages
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5)) # Ajouter les pourcentages

ggsave("Figs/Technique.peche/tech_2020-2024.png", width = 7, height = 4)

df. %>%  filter(a %in% c("2020","2021","2022") & mod_peche != "peche à l'oursin") %>% group_by(mod_peche) %>% count() -> data.1
data.1 <- data.frame(data.1)
data.1$fraction = (data.1$n/sum(data.1$n))*100
data.1
names(data.1) <- c("technique", "n", "fraction")

ggplot(data.1, aes(x="", y=fraction, fill=technique)) + #y=n
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2022 (n=", sum(data.1$n,na.rm = T), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) + 
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #geom_text(aes(label = n), position = position_stack(vjust = 0.5))
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5))  # Ajouter les pourcentages

ggsave("Figs/Technique.peche/tech_2020-2022.png", width = 7, height = 4)

df. %>%  filter(a %in% c("2023", "2024") & mod_peche != "peche à l'oursin") %>% group_by(mod_peche) %>% count() -> data.2
data.2 <- data.frame(data.2)
data.2$fraction = (data.2$n/sum(data.2$n))*100
data.2
names(data.2) <- c("technique", "n", "fraction")

ggplot(data.2, aes(x="", y=fraction, fill=technique)) + #y=n
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2023-2024 (n=", sum(data.2$n,na.rm = T), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) + 
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #geom_text(aes(label = n), position = position_stack(vjust = 0.5))
  geom_text(aes(label = scales::percent(fraction/100)), position = position_stack(vjust = 0.5))  # Ajouter les pourcentages

ggsave("Figs/Technique.peche/tech_2023-2024.png", width = 7, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# prepare df. to perform multivariate analysis ####

# in below multivariate analysis, I have included all the variables of the survey df used in Michel 2022 report, among them those of the "AFM" (majority) but also those of the summary table and associated Kiviat diagram. For many varaibles, there are no data for year 2023 !
# => in 2O23: raw. var.: mod_peche, res_tour, sexe, Temps_peche_effectif, Temps_peche_estime, zone & zone.gp ("z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61")
# => not in 2023: raw var.: age_moyen (& age_classe), cat_pro (& cat_pro.gp), nb_sort_an (& nb_sort_an_cat), avis_pnm, poss_bat, dep (& dep.median, dep.mod.peche, etc) ; calculated var. : time.week ("we", "sem"), time.day & time.day.gp ("aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")

unique(survey$mod_peche); length(na.omit(survey$mod_peche)) ; table(survey$mod_peche) ; table(survey$a, survey$mod_peche) 
unique(survey$age_moyen); length(na.omit(survey$age_moyen)) ; table(survey$age_moyen) ; table(survey$a, survey$age_moyen) #no data for 2023 & 2024
#profil ?? #not possible to find out what is the "profil" group of/unique variables used by Michel in the MFA!
unique(survey$act_pro); length(na.omit(survey$act_pro)) ; table(survey$act_pro) ; table(survey$a, survey$act_pro) #no sense to many categories, or merge act_pro ; no data for 2023 & 2024; not considered in the analysis.
unique(survey$cat_pro); length(na.omit(survey$cat_pro)) ; table(survey$cat_pro) ; table(survey$a, survey$cat_pro) #no data for 2023 & 2024
unique(survey$nb_sort_an); length(na.omit(survey$nb_sort_an)) ; table(survey$nb_sort_an) ; table(survey$a, survey$nb_sort_an) #no data for 2023 & 2024
unique(survey$avis_pnm); length(na.omit(survey$avis_pnm)) ; table(survey$avis_pnm) ; table(survey$a, survey$avis_pnm) #no data for 2023 & 2024
unique(survey$res_tour); length(na.omit(survey$res_tour)) ; table(survey$res_tour) ; table(survey$a, survey$res_tour)
unique(survey$sexe); length(na.omit(survey$sexe)) ; table(survey$sexe) ; table(survey$a, survey$sexe)
unique(survey$poss_bat); length(na.omit(survey$poss_bat)) ; table(survey$poss_bat) ; table(survey$a, survey$poss_bat) #no data for 2023 & 2024
unique(survey$Temps_peche_effectif); length(na.omit(survey$Temps_peche_effectif)) ; table(survey$Temps_peche_effectif) ; table(survey$a, survey$Temps_peche_effectif) 
unique(survey$Temps_peche_estime); length(na.omit(survey$Temps_peche_estime)) ; table(survey$Temps_peche_estime) ; table(survey$a, survey$Temps_peche_estime) 

unique(survey$a)
df. <- survey[, c("fiche_n", "a", "mod_peche", "age_moyen", "cat_pro", "nb_sort_an", "avis_pnm"
                  , "sexe"
                  , "dep_pdb", "dep_pe", "dep_csm", "dep_po"
                  , "res_tour"
                  , "espcib1", "espcib2", "espcib3"
                  , "we", "sem"
                  , "aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q"
                  , "z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61"
                  , "poss_bat"
                  , "Temps_peche_effectif"
                  , "Temps_peche_estime")]

unique(df.$poss_bat); length(na.omit(df.$poss_bat)) ; table(df.$poss_bat) ; table(df.$a, df.$poss_bat) 
df.$poss_bat <- ifelse(df.$poss_bat == "NA", NA, df.$poss_bat)
unique(df.$poss_bat); length(na.omit(df.$poss_bat)) ; table(df.$poss_bat) ; table(df.$a, df.$poss_bat)  

df. %>%
  mutate(Temps_peche_effectif.30min = if_else(Temps_peche_effectif < parse_hms("00:30:00"), NA, as_hms(Temps_peche_effectif))) -> df.

df.$Temps_peche_effectif.cat2h <- NA
for (i in 1:length(df.$Temps_peche_effectif.30min)) {
  #i=1
  if (as.numeric(df.[i, "Temps_peche_effectif.30min"]) < as.numeric(as.hms("02:00:00")) & !is.na(df.[i, "Temps_peche_effectif.30min"])) {
    df.[i, "Temps_peche_effectif.cat2h"] <- "<2h"
    } else if (as.numeric(df.[i, "Temps_peche_effectif.30min"]) >= as.numeric(as.hms("02:00:00")) & !is.na(df.[i, "Temps_peche_effectif.30min"])) {
      df.[i, "Temps_peche_effectif.cat2h"]  <- ">=2h"
      }  else if (is.na(df.[i, "Temps_peche_effectif.30min"])) {
        df.[i, "Temps_peche_effectif.cat2h"]  <- NA
        } 
rm(i)
}

df. %>%
  mutate(Temps_peche_estime.30min = if_else(Temps_peche_estime < parse_hms("00:30:00"), NA, as_hms(Temps_peche_estime))) -> df.

df.$Temps_peche_estime.cat2h <- NA
for (i in 1:length(df.$Temps_peche_estime.30min)) {
  #i=1
  if (as.numeric(df.[i, "Temps_peche_estime.30min"]) < as.numeric(as.hms("02:00:00")) & !is.na(df.[i, "Temps_peche_estime.30min"])) {
    df.[i, "Temps_peche_estime.cat2h"] <- "<2h"
  } else if (as.numeric(df.[i, "Temps_peche_estime.30min"]) >= as.numeric(as.hms("02:00:00")) & !is.na(df.[i, "Temps_peche_estime.30min"])) {
    df.[i, "Temps_peche_estime.cat2h"]  <- ">=2h"
  }  else if (is.na(df.[i, "Temps_peche_estime.30min"])) {
    df.[i, "Temps_peche_estime.cat2h"]  <- NA
  } 
  rm(i)
}

unique(df.$Temps_peche_effectif.cat2h); length(na.omit(df.$Temps_peche_effectif.cat2h)) ; table(df.$Temps_peche_effectif.cat2h) ; table(df.$a, df.$Temps_peche_effectif.cat2h)  
unique(df.$Temps_peche_estime.cat2h); length(na.omit(df.$Temps_peche_estime.cat2h)) ; table(df.$Temps_peche_estime.cat2h) ; table(df.$a, df.$Temps_peche_estime.cat2h)  

df.time <- survey[, c("fiche_n","we", "sem")]
summary(df.time[,-1])

replace_values <- function(column_name, data) {
  if (column_name == "we") {
    data[data == 1] <- "weekend"
  } else if (column_name == "sem") {
    data[data == 1] <- "semaine"
  } 
  return(data)
}

cols_to_replace <- c("we", "sem")
df.time[cols_to_replace] <- lapply(cols_to_replace, function(col) replace_values(col, df.time[[col]]))
#write.csv2(df.spatial, "df.spatial.csv") #for ChatGPT

concatenate_unique <- function(row) {
  values <- as.character(row)
  values <- values[values != "0"]
  unique_values <- unique(values)
  paste(unique_values, collapse = ", ")
}

df.time <- df.time %>%
  rowwise() %>%
  mutate(concatenated_values = concatenate_unique(c_across(2:3)))
df.time <- data.frame(df.time)
for (i in 1:nrow(df.time)) {
  df.time[i,"concatenated_values"] <- ifelse(df.time[i,"we"]=="0" & df.time[i,"sem"]=="0" & df.time[i,"concatenated_values"]=="", "0", df.time[i,"concatenated_values"]) 
}
df.time[,"concatenated_values"]  <- ifelse(df.time[,"concatenated_values"] == "NA", NA, df.time[,"concatenated_values"] )
#df.time <- data.frame(df.time)
names(df.time)[names(df.time) == "concatenated_values"] <- "time.week"
table(df.time[,2:4])

df. <- bind_cols(df., df.time[,c("fiche_n", "time.week")])
df. <- subset(df., select = -c(fiche_n...50))
names(df.)[names(df.) == "fiche_n...1"] <- "fiche_n"
rm(df.time,i)

unique(df.$time.week); length(na.omit(df.$time.week)) ; table(df.$time.week) ; table(df.$a, df.$time.week)
# either some fishers do not fish neither the week, nor the weekend, either the 0 are in fact NAs !? consider them like NAs, otherwise a third category "03 that makes no sense. But then almost no observations, so removed from the analysis.
df.$time.week <- ifelse(df.$time.week == "0", NA, df.$time.week)
unique(df.$time.week); length(na.omit(df.$time.week)) ; table(df.$time.week) ; table(df.$a, df.$time.week)

df.time <- survey[, c("fiche_n","aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")]
summary(df.time[,-1])

replace_values <- function(column_name, data) {
  if (column_name == "aube") {
    data[data == 1] <- "aube"
  } else if (column_name == "matin") {
    data[data == 1] <- "matin"
  }  else if (column_name == "soir") {
    data[data == 1] <- "soir"
  }  else if (column_name == "crepuscule") {
    data[data == 1] <- "crepuscule"
  }  else if (column_name == "nuit") {
    data[data == 1] <- "nuit"
  }  else if (column_name == "apm") {
    data[data == 1] <- "apm"
  }  else if (column_name == "journee") {
    data[data == 1] <- "journee"
  }  else if (column_name == "nimp_q") {
    data[data == 1] <- "nimp_q"
  } 
  return(data)
}

cols_to_replace <- c("aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")
df.time[cols_to_replace] <- lapply(cols_to_replace, function(col) replace_values(col, df.time[[col]]))
#write.csv2(df.spatial, "df.spatial.csv") #for ChatGPT

concatenate_unique <- function(row) {
  values <- as.character(row)
  values <- values[values != "0"]
  unique_values <- unique(values)
  paste(unique_values, collapse = ", ")
}

df.time <- df.time %>%
  rowwise() %>%
  mutate(concatenated_values = concatenate_unique(c_across(2:9)))
df.time <- data.frame(df.time)
for (i in 1:nrow(df.time)) {
  df.time[i,"concatenated_values"] <- ifelse(
    df.time[i,"aube"]=="0" 
    & df.time[i,"matin"]=="0" 
    & df.time[i,"soir"]=="0" 
    & df.time[i,"crepuscule"]=="0" 
    & df.time[i,"nuit"]=="0" 
    & df.time[i,"apm"]=="0" 
    & df.time[i,"journee"]=="0" 
    & df.time[i,"nimp_q"]=="0"
    & df.time[i,"concatenated_values"]==""
    , "0", df.time[i,"concatenated_values"]) 
}
df.time[,"concatenated_values"]  <- ifelse(df.time[,"concatenated_values"] == "NA", NA, df.time[,"concatenated_values"] )
#df.time <- data.frame(df.time)
names(df.time)[names(df.time) == "concatenated_values"] <- "time.day"
table(df.time[,2:10])

df. <- bind_cols(df., df.time[,c("fiche_n", "time.day")])
df. <- subset(df., select = -c(fiche_n...51))
names(df.)[names(df.) == "fiche_n...1"] <- "fiche_n"
rm(df.time,i)

unique(df.$time.day); length(na.omit(df.$time.day)) ; table(df.$time.day) ; table(df.$a, df.$time.day)

df.$time.day.gp <- df.$time.day
df.$time.day.gp <- ifelse(df.$time.day.gp %in% c("aube", "matin"), "am", df.$time.day.gp)
df.$time.day.gp <- ifelse(df.$time.day.gp %in% c("apm", "crepuscule", "soir"), "pm", df.$time.day.gp)
df.$time.day.gp <-  ifelse(df.$time.day.gp %notin% c("0", "NA",
                                                     "am", "pm", "nuit", "journee", "nimp_q") 
                           & !is.na(df.$time.day.gp), "nimp_q", df.$time.day.gp)

unique(df.$time.day.gp); length(na.omit(df.$time.day.gp)) ; table(df.$time.day.gp) ; table(df.$a, df.$time.day.gp)
# remove the variable level "0" that makes no sense
df.$time.day.gp <- ifelse(df.$time.day.gp == "0", NA, df.$time.day.gp)
unique(df.$time.day.gp); length(na.omit(df.$time.day.gp)) ; table(df.$time.day.gp) ; table(df.$a, df.$time.day.gp)

df.spatial <- survey[, c("fiche_n","z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61")]
summary(df.spatial[,-1])
table(df.spatial$z_9)
df.spatial$z_9 <- ifelse(df.spatial$z_9 == 9, 1, df.spatial$z_9)
#write.csv2(df.spatial, "df.spatial.csv") #for ChatGPT

replace_values <- function(column_name, data) {
  if (column_name %in% c("z_54", "z_55", "z_56", "z_57", "z_58")) {
    data[data == 1] <- "Agriates"
  } else if (column_name == "z_59") {
    data[data == 1] <- "Saint-Florent"
  } else if (column_name %in% c("z_60", "z_61", "z_1", "z_2")) {
    data[data == 1] <- "Ouest Cap"
  } else if (column_name %in% c("z_3", "z_4")) {
    data[data == 1] <- "Nord Cap"
  } else if (column_name %in% c("z_6", "z_7", "z_8", "z_9")) {
    data[data == 1] <- "Est Cap"
  }
  return(data)
}

cols_to_replace <- c("z_54", "z_55", "z_56", "z_57", "z_58", "z_59", "z_60", "z_61", "z_1", "z_2", "z_3", "z_4", "z_6", "z_7", "z_8", "z_9")
df.spatial[cols_to_replace] <- lapply(cols_to_replace, function(col) replace_values(col, df.spatial[[col]]))
#write.csv2(df.spatial, "df.spatial.csv") #for ChatGPT

concatenate_unique <- function(row) {
  values <- as.character(row)
  values <- values[values != "0"]
  unique_values <- unique(values)
  paste(unique_values, collapse = ", ")
}

df.spatial <- df.spatial %>%
  rowwise() %>%
  mutate(concatenated_values = concatenate_unique(c_across(2:17)))
df.spatial <- data.frame(df.spatial)
names(df.spatial)[names(df.spatial) == "concatenated_values"] <- "zone"

df. <- bind_cols(df., df.spatial[,c("fiche_n", "zone")])
df. <- subset(df., select = -c(fiche_n...53))
names(df.)[names(df.) == "fiche_n...1"] <- "fiche_n"
rm(df.spatial)

unique(df.$zone); length(na.omit(df.$zone)) ; table(df.$zone) ; table(df.$a, df.$zone)

df.$zone.gp <- df.$zone
df.$zone.gp <-  ifelse(df.$zone.gp %notin% c("0", "NA",
                                                     "Est Cap", "Ouest Cap", "Nord Cap", "Saint-Florent", "Agriates") 
                           & !is.na(df.$zone.gp), "nimp_ou", df.$zone.gp)

unique(df.$zone.gp); length(na.omit(df.$zone.gp)) ; table(df.$zone.gp) ; table(df.$a, df.$zone.gp)
df.$zone.gp <-  ifelse(df.$zone.gp == "NA", NA, df.$zone.gp)
unique(df.$zone.gp); length(na.omit(df.$zone.gp)) ; table(df.$zone.gp) ; table(df.$a, df.$zone.gp)

df. <- add_column(df., age_classe = NA, .after = "age_moyen")
df.$age_classe <- as.character(df.$age_classe)
df. <- data.frame(df.)
df.$age_classe <- ifelse(df.$age_moyen >= 70, "70 et plus", df.$age_classe) 
df.$age_classe <- ifelse(df.$age_moyen >= 60 & df.$age_moyen < 70, "60-69", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 50 & df.$age_moyen < 60, "50-59", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 40 & df.$age_moyen < 50, "40-49", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen >= 25 & df.$age_moyen < 40, "25-39", df.$age_classe)
df.$age_classe <- ifelse(df.$age_moyen < 25, "13-24", df.$age_classe)
unique(df.$age_classe)
nrow(df.) ; length(na.omit(df.$age_classe))
unique(df.$age_classe); length(na.omit(df.$age_classe)) ; table(df.$age_classe) ; table(df.$a, df.$age_classe)
#we have no info about age_moyen in 2023 & 2024 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis

df. %>% filter(mod_peche != "pdb, csm") -> df.
table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée", "peche à l'oursin"))
table(df.$mod_peche) 
nrow(df.) ; length(na.omit(df.$mod_peche))
unique(df.$mod_peche); length(na.omit(df.$mod_peche)) ; table(df.$mod_peche) ; table(df.$a, df.$mod_peche)

unique(df.$cat_pro)
df.$cat_pro <- ifelse(df.$cat_pro == "NA", NA, df.$cat_pro)
table(df.$cat_pro)
nrow(df.) ; length(na.omit(df.$cat_pro))
table(df.[, c("a", "cat_pro")])
df.$cat_pro.gp <- df.$cat_pro
df.$cat_pro.gp <- ifelse(df.$cat_pro.gp %in% c("sans_emploi", "en_arret"), "sans_emploi-en_arret", df.$cat_pro.gp)
table(df.$cat_pro.gp)
unique(df.$cat_pro.gp); length(na.omit(df.$cat_pro.gp)) ; table(df.$cat_pro.gp) ; table(df.$a, df.$cat_pro.gp)

table(survey[, c("a", "act_pro")])
unique(survey$a)
unique(filter(survey[,c("a", "act_pro")], "a" %in% c("2023", "2024")))
#we have no info about cat_pro in 2023 & 2024 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

unique(df.$avis_pnm)
df. <- add_column(df., classe_avis_pnm = df.$avis_pnm, .after = "avis_pnm")
unique(df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "NA", NA, df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "_", NA, df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "t_positif", "positif", df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "t_negatif", "negatif", df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "nsp", "p_avis", df.$classe_avis_pnm)
table(df.$classe_avis_pnm)
unique(df.$classe_avis_pnm); length(na.omit(df.$classe_avis_pnm)) ; table(df.$classe_avis_pnm) ; table(df.$a, df.$classe_avis_pnm)

unique(df.$nb_sort_an)
df.$nb_sort_an <- ifelse(df.$nb_sort_an == "NA", NA, df.$nb_sort_an)
df.$nb_sort_an <- ifelse(df.$nb_sort_an == "illimite", 999, df.$nb_sort_an)
table(df.$nb_sort_an) #or considering that variable as categorial, cfr idem classe_age? 
nrow(df.) ; length(na.omit(df.$nb_sort_an))
unique(filter(survey[,c("a", "nb_sort_an")], "a" %in% c("2023", "2024")))
#we have no info about nb_sort_an in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

unique(df.$nb_sort_an); length(na.omit(df.$nb_sort_an)) ; table(df.$nb_sort_an) ; table(df.$a, df.$nb_sort_an) #no data for 2023 & 2024
df.$nb_sort_an <- as.numeric(df.$nb_sort_an)
df.$nb_sort_an_cat <- cut(df.$nb_sort_an, breaks=quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T), include.lowest=TRUE, labels=c(
  paste0("Q1 (<=", 
         #quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[1]], "-",
         quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[2]], 
         ")") , 
  paste0("Q2 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[2]]+1, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[3]], ")") , 
  paste0("Q3 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[3]]+1, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[4]], ")") , 
  paste0("Q4 (>=", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[4]]+1
         #, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[5]]
         ,")") ))
unique(df.$nb_sort_an_cat); length(na.omit(df.$nb_sort_an_cat)) ; table(df.$nb_sort_an_cat) ; table(df.$a, df.$nb_sort_an_cat) #no data for 2023 & 2024

# dépenses
# 1= "< 100 euros"; 2="entre 100-500 euros"; 3= "entre 500-1000 euros "; 4= "entre 1000-2000 euros ";5 ="supérieur à 2000"
table(df.$dep_pdb)
table(df.$dep_pe)
table(df.$dep_csm)
table(df.$dep_po)
df.[, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")] <- lapply(df.[, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")], as.numeric)

df.$dep <- NA
df.$dep.median <- NA
df.$dep.median.rd <- NA
df.$dep_n <- NA
for (i in 1:nrow(df.)) {
  #i=1
  dep. <- as.vector(unlist(df.[i, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")]))
  sort(dep.[na.omit(dep.)!=0])
  df.[i,"dep"] <- paste0(sort(dep.[na.omit(dep.)!=0]), collapse=",")
  df.[i,"dep.median"] <- median(dep.[na.omit(dep.)!=0])
  df.[i,"dep.median.rd"] <- ceiling(df.[i,"dep.median"])
  df.[i,"dep_n"] <- length(na.omit(dep.[dep.!=0]))
}
rm(dep.,i)
table(df.$dep)

arrange(unique(df.[, c("mod_peche", "dep_pdb", "dep_pe", "dep_csm", "dep_po")]), mod_peche)
table(df.$dep.median.rd)

df.$dep.mod_peche <- NA
unique(df.$mod_peche)

for (i in 1:nrow(df.)) {
  if(df.[i,"mod_peche"] == "peche du bord") {
    df.[i,"dep.mod_peche"] <- df.[i,"dep_pdb"]
  } else if (df.[i,"mod_peche"] == "peche embarquée") {
    df.[i,"dep.mod_peche"] <- df.[i,"dep_pe"]
  } else if (df.[i,"mod_peche"] == "chasse sous-marine") {
    df.[i,"dep.mod_peche"] <- df.[i,"dep_csm"]
  } else if (df.[i,"mod_peche"] == "peche à l'oursin") {
    df.[i,"dep.mod_peche"] <- df.[i,"dep_po"]
  } 
}

df.$dep.mod_peche <- ifelse(df.$dep.mod_peche == 0, NA, df.$dep.mod_peche)
unique(df.$dep.mod_peche); length(na.omit(df.$dep.mod_peche)) ; table(df.$dep.mod_peche) ; table(df.$a, df.$dep.mod_peche) #no data for 2023

# 1= "< 100 euros"; 2="entre 100-500 euros"; 3= "entre 500-1000 euros"; 4= "entre 1000-2000 euros";5 ="supérieur à 2000"

df.$dep.mod_peche.chr <- as.character(df.$dep.mod_peche)
df.$dep.mod_peche.chr <- ifelse(is.na(df.$dep.mod_peche.chr), "NA", df.$dep.mod_peche.chr)
for (i in 1:nrow(df.)) {
  if (df.[i,"dep.mod_peche.chr"] == "1") {
    df.[i,"dep.mod_peche.chr"] <- "< 100 euros"
  } else if (df.[i,"dep.mod_peche.chr"] == "2") {
    df.[i,"dep.mod_peche.chr"] <- "entre 100-500 euros"
  } else if (df.[i,"dep.mod_peche.chr"] == "3") {
    df.[i,"dep.mod_peche.chr"] <- "entre 500-1000 euros"
  } else if (df.[i,"dep.mod_peche.chr"] == "4") {
    df.[i,"dep.mod_peche.chr"] <- "entre 1000-2000 euros"
  } else if (df.[i,"dep.mod_peche.chr"] == "5") {
    df.[i,"dep.mod_peche.chr"] <- "supérieur à 2000"
  } else {
    df.[i,"dep.mod_peche.chr"] <- NA
  }
}

df.$dep.median.rd.chr <- as.character(df.$dep.median.rd)
df.$dep.median.rd.chr <- ifelse(is.na(df.$dep.median.rd.chr), "NA", df.$dep.median.rd.chr)
for (i in 1:nrow(df.)) {
  if (df.[i,"dep.median.rd.chr"] == "1") {
    df.[i,"dep.median.rd.chr"] <- "< 100 euros"
  } else if (df.[i,"dep.median.rd.chr"] == "2") {
    df.[i,"dep.median.rd.chr"] <- "entre 100-500 euros"
  } else if (df.[i,"dep.median.rd.chr"] == "3") {
    df.[i,"dep.median.rd.chr"] <- "entre 500-1000 euros"
  } else if (df.[i,"dep.median.rd.chr"] == "4") {
    df.[i,"dep.median.rd.chr"] <- "entre 1000-2000 euros"
  } else if (df.[i,"dep.median.rd.chr"] == "5") {
    df.[i,"dep.median.rd.chr"] <- "supérieur à 2000"
  } else {
    df.[i,"dep.median.rd.chr"] <- NA
  }
}
rm(i)

unique(df.$res_tour)
df.$res_tour <- ifelse(df.$res_tour == "occasionnel", "touriste occasionnel", df.$res_tour)
df.$res_tour <- ifelse(df.$res_tour == "touriste", "touriste occasionnel", df.$res_tour)
df.$res_tour <- ifelse(df.$res_tour == "secondaire", "maison secondaire", df.$res_tour)
unique(df.$res_tour)

df.$a <- as.factor(df.$a)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$age_classe <- as.factor(df.$age_classe)
df.$cat_pro.gp <- as.factor(df.$cat_pro.gp)
df.$nb_sort_an <- as.numeric(df.$nb_sort_an)
df.$classe_avis_pnm <- as.factor(df.$classe_avis_pnm)
df.$sexe <- as.factor(df.$sexe)
df.$res_tour <- as.factor(df.$res_tour)
df.$espcib1 <- as.factor(df.$espcib1)
df.$espcib2 <- as.factor(df.$espcib2)
df.$espcib3 <- as.factor(df.$espcib3)
df.$poss_bat <- as.factor(df.$poss_bat)
df.$Temps_peche_estime.cat2h <- as.factor(df.$Temps_peche_estime.cat2h)
df.$Temps_peche_effectif.cat2h <- as.factor(df.$Temps_peche_effectif.cat2h)
df.$time.day.gp <- as.factor(df.$time.day.gp)
df.$zone.gp <- as.factor(df.$zone.gp)
df.$nb_sort_an_cat <- as.factor(df.$nb_sort_an_cat)
df.$dep.mod_peche.chr <- as.factor(df.$dep.mod_peche.chr)
df.$dep.median.rd.chr <- as.factor(df.$dep.median.rd.chr)

nrow(filter(df., a %in% c("2020", "2021", "2022")))
nrow(filter(df., a %in% c("2023", "2024")))


# reduce df. for multivariate spatial analysis ####

df.msa <- df.[, c("a", "mod_peche","age_classe","cat_pro.gp"
                   , "classe_avis_pnm"
                   , "sexe"
                   , "res_tour"
                   , "espcib1", "espcib2", "espcib3"
                   , "poss_bat"
                   , "Temps_peche_estime.cat2h"
                   , "Temps_peche_effectif.cat2h"
                   , "time.day.gp"
                   , "zone.gp"
                   , "nb_sort_an_cat"
                   , "dep.mod_peche.chr"
                   , "dep.median.rd.chr"
                   )]

length(na.omit((filter(df.msa, as.character(df.msa$a) != "NA"))[,"a"]))
length(na.omit((filter(df.msa, as.character(df.msa$mod_peche) != "NA"))[,"mod_peche"]))
length(na.omit((filter(df.msa, as.character(df.msa$age_classe) != "NA"))[,"age_classe"]))
length(na.omit((filter(df.msa, as.character(df.msa$cat_pro.gp) != "NA"))[,"cat_pro.gp"]))
length(na.omit((filter(df.msa, as.character(df.msa$nb_sort_an_cat) != "NA"))[,"nb_sort_an_cat"]))
length(na.omit((filter(df.msa, as.character(df.msa$classe_avis_pnm) != "NA"))[,"classe_avis_pnm"]))
length(na.omit((filter(df.msa, as.character(df.msa$sexe) != "NA"))[,"sexe"]))
length(na.omit((filter(df.msa, as.character(df.msa$res_tour) != "NA"))[,"res_tour"]))
length(na.omit((filter(df.msa, as.character(df.msa$espcib1) != "NA"))[,"espcib1"]))
length(na.omit((filter(df.msa, as.character(df.msa$espcib2) != "NA"))[,"espcib2"]))
length(na.omit((filter(df.msa, as.character(df.msa$espcib3) != "NA"))[,"espcib3"]))
length(na.omit((filter(df.msa, as.character(df.msa$poss_bat) != "NA"))[,"poss_bat"]))
length(na.omit((filter(df.msa, as.character(df.msa$Temps_peche_estime.cat2h) != "NA"))[,"Temps_peche_estime.cat2h"]))
length(na.omit((filter(df.msa, as.character(df.msa$Temps_peche_effectif.cat2h) != "NA"))[,"Temps_peche_effectif.cat2h"]))
length(na.omit((filter(df.msa, as.character(df.msa$time.day.gp) != "NA"))[,"time.day.gp"]))
length(na.omit((filter(df.msa, as.character(df.msa$zone.gp) != "NA"))[,"zone.gp"]))
length(na.omit((filter(df.msa, as.character(df.msa$nb_sort_an_cat) != "NA"))[,"nb_sort_an_cat"]))
length(na.omit((filter(df.msa, as.character(df.msa$dep.median.rd.chr) != "NA"))[,"dep.median.rd.chr"]))
length(na.omit((filter(df.msa, as.character(df.msa$dep.mod_peche.chr) != "NA"))[,"dep.mod_peche.chr"]))

#There are variables with n ~ 517, when surveyed in 2023 & 2024, and variables with n ~ 260, when only surveyed in 2020-2022, but not 2023-2024. So we cannot do the full multivariate spatial analysis with all the variables for 2023-2024 ; and for espece cible, even less data ...


# plot FAMD ####

#La MFA a été utilisée pour discriminer les modes de peche (bateau, du bord, chasse sous-marine, oursin) en fonction de 5 variables quantitatives et qualitatives (âge, profil, activité, nombre de sortie, avis sur le PNMCCA).
#I am not sure about the variables, groups of variables Michel used in his analysis ...
#obviously, except for "profil", one variable per group ... Special case : if each group has just one variable =) Factor Analysis of Mixed Data (FAMD)
#MFA: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/
#FAMD: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

# #2020-2023
# df.FAMD.red <- df.msa[, c("a",
#                           "mod_peche",
#                           "sexe",
#                           "res_tour",
#                           "Temps_peche_estime.cat2h",
#                           "zone.gp")
# ]
# 
# #2020-2022
# df.FAMD <- df.msa[, c(
#   #"a",
#   "mod_peche",
#   "sexe",
#   "res_tour",
#   "Temps_peche_estime.cat2h",
#   "zone.gp",
#   "age_classe",
#   "cat_pro.gp",
#   "nb_sort_an_cat",
#   "classe_avis_pnm",
#   "poss_bat",
#   "time.day.gp",
#   "nb_sort_an_cat",
#   "dep.median.rd.chr",
#   "dep.mod_peche.chr")
# ]

#write.csv2(na.omit(df.FAMD), "df.FAMD.csv") ##for ChatGPT
# Actually, because we now only have categorical variables, it is not an FAMD anymore but a Multiple correspondence analysis (MCA) (equivalent to ACP but for a df with categorical variables only)

#res.famd <- FAMD(na.omit(df.FAMD))
#print(res.famd)
#(eig.val <- get_eigenvalue(res.famd))
#fviz_screeplot(res.famd)
#(var <- get_famd_var(res.famd))
#var$coord # Coordinates of variables
# var$cos2 # Cos2: quality of representation on the factor map
# var$contrib # Contributions to the  dimensions
# fviz_famd_var(res.famd, repel = TRUE)  # Plot of variables
# fviz_contrib(res.famd, "var", axes = 1) # Contribution to the first dimension
# fviz_contrib(res.famd, "var", axes = 2 )# Contribution to the second dimension

# quanti.var <- get_famd_var(res.famd, "quanti.var")
# quanti.var
# quanti.var$cos2
# #fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
# quali.var <- get_famd_var(res.famd, "quali.var")
# quali.var
# fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black", label = "none")
# ind <- get_famd_ind(res.famd)
# ind
# fviz_famd_ind(res.famd, col.ind = "cos2", repel = TRUE
#               # ,label = "none"
#               )
# fviz_mfa_ind(res.famd, habillage = "mod_peche", addEllipses = TRUE, ellipse.type = "confidence", label = "none", repel = TRUE # Avoid text overlapping
# )
# 
# filter(df.FAMD, age_classe=="13-24", cat_pro=="etudiant")
# filter(df.FAMD, cat_pro=="etudiant")
# df.FAMD.red <- filter(df.FAMD, cat_pro!="etudiant")
# 
# res.famd <- FAMD(df.FAMD.red, graph = T)
# print(res.famd)
# (eig.val <- get_eigenvalue(res.famd))
# fviz_screeplot(res.famd)
# (var <- get_famd_var(res.famd))
# var$coord # Coordinates of variables
# var$cos2 # Cos2: quality of representation on the factor map
# var$contrib # Contributions to the  dimensions
# fviz_famd_var(res.famd, repel = TRUE)  # Plot of variables
# fviz_contrib(res.famd, "var", axes = 1) # Contribution to the first dimension
# fviz_contrib(res.famd, "var", axes = 2 )# Contribution to the second dimension
# 
# quanti.var <- get_famd_var(res.famd, "quanti.var")
# quanti.var
# quanti.var$cos2
# #fviz_famd_var(res.famd, "quanti.var", repel = TRUE, col.var = "black")
# quali.var <- get_famd_var(res.famd, "quali.var")
# quali.var
# fviz_famd_var(res.famd, "quali.var", repel = TRUE, col.var = "black", label = "none")
# ind <- get_famd_ind(res.famd)
# ind
# fviz_famd_ind(res.famd, col.ind = "cos2", repel = TRUE
#               ,label = "none"
# )
# fviz_mfa_ind(res.famd, habillage = "mod_peche", addEllipses = TRUE, ellipse.type = "confidence", label = "none", repel = TRUE # Avoid text overlapping
# )


# plot MCA ####

#http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/

#In one variable there is a zero that must be removed in the df construction section above
#remove peche a l'oursin; also etudiant category here below before performing the analysis (but keep them for Kiviat or regression etc analysis )

#2020-2022
df.full <- na.omit(filter(df.msa[, c(
  "a",
  "mod_peche",
  "sexe",
  "res_tour",
  "Temps_peche_estime.cat2h",
  "zone.gp",
  "age_classe",
  "cat_pro.gp",
  "nb_sort_an_cat",
  "classe_avis_pnm",
  "poss_bat",
  "time.day.gp",
  #"dep.median.rd.chr",
  "dep.mod_peche.chr")
], a %in% c("2020", "2021", "2022")))
df.full <- filter(df.full, cat_pro.gp != "etudiant")
df.full <- filter(df.full, age_classe != "13-24")
df.full <- filter(df.full, mod_peche != "peche à l'oursin")

#2020-2024
df.red <- na.omit(filter(df.msa[, c("a",
                          "mod_peche",
                          "sexe",
                          "res_tour",
                          "Temps_peche_estime.cat2h",
                          "zone.gp")
], a %in% c("2020", "2021", "2022", "2023", "2024")))
df.red <- filter(df.red, mod_peche != "peche à l'oursin")

df.full <- droplevels(df.full)
summary(df.full)
df.red <- droplevels(df.red)
summary(df.red)
# MCA: performed on one of the above df.

# df.full 2020-2022
unique(df.full$a)
res.mca <- MCA(df.full, ncp = ncol(df.full), graph = TRUE)
#res.mca <- MCA(df.full[, 3:ncol(df.full)], ncp = ncol(df.full[, 3:ncol(df.full)]), graph = TRUE)

# Eigenvalues / Variances = The proportion of variances retained by the different dimensions (axes) 
(eig.val <- get_eigenvalue(res.mca))
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, eig.val[1,3]*1.1))
# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
# Graph of variables
(var <- get_mca_var(res.mca))
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
ggsave("Figs/MCA/variables_2020-2022.png", width = 7, height = 4, bg = "white")
# Coordinates of variable categories
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# Quality of representation of variable categories: the squared cosine (cos2) measures the degree of association between variable categories and a particular axis (quality on the factor map)
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
# Contribution of variable categories to the dimensions
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
# Color the most important (or, contributing) variable categories 
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
# Graph of individuals
(ind <- get_mca_ind(res.mca))
# Plots: quality and contribution
# color individuals by their cos2 values
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
# Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)
# Color individuals by groups
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "mod_peche", # color by groups 
             #palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())
#ggsave("Figs/MCA/factor.map_mod.peche_2020-2022.png", width = 10, height = 5)
# color individuals using multiple categorical variables
fviz_ellipses(res.mca, c("a", "mod_peche"),
              geom = "point",
              )
#ggsave("Figs/MCA/factor.map_a_mod.peche_2020-2022.png", width = 10, height = 5)
# Dimension description: to identify the most correlated variables with a given dimension
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

# df.short 2020-2024
unique(df.red$a)
res.mca <- MCA(df.red, ncp = ncol(df.red), graph = TRUE)
#res.mca <- MCA(df.red[, 3:ncol(df.red)], ncp = ncol(df.red[, 3:ncol(df.red)]), graph = TRUE)

# Eigenvalues / Variances = The proportion of variances retained by the different dimensions (axes) 
(eig.val <- get_eigenvalue(res.mca))
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, eig.val[1,3]*1.1))
# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
# Graph of variables
(var <- get_mca_var(res.mca))
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
ggsave("Figs/MCA/variables_2020-2024.png", width = 7, height = 4, bg = "white")
# Coordinates of variable categories
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# Quality of representation of variable categories: the squared cosine (cos2) measures the degree of association between variable categories and a particular axis (quality on the factor map)
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
# Contribution of variable categories to the dimensions
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
# Color the most important (or, contributing) variable categories 
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
# Graph of individuals
(ind <- get_mca_ind(res.mca))
# Plots: quality and contribution
# color individuals by their cos2 values
fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())
# Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)
# Color individuals by groups
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "mod_peche", # color by groups 
             #palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 
#ggsave("Figs/MCA/factor.map_mod.peche_2020-2024.png", width = 10, height = 5)
# color individuals using multiple categorical variables
fviz_ellipses(res.mca, c("a", "mod_peche"),
              geom = "point")
#ggsave("Figs/MCA/factor.map_a_mod.peche_2020-2024.png", width = 10, height = 5)
# Dimension description: to identify the most correlated variables with a given dimension
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]


# modelling ####

# Multinomial Logistic Regression Model

unique(survey$a)
df.mod <- survey[, c(
                  #short model
                  "fiche_n"
                  , "a" 
                  , "mod_peche"
                  , "res_tour"
                  , "z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61"
                  , "Temps_peche_estime"
                  # large model
                  , "age_moyen" 
                  , "cat_pro" 
                  , "nb_sort_an" 
                  , "avis_pnm"
                  , "dep_pdb", "dep_pe", "dep_csm", "dep_po"
                  , "aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q"
                  , "poss_bat"
                  , "Temps_peche_effectif"
                  )]

#df.mod$a <- factor(df.mod$a, levels = c("2020", "2021", "2022", "2023", "2024"))
#df.mod %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.mod

# for model short

unique(df.mod$mod_peche)
table(df.mod$mod_peche)
df.mod$mod_peche <- ifelse(df.mod$mod_peche == "csm", "chasse sous-marine", df.mod$mod_peche)
df.mod$mod_peche <- ifelse(df.mod$mod_peche == "pdb", "peche du bord", df.mod$mod_peche)
df.mod$mod_peche <- ifelse(df.mod$mod_peche == "pe", "peche embarquée", df.mod$mod_peche)
df.mod$mod_peche <- ifelse(df.mod$mod_peche == "po", "peche à l'oursin", df.mod$mod_peche)
table(df.mod$mod_peche)
df.mod$mod_peche <- as.factor(df.mod$mod_peche)
nrow(df.mod) ; length(na.omit(df.mod$mod_peche))
unique(df.mod$mod_peche); length(na.omit(df.mod$mod_peche)) ; table(df.mod$mod_peche) ; table(df.mod$a, df.mod$mod_peche)

table(df.mod$res_tour)
df.mod$res_tour <- ifelse(df.mod$res_tour %in% c("occasionnel", "secondaire", "touriste"), "non-resident", df.mod$res_tour)
table(df.mod$res_tour)
df.mod$res_tour <- as.factor(df.mod$res_tour)

df.spatial <- df.mod[, c("fiche_n","z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61")]
summary(df.spatial[,-1])
table(df.spatial$z_9)
df.spatial$z_9 <- ifelse(df.spatial$z_9 == 9, 1, df.spatial$z_9)

replace_values <- function(column_name, data) {
  if (column_name %in% c("z_54", "z_55", "z_56", "z_57", "z_58", "z_59")) {
    data[data == 1] <- "Agriates_St-Florent"
  } else if (column_name %in% c("z_60", "z_61", "z_1", "z_2", "z_3")) {
    data[data == 1] <- "Ouest-Nord Cap"
  } else if (column_name %in% c("z_6", "z_7", "z_8", "z_9", "z_4")) {
    data[data == 1] <- "Est-Nord Cap"
  }
  return(data)
}

cols_to_replace <- c("z_54", "z_55", "z_56", "z_57", "z_58", "z_59", "z_60", "z_61", "z_1", "z_2", "z_3", "z_4", "z_6", "z_7", "z_8", "z_9")
df.spatial[cols_to_replace] <- lapply(cols_to_replace, function(col) replace_values(col, df.spatial[[col]]))

concatenate_unique <- function(row) {
  values <- as.character(row)
  values <- values[values != "0"]
  unique_values <- unique(values)
  paste(unique_values, collapse = ", ")
}

df.spatial <- df.spatial %>%
  rowwise() %>%
  mutate(concatenated_values = concatenate_unique(c_across(2:17)))
df.spatial <- data.frame(df.spatial)
names(df.spatial)[names(df.spatial) == "concatenated_values"] <- "zone"

df.mod <- bind_cols(df.mod, df.spatial[,c("fiche_n", "zone")])
df.mod <- subset(df.mod, select = -c(fiche_n...40))
names(df.mod)[names(df.mod) == "fiche_n...1"] <- "fiche_n"
rm(df.spatial)
unique(df.mod$zone); length(na.omit(df.mod$zone)) ; table(df.mod$zone) ; table(df.mod$a, df.mod$zone)

df.mod$zone.gp <- df.mod$zone
df.mod$zone.gp <-  ifelse(df.mod$zone.gp %notin% c("0", "NA",
                                             "Est-Nord Cap", "Ouest-Nord Cap", "Agriates_St-Florent") 
                       & !is.na(df.mod$zone.gp), "nimp_ou", df.mod$zone.gp)
unique(df.mod$zone.gp); length(na.omit(df.mod$zone.gp)) ; table(df.mod$zone.gp) ; table(df.mod$a, df.mod$zone.gp)
df.mod$zone.gp <-  ifelse(df.mod$zone.gp == "NA", NA, df.mod$zone.gp)
unique(df.mod$zone.gp); length(na.omit(df.mod$zone.gp)) ; table(df.mod$zone.gp) ; table(df.mod$a, df.mod$zone.gp)
df.mod$zone.gp <- as.factor(df.mod$zone.gp)

df.mod %>%
  mutate(Temps_peche_estime.30min = if_else(Temps_peche_estime < parse_hms("00:30:00"), NA, as_hms(Temps_peche_estime))) -> df.mod

df.mod$Temps_peche_estime.cat2h <- NA
for (i in 1:length(df.mod$Temps_peche_estime.30min)) {
  #i=1
  if (as.numeric(df.mod[i, "Temps_peche_estime.30min"]) < as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"] <- "<2h"
  } else if (as.numeric(df.mod[i, "Temps_peche_estime.30min"]) >= as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"]  <- ">=2h"
  }  else if (is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"]  <- NA
  } 
  rm(i)
}

unique(df.mod$Temps_peche_estime.cat2h); length(na.omit(df.mod$Temps_peche_estime.cat2h)) ; table(df.mod$Temps_peche_estime.cat2h) ; table(df.mod$a, df.mod$Temps_peche_estime.cat2h)  
df.mod$Temps_peche_estime.cat2h <- as.factor(df.mod$Temps_peche_estime.cat2h)

nrow(na.omit((filter(df.mod, as.character(df.mod$a) != "NA"))[,"a"]))
nrow(na.omit((filter(df.mod, as.character(df.mod$mod_peche) != "NA"))[,"mod_peche"]))
nrow(na.omit((filter(df.mod, as.character(df.mod$res_tour) != "NA"))[,"res_tour"]))
nrow(na.omit((filter(df.mod, as.character(df.mod$Temps_peche_estime.cat2h) != "NA"))[,"Temps_peche_estime.cat2h"]))
nrow(na.omit((filter(df.mod, as.character(df.mod$zone.gp) != "NA"))[,"zone.gp"]))

# for model large

unique(df.mod$poss_bat); length(na.omit(df.mod$poss_bat)) ; table(df.mod$poss_bat) ; table(df.mod$a, df.mod$poss_bat) 
df.mod$poss_bat <- ifelse(df.mod$poss_bat == "NA", NA, df.mod$poss_bat)
unique(df.mod$poss_bat); length(na.omit(df.mod$poss_bat)) ; table(df.mod$poss_bat) ; table(df.mod$a, df.mod$poss_bat)  

df.mod %>%
  mutate(Temps_peche_effectif.30min = if_else(Temps_peche_effectif < parse_hms("00:30:00"), NA, as_hms(Temps_peche_effectif))) -> df.mod

df.mod$Temps_peche_effectif.cat2h <- NA
for (i in 1:length(df.mod$Temps_peche_effectif.30min)) {
  #i=1
  if (as.numeric(df.mod[i, "Temps_peche_effectif.30min"]) < as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_effectif.30min"])) {
    df.mod[i, "Temps_peche_effectif.cat2h"] <- "<2h"
  } else if (as.numeric(df.mod[i, "Temps_peche_effectif.30min"]) >= as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_effectif.30min"])) {
    df.mod[i, "Temps_peche_effectif.cat2h"]  <- ">=2h"
  }  else if (is.na(df.mod[i, "Temps_peche_effectif.30min"])) {
    df.mod[i, "Temps_peche_effectif.cat2h"]  <- NA
  } 
  rm(i)
}

df.mod %>%
  mutate(Temps_peche_estime.30min = if_else(Temps_peche_estime < parse_hms("00:30:00"), NA, as_hms(Temps_peche_estime))) -> df.mod

df.mod$Temps_peche_estime.cat2h <- NA
for (i in 1:length(df.mod$Temps_peche_estime.30min)) {
  #i=1
  if (as.numeric(df.mod[i, "Temps_peche_estime.30min"]) < as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"] <- "<2h"
  } else if (as.numeric(df.mod[i, "Temps_peche_estime.30min"]) >= as.numeric(as.hms("02:00:00")) & !is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"]  <- ">=2h"
  }  else if (is.na(df.mod[i, "Temps_peche_estime.30min"])) {
    df.mod[i, "Temps_peche_estime.cat2h"]  <- NA
  } 
  rm(i)
}

unique(df.mod$Temps_peche_effectif.cat2h); length(na.omit(df.mod$Temps_peche_effectif.cat2h)) ; table(df.mod$Temps_peche_effectif.cat2h) ; table(df.mod$a, df.mod$Temps_peche_effectif.cat2h)  
unique(df.mod$Temps_peche_estime.cat2h); length(na.omit(df.mod$Temps_peche_estime.cat2h)) ; table(df.mod$Temps_peche_estime.cat2h) ; table(df.mod$a, df.mod$Temps_peche_estime.cat2h)  

df.time <- survey[, c("fiche_n","aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")]
summary(df.time[,-1])

replace_values <- function(column_name, data) {
  if (column_name == "aube") {
    data[data == 1] <- "aube"
  } else if (column_name == "matin") {
    data[data == 1] <- "matin"
  }  else if (column_name == "soir") {
    data[data == 1] <- "soir"
  }  else if (column_name == "crepuscule") {
    data[data == 1] <- "crepuscule"
  }  else if (column_name == "nuit") {
    data[data == 1] <- "nuit"
  }  else if (column_name == "apm") {
    data[data == 1] <- "apm"
  }  else if (column_name == "journee") {
    data[data == 1] <- "journee"
  }  else if (column_name == "nimp_q") {
    data[data == 1] <- "nimp_q"
  } 
  return(data)
}

cols_to_replace <- c("aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")
df.time[cols_to_replace] <- lapply(cols_to_replace, function(col) replace_values(col, df.time[[col]]))

concatenate_unique <- function(row) {
  values <- as.character(row)
  values <- values[values != "0"]
  unique_values <- unique(values)
  paste(unique_values, collapse = ", ")
}

df.time <- df.time %>%
  rowwise() %>%
  mutate(concatenated_values = concatenate_unique(c_across(2:9)))
df.time <- data.frame(df.time)
for (i in 1:nrow(df.time)) {
  df.time[i,"concatenated_values"] <- ifelse(
    df.time[i,"aube"]=="0" 
    & df.time[i,"matin"]=="0" 
    & df.time[i,"soir"]=="0" 
    & df.time[i,"crepuscule"]=="0" 
    & df.time[i,"nuit"]=="0" 
    & df.time[i,"apm"]=="0" 
    & df.time[i,"journee"]=="0" 
    & df.time[i,"nimp_q"]=="0"
    & df.time[i,"concatenated_values"]==""
    , "0", df.time[i,"concatenated_values"]) 
}
df.time[,"concatenated_values"]  <- ifelse(df.time[,"concatenated_values"] == "NA", NA, df.time[,"concatenated_values"] )
names(df.time)[names(df.time) == "concatenated_values"] <- "time.day"
table(df.time[,2:10])

df.mod <- bind_cols(df.mod, df.time[,c("fiche_n", "time.day")])
df.mod <- subset(df.mod, select = -c(fiche_n...46))
names(df.mod)[names(df.mod) == "fiche_n...1"] <- "fiche_n"
rm(df.time,i)

unique(df.mod$time.day); length(na.omit(df.mod$time.day)) ; table(df.mod$time.day) ; table(df.mod$a, df.mod$time.day)

df.mod$time.day.gp <- df.mod$time.day
df.mod$time.day.gp <- ifelse(df.mod$time.day.gp %in% c("aube", "matin"), "am", df.mod$time.day.gp)
df.mod$time.day.gp <- ifelse(df.mod$time.day.gp %in% c("apm", "crepuscule", "soir"), "pm", df.mod$time.day.gp)
df.mod$time.day.gp <-  ifelse(df.mod$time.day.gp %notin% c("0", "NA",
                                                     "am", "pm", "nimp_q") 
                           & !is.na(df.mod$time.day.gp), "nimp_q", df.mod$time.day.gp)

unique(df.mod$time.day.gp); length(na.omit(df.mod$time.day.gp)) ; table(df.mod$time.day.gp) ; table(df.mod$a, df.mod$time.day.gp)
# remove the variable level "0" that makes no sense
df.mod$time.day.gp <- ifelse(df.mod$time.day.gp == "0", NA, df.mod$time.day.gp)
unique(df.mod$time.day.gp); length(na.omit(df.mod$time.day.gp)) ; table(df.mod$time.day.gp) ; table(df.mod$a, df.mod$time.day.gp)

df.mod <- add_column(df.mod, age_classe = NA, .after = "age_moyen")
df.mod$age_classe <- as.character(df.mod$age_classe)
df.mod <- data.frame(df.mod)
df.mod$age_classe <- ifelse(df.mod$age_moyen >= 60, "60 et plus", df.mod$age_classe) 
df.mod$age_classe <- ifelse(df.mod$age_moyen >= 40 & df.mod$age_moyen < 60, "40-59", df.mod$age_classe)
df.mod$age_classe <- ifelse(df.mod$age_moyen < 40, "13-40", df.mod$age_classe)
unique(df.mod$age_classe)
nrow(df.mod) ; length(na.omit(df.mod$age_classe))
unique(df.mod$age_classe); length(na.omit(df.mod$age_classe)) ; table(df.mod$age_classe) ; table(df.mod$a, df.mod$age_classe)

unique(df.mod$cat_pro)
df.mod$cat_pro <- ifelse(df.mod$cat_pro == "NA", NA, df.mod$cat_pro)
table(df.mod$cat_pro)
nrow(df.mod) ; length(na.omit(df.mod$cat_pro))
table(df.mod[, c("a", "cat_pro")])
df.mod$cat_pro.gp <- df.mod$cat_pro
df.mod$cat_pro.gp <- ifelse(df.mod$cat_pro.gp %in% c("sans_emploi", "en_arret", "etudiant"), "sans_emploi-en_arret-etudiant", df.mod$cat_pro.gp)
table(df.mod$cat_pro.gp)
unique(df.mod$cat_pro.gp); length(na.omit(df.mod$cat_pro.gp)) ; table(df.mod$cat_pro.gp) ; table(df.mod$a, df.mod$cat_pro.gp)

unique(df.mod$avis_pnm)
df.mod <- add_column(df.mod, classe_avis_pnm = df.mod$avis_pnm, .after = "avis_pnm")
unique(df.mod$classe_avis_pnm)
df.mod$classe_avis_pnm <- ifelse(df.mod$classe_avis_pnm == "NA", NA, df.mod$classe_avis_pnm)
df.mod$classe_avis_pnm <- ifelse(df.mod$classe_avis_pnm == "_", NA, df.mod$classe_avis_pnm)
df.mod$classe_avis_pnm <- ifelse(df.mod$classe_avis_pnm == "t_positif", "positif", df.mod$classe_avis_pnm)
df.mod$classe_avis_pnm <- ifelse(df.mod$classe_avis_pnm == "t_negatif", "negatif", df.mod$classe_avis_pnm)
df.mod$classe_avis_pnm <- ifelse(df.mod$classe_avis_pnm == "nsp", "p_avis", df.mod$classe_avis_pnm)
table(df.mod$classe_avis_pnm)
unique(df.mod$classe_avis_pnm); length(na.omit(df.mod$classe_avis_pnm)) ; table(df.mod$classe_avis_pnm) ; table(df.mod$a, df.mod$classe_avis_pnm)

unique(df.mod$nb_sort_an)
df.mod$nb_sort_an <- ifelse(df.mod$nb_sort_an == "NA", NA, df.mod$nb_sort_an)
df.mod$nb_sort_an <- ifelse(df.mod$nb_sort_an == "illimite", 999, df.mod$nb_sort_an)
table(df.mod$nb_sort_an) #or considering that variable as categorial, cfr idem classe_age? 
nrow(df.mod) ; length(na.omit(df.mod$nb_sort_an))

unique(df.mod$nb_sort_an); length(na.omit(df.mod$nb_sort_an)) ; table(df.mod$nb_sort_an) ; table(df.mod$a, df.mod$nb_sort_an) #no data for 2023 & 2024
df.mod$nb_sort_an <- as.numeric(df.mod$nb_sort_an)
df.mod$nb_sort_an_cat <- cut(df.mod$nb_sort_an, breaks=quantile(df.mod$nb_sort_an, probs=0:3/3, na.rm = T), include.lowest=TRUE, labels=c(
  paste0("Q1 (<=", 
         #quantile(df.mod$nb_sort_an, probs=0:4/4, na.rm = T)[[1]], "-",
         quantile(df.mod$nb_sort_an, probs=0:3/3, na.rm = T)[[2]], 
         ")"), 
  paste0("Q2 (", quantile(df.mod$nb_sort_an, probs=0:3/3, na.rm = T)[[2]]+1, "-", quantile(df.mod$nb_sort_an, probs=0:3/3, na.rm = T)[[3]], ")"), 
  paste0("Q3 (>=", quantile(df.mod$nb_sort_an, probs=0:3/3, na.rm = T)[[3]]+1
         ,")") ))
unique(df.mod$nb_sort_an_cat); length(na.omit(df.mod$nb_sort_an_cat)) ; table(df.mod$nb_sort_an_cat) ; table(df.mod$a, df.mod$nb_sort_an_cat) 

# dépenses
# 1= "< 100 euros"; 2="entre 100-500 euros"; 3= "entre 500-1000 euros "; 4= "entre 1000-2000 euros ";5 ="supérieur à 2000"
table(df.mod$dep_pdb)
table(df.mod$dep_pe)
table(df.mod$dep_csm)
table(df.mod$dep_po)
df.mod[, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")] <- lapply(df.mod[, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")], as.numeric)

df.mod$dep <- NA
df.mod$dep.median <- NA
df.mod$dep.median.rd <- NA
df.mod$dep_n <- NA
for (i in 1:nrow(df.mod)) {
  #i=1
  dep. <- as.vector(unlist(df.mod[i, c("dep_pdb", "dep_pe", "dep_csm", "dep_po")]))
  sort(dep.[na.omit(dep.)!=0])
  df.mod[i,"dep"] <- paste0(sort(dep.[na.omit(dep.)!=0]), collapse=",")
  df.mod[i,"dep.median"] <- median(dep.[na.omit(dep.)!=0])
  df.mod[i,"dep.median.rd"] <- ceiling(df.mod[i,"dep.median"])
  df.mod[i,"dep_n"] <- length(na.omit(dep.[dep.!=0]))
}
rm(dep.,i)
table(df.mod$dep)

arrange(unique(df.mod[, c("mod_peche", "dep_pdb", "dep_pe", "dep_csm", "dep_po")]), mod_peche)
table(df.mod$dep.median.rd)

df.mod$dep.mod_peche <- NA
unique(df.mod$mod_peche)
table(df.mod$mod_peche)

#for (i in 1:nrow(df.mod)) {
#  if(df.mod[i,"mod_peche"] == "peche du bord") {
#    df.mod[i,"dep.mod_peche"] <- df.mod[i,"dep_pdb"]
#  } else if (df.mod[i,"mod_peche"] == "peche embarquée") {
#    df.mod[i,"dep.mod_peche"] <- df.mod[i,"dep_pe"]
#  } else if (df.mod[i,"mod_peche"] == "chasse sous-marine") {
#    df.mod[i,"dep.mod_peche"] <- df.mod[i,"dep_csm"]
#  } else if (df.mod[i,"mod_peche"] == "peche à l'oursin") {
#    df.mod[i,"dep.mod_peche"] <- df.mod[i,"dep_po"]
#  } else {
#    df.mod[i,"dep.mod_peche"] <- NA
#  } 
#}

for (i in 1:nrow(df.mod)) {
  df.mod[i,"dep.mod_peche"] <- ifelse(df.mod[i,"mod_peche"] == "peche du bord", df.mod[i,"dep_pdb"], df.mod[i,"dep.mod_peche"])
  df.mod[i,"dep.mod_peche"] <- ifelse(df.mod[i,"mod_peche"] == "peche embarquée", df.mod[i,"dep_pe"], df.mod[i,"dep.mod_peche"])
  df.mod[i,"dep.mod_peche"] <- ifelse(df.mod[i,"mod_peche"] == "chasse sous-marine", df.mod[i,"dep_csm"], df.mod[i,"dep.mod_peche"])
  df.mod[i,"dep.mod_peche"] <- ifelse(df.mod[i,"mod_peche"] == "peche à l'oursin", df.mod[i,"dep_po"], df.mod[i,"dep.mod_peche"])
  df.mod[i,"dep.mod_peche"] <- ifelse(df.mod[i,"mod_peche"] %notin% c("peche du bord", "peche embarquée", "chasse sous-marine", "peche à l'oursin"), NA, df.mod[i,"dep.mod_peche"])
}

df.mod[,c("mod_peche","dep_pdb", "dep_pe", "dep_csm", "dep_po", "dep.mod_peche")]

df.mod$dep.mod_peche <- ifelse(df.mod$dep.mod_peche == 0, NA, df.mod$dep.mod_peche)
unique(df.mod$dep.mod_peche); length(na.omit(df.mod$dep.mod_peche)) ; table(df.mod$dep.mod_peche) ; table(df.mod$a, df.mod$dep.mod_peche) #no data for 2023 & 2024

# 1= "< 100 euros"; 2="entre 100-500 euros"; 3= "entre 500-1000 euros"; 4= "entre 1000-2000 euros";5 ="supérieur à 2000"

df.mod$dep.mod_peche.chr <- as.character(df.mod$dep.mod_peche)
df.mod$dep.mod_peche.chr <- ifelse(is.na(df.mod$dep.mod_peche.chr), "NA", df.mod$dep.mod_peche.chr)
for (i in 1:nrow(df.mod)) {
  if (df.mod[i,"dep.mod_peche.chr"] %in% c("1", "2")) {
    df.mod[i,"dep.mod_peche.chr"] <- "< 100-500 euros"
  } else if (df.mod[i,"dep.mod_peche.chr"] %in% c("3", "4")) {
    df.mod[i,"dep.mod_peche.chr"] <- "entre 500-2000 euros"
  } else if (df.mod[i,"dep.mod_peche.chr"] == "5") {
    df.mod[i,"dep.mod_peche.chr"] <- "> 2000 euros"
  } else {
    df.mod[i,"dep.mod_peche.chr"] <- NA
  }
}

df.mod$dep.median.rd.chr <- as.character(df.mod$dep.median.rd)
df.mod$dep.median.rd.chr <- ifelse(is.na(df.mod$dep.median.rd.chr), "NA", df.mod$dep.median.rd.chr)
for (i in 1:nrow(df.mod)) {
  if (df.mod[i,"dep.median.rd.chr"] %in% c("1", "2")) {
    df.mod[i,"dep.median.rd.chr"] <- "< 100-500 euros"
  } else if (df.mod[i,"dep.median.rd.chr"] %in% c("3", "4")) {
    df.mod[i,"dep.median.rd.chr"] <- "entre 500-2000 euros"
  } else if (df.mod[i,"dep.median.rd.chr"] == "5") {
    df.mod[i,"dep.median.rd.chr"] <- "> 2000 euros"
  } else {
    df.mod[i,"dep.median.rd.chr"] <- NA
  }
}
rm(i)

df.mod$a <- as.factor(df.mod$a)
df.mod$mod_peche <- as.factor(df.mod$mod_peche)
df.mod$age_classe <- as.factor(df.mod$age_classe)
df.mod$cat_pro.gp <- as.factor(df.mod$cat_pro.gp)
df.mod$nb_sort_an <- as.numeric(df.mod$nb_sort_an)
df.mod$classe_avis_pnm <- as.factor(df.mod$classe_avis_pnm)
df.mod$res_tour <- as.factor(df.mod$res_tour)
df.mod$poss_bat <- as.factor(df.mod$poss_bat)
df.mod$Temps_peche_estime.cat2h <- as.factor(df.mod$Temps_peche_estime.cat2h)
df.mod$Temps_peche_effectif.cat2h <- as.factor(df.mod$Temps_peche_effectif.cat2h)
df.mod$time.day.gp <- as.factor(df.mod$time.day.gp)
df.mod$zone.gp <- as.factor(df.mod$zone.gp)
df.mod$nb_sort_an_cat <- as.factor(df.mod$nb_sort_an_cat)
df.mod$dep.mod_peche.chr <- as.factor(df.mod$dep.mod_peche.chr)
df.mod$dep.median.rd.chr <- as.factor(df.mod$dep.median.rd.chr)

df.mod <- df.mod[, c("mod_peche" 
           #short
           ,"a"
           ,"res_tour"
           ,"Temps_peche_estime.cat2h" 
           ,"zone.gp"
           #large
           ,"age_classe"
           ,"cat_pro.gp"
           ,"poss_bat" 
           ,"time.day.gp" 
           ,"nb_sort_an_cat" 
           ,"dep.mod_peche.chr"
           ,"classe_avis_pnm"
           )]

df.mod %>% filter(mod_peche != "pdb, csm") -> df.mod
filter(df.mod, mod_peche != "peche à l'oursin") -> df.mod
# Set the reference group for mod_peche bord to be peche du
df.mod$mod_peche <- relevel(df.mod$mod_peche, ref = "peche du bord")
df.mod <- droplevels(df.mod)

# model short

df.mod.short <- na.omit(df.mod[,c("mod_peche", 
          "a", 
          "res_tour",
          "Temps_peche_estime.cat2h", 
          "zone.gp")])
#saveRDS(df.mod.short, "df.mod.short.rds")

df.mod.short <- droplevels(df.mod.short)
df.mod.short <- data.frame(df.mod.short)

# Function to count observations for each level of a factor variable
count_levels <- function(df, var_name) {
  df %>%
    group_by(across(all_of(var_name))) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(count))
}
# List of categorical variables to summarize
categorical_vars <- c("a", "res_tour", "Temps_peche_estime.cat2h", "zone.gp")
# Summarize counts for each categorical variable
summary_list <- lapply(categorical_vars, function(var) count_levels(df.mod.short, var))
# Display summaries
names(summary_list) <- categorical_vars
summary_list

(count_table <- df.mod.short %>%
  group_by(a, #sexe, 
           res_tour, Temps_peche_estime.cat2h, zone.gp) %>%
  summarise(count = n()) %>%
  ungroup())

multinom_model.short <- multinom(mod_peche ~ ., data = df.mod.short)
summary(multinom_model.short)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.short))
(multinom.tbl.short <- tidy(multinom_model.short, conf.int = TRUE))
(multinom.tbl.short.sign <- filter(multinom.tbl.short, p.value < 0.05))

#Messages d'avis :
#1: Dans sqrt(diag(vc)) : Production de NaN
#2: Dans sqrt(diag(vcov(object))) : Production de NaN
# All of the observations are zero for some combination of factors. Thus any coefficient that involves a comparison with this state will have a parameter value of large magnitude (i.e. abs(beta) >> 1); it should theoretically be infinite, but is usually somewhere between 10 and 30 (depending on where the numerical methods give up). These coefficients will either have ridiculously large standard errors and (Wald) confidence intervals, or (as in your case) NaN values (https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced)

#https://www.youtube.com/@bkrai
#https://www.youtube.com/watch?v=S2rZp4L_nXo
#https://www.youtube.com/watch?v=oxRy2DMrOF4
#https://www.youtube.com/watch?v=11VY8CmNVDQ
#https://www.youtube.com/watch?v=POyTaeneHJY

#2-tailed Z-test (idem result than with above tidy function on model)
z <- summary(multinom_model.short)$coefficient/summary(multinom_model.short)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

multinom_model.short.sign <- multinom(mod_peche ~ . 
                                      #- res_tour #variable now with significant effect when considering 2024 data
                                      , data = df.mod.short)
summary(multinom_model.short.sign)

z <- summary(multinom_model.short.sign)$coefficient/summary(multinom_model.short.sign)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

table(df.mod.short$mod_peche)

# Data partition and modeling
set.seed(222)
ind <- sample(2, nrow(df.mod.short), replace = T, prob = c(.6,.4))
training <- df.mod.short[ind==1,]
testing <- df.mod.short[ind==2,]

table(training$mod_peche)
table(testing$mod_peche)

multinom_model.training <- multinom(mod_peche ~ . 
                                    #- res_tour
                                    , data = training)

# Confusion matrix and misclassification error - training data
(p <- predict(multinom_model.training, training))
(tab <- table(p, training$mod_peche))
1 - sum(diag(tab))/sum(tab) # misclassification
# Confusion matrix and misclassification error - testing data
(p1 <- predict(multinom_model.training, testing))
(tab1 <- table(p1, testing$mod_peche))
1 - sum(diag(tab1))/sum(tab1) # misclassification

tab/colSums(tab)
tab1/colSums(tab1)
# obviously I do not have enough data in chasse sous-marine class !!

# Check assumptions for multinomial logistic regression:

#Assumption 1: Independence of observations.

#Assumption 2: Independence of Irrelevant Alternatives (IIA); i.e. Categories of the outcome variable must be mutually exclusive and exhaustive.
#Use the Hausman-McFadden test to check the IIA assumption. However, this test is not directly available in R, so we'll use an approximation.
# IIA assumption check
iia_test <- function(model, data, formula) {
  reduced_model <- update(model, . ~ . - predictor1) # Example, modify as needed
  model_ll <- logLik(model)
  reduced_model_ll <- logLik(reduced_model)
  chi_square <- 2 * (model_ll - reduced_model_ll)
  p_value <- pchisq(chi_square, df = length(coef(model)) - length(coef(reduced_model)), lower.tail = FALSE)
  return(p_value)
}

iia_result <- iia_test(model = multinom_model.short, data = df.mod.short, mod_peche ~  a + res_tour + Temps_peche_estime.cat2h + zone.gp)
print(iia_result)

#Assumption 3: No Multicollinearity between independent variables.
#Check multicollinearity using the Variance Inflation Factor (VIF).
vif_results <- vif(multinom_model.short)
print(vif_results)
#Warning message
#glm.fit: algorithm did not converge
#This warning often occurs when you attempt to fit a logistic regression model in R and you experience perfect separation – that is, a predictor variable is able to perfectly separate the response variable into 0’s and 1’s. (https://www.statology.org/glm-fit-algorithm-did-not-converge/)
labels = rownames(coefficients(multinom_model.short))
ref = setdiff(multinom_model.short$lab,labels)
t(sapply(labels,function(i){
  dat = df.mod.short
  dat$mod_peche = as.numeric(dat$mod_peche == i)
  vif.i <<- vif(glm(mod_peche ~ .,data=dat,family="binomial"))
}))

#Assumption 4: Linearity in the logit (for continuous variables); i.e.  Linear relationship between continuous variables and the logit transformation of the outcome variable.

#Assumption 5: No outliers or highly influential points.

#Also, absolute minimum of cases or multinomial logistic regression indicates a minimum of 10 cases per independent variable.

# stepwize regression for Multinomial Logistic Regression Model. (http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/)
# the null model contains only the intercept; the full model contains all predictors
(null_model <- multinom(mod_peche ~ 1, data = df.mod.short))
(multinom_model.short)

# use the stepAIC function from the MASS package, which performs both forward and backward stepwise selection based on AIC (Akaike Information Criterion)
stepwise_model.forward <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.short), direction = "both")
stepwise_model.forward$call
stepwise_model.backward <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.short), direction = "backward")
stepwise_model.backward$call
stepwise_model.both <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.short), direction = "both")
stepwise_model.both$call

stepwise_model.forward$call
stepwise_model.backward$call
stepwise_model.both$call

# Custom Stepwise Function for Multinomial Logistic Regression
stepwise_multinom <- function(data, response, predictors) {
  # Helper function to calculate AIC for a given set of predictors
  calculate_aic <- function(predictors) {
    formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
    model <- multinom(formula, data = data)
    return(AIC(model))
  }
  
  best_aic <- Inf
  best_model <- NULL
  current_predictors <- c()
  
  # Forward selection
  for (predictor in predictors) {
    new_predictors <- c(current_predictors, predictor)
    new_aic <- calculate_aic(new_predictors)
    if (new_aic < best_aic) {
      best_aic <- new_aic
      best_model <- new_predictors
    }
  }
  
  # Backward elimination
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    for (predictor in best_model) {
      new_predictors <- setdiff(best_model, predictor)
      if (length(new_predictors) == 0) break
      new_aic <- calculate_aic(new_predictors)
      if (new_aic < best_aic) {
        best_aic <- new_aic
        best_model <- new_predictors
        improved <- TRUE
        break
      }
    }
  }
  
  # Fit the final model with the selected predictors
  final_formula <- as.formula(paste(response, "~", paste(best_model, collapse = "+")))
  final_model <- multinom(final_formula, data = data)
  
  return(list(model = final_model, predictors = best_model))
}

stepwise_multinom(data = df.mod.short, response = "mod_peche", predictors = c("a", "res_tour", "Temps_peche_estime.cat2h", "zone.gp"))

# fit with variables that have a significant effect on the dependant variable mod_peche

multinom_model.short.red <- multinom(mod_peche ~ . 
                                     #- res_tour
                                     , data = df.mod.short)
summary(multinom_model.short.red)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.short.red))
(multinom.tbl.short.red <- tidy(multinom_model.short.red, conf.int = TRUE))
write.csv2(multinom.tbl.short.red, "Tables/Regression/multinom.2020-2024.csv", row.names = F)
(multinom.tbl.short.red.sign <- filter(multinom.tbl.short.red, p.value < 0.05))
write.csv2(multinom.tbl.short.red.sign, "Tables/Regression/multinom_sign.2020-2024.csv", row.names = F)
# watch video https://www.youtube.com/watch?v=oxRy2DMrOF4 on how to write the model to add it in a report
(mod1vs2 <- paste0("ln[P(mod_peche=2)/P(mod_peche=1)] = ", 
                   round((exp(coef(multinom_model.short.red)))[1,1], 2), " + " , 
                   round((exp(coef(multinom_model.short.red)))[1,2], 2),"*",colnames(exp(coef(multinom_model.short.red)))[2], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,3], 2),"*",colnames(exp(coef(multinom_model.short.red)))[3], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,4], 2),"*",colnames(exp(coef(multinom_model.short.red)))[4], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,5], 2),"*",colnames(exp(coef(multinom_model.short.red)))[5], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,6], 2),"*",colnames(exp(coef(multinom_model.short.red)))[6], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,7], 2),"*",colnames(exp(coef(multinom_model.short.red)))[7], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,8], 2),"*",colnames(exp(coef(multinom_model.short.red)))[8], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,9], 2),"*",colnames(exp(coef(multinom_model.short.red)))[9], " + ",
                   round((exp(coef(multinom_model.short.red)))[1,10], 2),"*",colnames(exp(coef(multinom_model.short.red)))[10]
                   ))  
writeLines(mod1vs2, "Tables/Regression/multinom.2020-2024.P1vsP2.txt")
# I can as well write the other equations more properly if needed.
(mod1vs3 <- paste0("ln[P(mod_peche=3)/P(mod_peche=1)] = ", (exp(coef(multinom_model.short.red)))[2,1], " + " , (exp(coef(multinom_model.short.red)))[2,2],"*",colnames(exp(coef(multinom_model.short.red)))[2], " + ", (exp(coef(multinom_model.short.red)))[2,3],"*",colnames(exp(coef(multinom_model.short.red)))[3], " + ..."))
writeLines(mod1vs3, "Tables/Regression/multinom.2020-2024.P1vsP3.txt")

# Fit a decision tree
# http://www.milbo.org/rpart-plot/prp.pdf

tree_model <- rpart(mod_peche ~ . 
                    #- res_tour
                    , data = df.mod.short, method = "class")
rpart.plot(tree_model, type = 4, extra = 2)
#ggsave("Figs/Tree/tree_2020-2024.png", width = 7, height = 4) #doesn't save the plot with ggsave !
# Open a PNG device
#png("Figs/Tree/tree_2020-2024.png", width = 600, height = 350) # bad resolution, save it manually
# Plot the decision tree
#rpart.plot(tree_model, type = 4, extra = 2)
# Close the device
#dev.off()

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red")))

# model large

df.mod.large <- na.omit(df.mod)

df.mod.large <- droplevels(df.mod.large)
df.mod.large <- data.frame(df.mod.large)

# Function to count observations for each level of a factor variable
count_levels <- function(df, var_name) {
  df %>%
    group_by(across(all_of(var_name))) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    arrange(desc(count))
}
# List of categorical variables to summarize
categorical_vars <- c("a", "res_tour", "Temps_peche_estime.cat2h", "zone.gp",
                      "age_classe" ,"cat_pro.gp", "poss_bat", "time.day.gp", "nb_sort_an_cat", "dep.mod_peche.chr", "classe_avis_pnm")

# Summarize counts for each categorical variable
summary_list <- lapply(categorical_vars, function(var) count_levels(df.mod.large, var))
# Display summaries
names(summary_list) <- categorical_vars
summary_list

filter(df.mod.large, classe_avis_pnm != "negatif") -> df.mod.large
df.mod.large <- droplevels(df.mod.large)

(count_table <- df.mod.large %>%
    group_by(a, res_tour, Temps_peche_estime.cat2h, zone.gp,
             age_classe ,cat_pro.gp, poss_bat, time.day.gp, nb_sort_an_cat, dep.mod_peche.chr, classe_avis_pnm) %>%
    summarise(count = n()) %>%
    ungroup())

multinom_model.large <- multinom(mod_peche ~ . 
                                 #-poss_bat
                                 , data = df.mod.large)
summary(multinom_model.large)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.large))
(multinom.tbl.large <- tidy(multinom_model.large, conf.int = TRUE))
(multinom.tbl.large.sign <- filter(multinom.tbl.large, p.value < 0.05))

#Messages d'avis :
#1: Dans sqrt(diag(vc)) : Production de NaN
#2: Dans sqrt(diag(vcov(object))) : Production de NaN
# All of the observations are zero for some combination of factors. Thus any coefficient that involves a comparison with this state will have a parameter value of large magnitude (i.e. abs(beta) >> 1); it should theoretically be infinite, but is usually somewhere between 10 and 30 (depending on where the numerical methods give up). These coefficients will either have ridiculously large standard errors and (Wald) confidence intervals, or (as in your case) NaN values (https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced)

#https://www.youtube.com/@bkrai
#https://www.youtube.com/watch?v=S2rZp4L_nXo
#https://www.youtube.com/watch?v=oxRy2DMrOF4
#https://www.youtube.com/watch?v=11VY8CmNVDQ
#https://www.youtube.com/watch?v=POyTaeneHJY

#2-tailed Z-test (idem result than with above tidy function on model)
z <- summary(multinom_model.large)$coefficient/summary(multinom_model.large)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

multinom_model.large.sign <- multinom(mod_peche ~ . -a -res_tour -cat_pro.gp -time.day.gp
                                      , data = df.mod.large)
summary(multinom_model.large.sign)

z <- summary(multinom_model.large.sign)$coefficient/summary(multinom_model.large.sign)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

table(df.mod.large$mod_peche)

# Data partition and modeling
set.seed(222)
ind <- sample(2, nrow(df.mod.large), replace = T, prob = c(.6,.4))
training <- df.mod.large[ind==1,]
testing <- df.mod.large[ind==2,]

table(training$mod_peche)
table(testing$mod_peche)

multinom_model.training <- multinom(mod_peche ~ . -a -res_tour -cat_pro.gp -time.day.gp
                                    , data = training)

# Confusion matrix and misclassification error - training data
(p <- predict(multinom_model.training, training))
(tab <- table(p, training$mod_peche))
1 - sum(diag(tab))/sum(tab) # misclassification
# Confusion matrix and misclassification error - testing data
(p1 <- predict(multinom_model.training, testing))
(tab1 <- table(p1, testing$mod_peche))
1 - sum(diag(tab1))/sum(tab1) # misclassification

tab/colSums(tab)
tab1/colSums(tab1)
# obviously I now have enough variable to model chasse sous-marine class !!

# Check assumptions for multinomial logistic regression:

#Assumption 1: Independence of observations.

#Assumption 2: Independence of Irrelevant Alternatives (IIA); i.e. Categories of the outcome variable must be mutually exclusive and exhaustive.
#Use the Hausman-McFadden test to check the IIA assumption. However, this test is not directly available in R, so we'll use an approximation.
# IIA assumption check
iia_test <- function(model, data, formula) {
  reduced_model <- update(model, . ~ . - predictor1) # Example, modify as needed
  model_ll <- logLik(model)
  reduced_model_ll <- logLik(reduced_model)
  chi_square <- 2 * (model_ll - reduced_model_ll)
  p_value <- pchisq(chi_square, df = length(coef(model)) - length(coef(reduced_model)), lower.tail = FALSE)
  return(p_value)
}

iia_result <- iia_test(model = multinom_model.large, data = df.mod.large, mod_peche ~  a + res_tour + Temps_peche_estime.cat2h + zone.gp
                       + age_classe + cat_pro.gp + poss_bat + time.day.gp + nb_sort_an_cat + dep.mod_peche.chr + classe_avis_pnm)
print(iia_result)

#Assumption 3: No Multicollinearity between independent variables.
#Check multicollinearity using the Variance Inflation Factor (VIF).
vif_results <- vif(multinom_model.large)
print(vif_results)
#Warning message
#glm.fit: algorithm did not converge
#This warning often occurs when you attempt to fit a logistic regression model in R and you experience perfect separation – that is, a predictor variable is able to perfectly separate the response variable into 0’s and 1’s. (https://www.statology.org/glm-fit-algorithm-did-not-converge/)
labels = rownames(coefficients(multinom_model.large))
ref = setdiff(multinom_model.large$lab,labels)
t(sapply(labels,function(i){
  dat = df.mod.large
  dat$mod_peche = as.numeric(dat$mod_peche == i)
  vif.i <<- vif(glm(mod_peche ~ .,data=dat,family="binomial"))
}))
# check if vif still to high in the reduced model
labels = rownames(coefficients(multinom_model.large.sign))
ref = setdiff(multinom_model.large.sign$lab,labels)
t(sapply(labels,function(i){
  dat = df.mod.large
  dat$mod_peche = as.numeric(dat$mod_peche == i)
  vif.i <<- vif(glm(mod_peche ~ . -a -res_tour -cat_pro.gp -time.day.gp
                    #- poss_bat -res_tour -classe_avis_pnm -Temps_peche_estime.cat2h -cat_pro.gp
                    , data=dat,family="binomial"))
}))

#Assumption 4: Linearity in the logit (for continuous variables); i.e.  Linear relationship between continuous variables and the logit transformation of the outcome variable.

#Assumption 5: No outliers or highly influential points.

#Also, absolute minimum of cases or multinomial logistic regression indicates a minimum of 10 cases per independent variable.

# stepwize regression for Multinomial Logistic Regression Model. (http://www.sthda.com/english/articles/36-classification-methods-essentials/150-stepwise-logistic-regression-essentials-in-r/)
# the null model contains only the intercept; the full model contains all predictors
(null_model <- multinom(mod_peche ~ 1, data = df.mod.large))
(multinom_model.large)

# use the stepAIC function from the MASS package, which performs both forward and backward stepwise selection based on AIC (Akaike Information Criterion)
stepwise_model.forward <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.large), direction = "both")
stepwise_model.forward$call
stepwise_model.backward <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.large), direction = "backward")
stepwise_model.backward$call
stepwise_model.both <- stepAIC(null_model, scope = list(lower = null_model, upper = multinom_model.large), direction = "both")
stepwise_model.both$call

stepwise_model.forward$call
stepwise_model.backward$call
stepwise_model.both$call

# fit with variables that have a significant effect on the dependant variable mod_peche

multinom_model.large.red <- multinom(mod_peche ~ . -a -res_tour -time.day.gp
                                     , data = df.mod.large) # obviously cat_pro.gp still ameliorate the model although not having a significant effect so keep it as well
summary(multinom_model.large.red)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.large.red))
(multinom.tbl.large.red <- tidy(multinom_model.large.red, conf.int = TRUE))
write.csv2(multinom.tbl.large.red, "Tables/Regression/multinom.2020-2022.csv", row.names = F)
(multinom.tbl.large.red.sign <- filter(multinom.tbl.large.red, p.value < 0.05))
write.csv2(multinom.tbl.large.red.sign, "Tables/Regression/multinom_sign.2020-2022.csv", row.names = F)
# watch video https://www.youtube.com/watch?v=oxRy2DMrOF4 on how to write the model to add it in a report
(mod1vs2 <- paste0("ln[P(mod_peche=2)/P(mod_peche=1)] = ", (exp(coef(multinom_model.large.red)))[1,1], " + " , (exp(coef(multinom_model.large.red)))[1,2],"*",colnames(exp(coef(multinom_model.large.red)))[2], " + ", (exp(coef(multinom_model.large.red)))[1,3],"*",colnames(exp(coef(multinom_model.large.red)))[3], " + ..."))
writeLines(mod1vs2, "Tables/Regression/multinom.2020-2022.P1vsP2.txt")
(mod1vs3 <- paste0("ln[P(mod_peche=3)/P(mod_peche=1)] = ", (exp(coef(multinom_model.large.red)))[2,1], " + " , (exp(coef(multinom_model.large.red)))[2,2],"*",colnames(exp(coef(multinom_model.large.red)))[2], " + ", (exp(coef(multinom_model.large.red)))[2,3],"*",colnames(exp(coef(multinom_model.large.red)))[3], " + ..."))
writeLines(mod1vs3, "Tables/Regression/multinom.2020-2022.P1vsP3.txt")

# Fit a decision tree
# http://www.milbo.org/rpart-plot/prp.pdf
tree_model <- rpart(mod_peche ~ . -a -res_tour -time.day.gp
                    #- poss_bat -classe_avis_pnm -Temps_peche_estime.cat2h
                    , data = df.mod.large, method = "class")
rpart.plot(tree_model, type = 4, extra = 2)
# we have to remove poss_bat, cfr of course any fisher with a boat does peche embarquee
tree_model <- rpart(mod_peche ~ . -a -res_tour -time.day.gp
                    - poss_bat 
                    #-res_tour -classe_avis_pnm -Temps_peche_estime.cat2h
                    , data = df.mod.large, method = "class")
rpart.plot(tree_model, type = 4, extra = 2)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red")))


# plot Kiviat diagram ####

df.msa_saved <- df.msa
#df.msa <- df.msa_saved
#names(df.msa) <- c("année", "mode pêche", "classe âge", "catégorie prof.", "avis pnm", "sexe", "résidence", "esp. cible 1", "esp. cible 2", "esp. cible 3", "bateau", "tps. pêche estimée", "tps pêche effectif", "période pêche", "zone pêche", "nbr sorties an", "dépenses mode pêche", "dépenses médiane")

unique(df.msa$a)
unique(df.msa$mod_peche)
unique(df.msa$age_classe)
mapping <- c("13-24" = "13-24 ans", "25-39" = "25-39 ans", "40-49" = "40-49 ans", "50-59" = "50-59 ans", "60-69" = "60-69 ans", "70 et plus" = "70 ans et plus")
df.msa$age_classe <- mapping[df.msa$age_classe]
unique(df.msa$age_classe)
unique(df.msa$cat_pro.gp)
df.msa$cat_pro.gp <- fct_recode(factor(df.msa$cat_pro.gp), 
                        "sans emploi - en arret" = "sans_emploi-en_arret")
unique(df.msa$cat_pro.gp)
unique(df.msa$classe_avis_pnm)
df.msa$classe_avis_pnm <- fct_recode(factor(df.msa$classe_avis_pnm),
                                     "pas d'avis PNM" = "p_avis", "avis positif PNM" = "positif", "avis negatif PNM" = "negatif")
unique(df.msa$classe_avis_pnm)
unique(df.msa$sexe)
unique(df.msa$espcib1) # might be species name issue
unique(df.msa$espcib2) # might be species name issue
unique(df.msa$espcib3) # might be species name issue
unique(df.msa$poss_bat)
df.msa$poss_bat <- fct_recode(factor(df.msa$poss_bat),
                                     "possede bateau" = "oui", "possede pas bateau" = "non")
unique(df.msa$poss_bat)
unique(df.msa$Temps_peche_estime.cat2h)
df.msa$Temps_peche_estime.cat2h <- fct_recode(factor(df.msa$Temps_peche_estime.cat2h),
                               "tps peche estime >=2h" = ">=2h", "tps peche estime <2h" = "<2h")
unique(df.msa$Temps_peche_estime.cat2h)
unique(df.msa$Temps_peche_effectif.cat2h)
df.msa$Temps_peche_effectif.cat2h <- fct_recode(factor(df.msa$Temps_peche_effectif.cat2h),
                                              "tps peche effectif >=2h" = ">=2h", "tps peche effectif <2h" = "<2h")
unique(df.msa$Temps_peche_effectif.cat2h)
unique(df.msa$time.day.gp)
df.msa$time.day.gp <- fct_recode(factor(df.msa$time.day.gp),
                                                "peche matin" = "am", "peche apres-midi" = "pm", "peche tout moment" = "nimp_q", "peche journee" = "journee", "peche nuit" = "nuit")
unique(df.msa$time.day.gp)
unique(df.msa$zone.gp)
df.msa$zone.gp <- fct_recode(factor(df.msa$zone.gp),
                                 "Cap" = "nimp_ou")
unique(df.msa$zone.gp)
unique(df.msa$nb_sort_an_cat)
df.msa$nb_sort_an_cat <- fct_recode(factor(df.msa$nb_sort_an_cat),
                                 "nb sorties / an <=10" = "Q1 (<=10)", "nb sorties / an 11-15" = "Q2 (11-15)", "nb sorties / an 16-30" = "Q3 (16-30)", "nb sorties / an >=31" = "Q4 (>=31)")
unique(df.msa$nb_sort_an_cat)
unique(df.msa$dep.mod_peche.chr)
unique(df.msa$dep.median.rd.chr)

#2020-2022
df.full <- na.omit(filter(df.msa[, c(
  "a",
  "mod_peche",
  "sexe",
  "res_tour",
  "Temps_peche_estime.cat2h",
  "zone.gp",
  "age_classe",
  "cat_pro.gp",
  "nb_sort_an_cat",
  "classe_avis_pnm",
  "poss_bat",
  "time.day.gp",
  #"dep.median.rd.chr",
  "dep.mod_peche.chr")
], a %in% c("2020", "2021", "2022")))
#df.full <- filter(df.full, cat_pro.gp != "etudiant")
#df.full <- filter(df.full, age_classe != "13-24")
#df.full <- filter(df.full, mod_peche != "peche à l'oursin")

#2020-2024
df.red <- na.omit(filter(df.msa[, c("a",
                                    "mod_peche",
                                    "sexe",
                                    "res_tour",
                                    "Temps_peche_estime.cat2h",
                                    "zone.gp")
], a %in% c("2020", "2021", "2022", "2023", "2024")))
#df.red <- filter(df.red, mod_peche != "peche à l'oursin")

# Calculer les pourcentages pour les catégories
df_summary <- as.data.frame(sapply(
  #df.[, c("mod_peche", "nb_sort_an_cat", "classe_avis_pnm", "zone")],
  df.full, function(x) sort(table(x)/length(x)))[1][1])

df.radar <- df.full #df.[, c("mod_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat")]

p.radar <- data.frame(matrix(nrow = 2, ncol = ncol(df.full)))
colnames(p.radar) <-  names(df.full) #c("mod_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat")

for (i in 1:ncol(df.radar)) {
  #i=1
  p.radar[1, names(df.radar)[i]] <- paste0(names(sort(table(na.omit(df.radar[, i]))/length(na.omit(df.radar[, i])), decreasing = T)[1]), " (n=", length(na.omit(df.radar[, i])), ")") 
  p.radar[2, names(df.radar)[i]] <- sort(table(na.omit(df.radar[, i]))/length(na.omit(df.radar[, i])), decreasing = T)[1]
}

names(p.radar) <- p.radar[1,]
p.radar <- p.radar[-1,]
p.radar <- rbind(0,p.radar)
p.radar <- rbind(1,p.radar)
p.radar <- lapply(p.radar,as.numeric) 
names.radar <- names(p.radar)
p.radar <- data.frame(p.radar)
names(p.radar) <- names.radar
#write.csv2(p.radar, "p.radar.csv") #for ChatGPT

radarchart(p.radar
           , axistype=1
           , pcol=rgb(0.2,0.5,0.5,0.9) 
           , pfcol=rgb(0.2,0.5,0.5,0.5) 
           , plwd=4 
           #, cglcol="grey"
           , cglty=1
           , axislabcol="grey"
           , caxislabels= c(0, 20, 50, 75, 100)
           , cglwd=0.8
           , vlcex=1
           , title = "trois mode de peche confondus, 2020-2022"
)

plot.radar <- function(col., col.fill, title.) {
  
  p.radar <- data.frame(matrix(nrow = 2, ncol = ncol(df.plora))) # define df.radar prior ruuning the function
  colnames(p.radar) <- names(df.plora) #c("mode_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat"
  
  for (i in 1:ncol(df.plora)) {
    #i=1
    #df.plora = df.radar.pdb 
    #col. = "#F8766D" 
    #col.fill = "#F8766D50"
    #title. = "peche du bord"
    p.radar[1, names(df.plora)[i]] <- paste0(names(sort(table(na.omit(df.plora[, i]))/length(na.omit(df.plora[, i])), decreasing = T)[1]), " (n=", length(na.omit(df.plora[, i])), ")") 
    p.radar[2, names(df.plora)[i]] <- sort(table(na.omit(df.plora[, i]))/length(na.omit(df.plora[, i])), decreasing = T)[1]
  }
  
  names(p.radar) <- p.radar[1,]
  p.radar <- p.radar[-1,]
  p.radar <- rbind(0,p.radar)
  p.radar <- rbind(1,p.radar)
  p.radar <- lapply(p.radar,as.numeric) 
  names.radar <- names(p.radar)
  p.radar <- data.frame(p.radar)
  names(p.radar) <- names.radar
  #write.csv2(p.radar, "p.radar.csv") #for ChatGPT
  
  p.radar <- within(p.radar, rm("NA"))
  
  radarchart(p.radar
             , axistype=1
             , pcol=col. #rgb(0.2,0.5,0.5,0.9) 
             , pfcol=col.fill #rgb(0.2,0.5,0.5,0.5) 
             , plwd=4 
             #, cglcol="grey"
             , cglty=1
             , axislabcol="grey"
             , caxislabels= c(0, 20, 50, 75, 100)
             , cglwd=0.8
             , vlcex=1
             , title = title.
  )

}

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- df.full
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "trois mode de peche confondus, 2020-2022")

df.plora <- filter(df.red, a %in% c("2023", "2024"))
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "trois mode de peche confondus, 2023-2024")

df.plora <- df.red
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "trois mode de peche confondus, 2020-2024")

#ggsave("Figs/Kiviat/kiviat_mod_peche_a.png", width = 5, height = 15) #ggsave doesn't work, do it manually; 600x1200 ; idem for the next ones

par(op)
par(mfrow = c(1,1))

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- filter(df.full, mod_peche == "peche du bord")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2020-2022")

df.plora <- filter(df.full, mod_peche == "peche embarquée")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2020-2022")

df.plora <- filter(df.full, mod_peche == "chasse sous-marine")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2020-2022")

par(op)
par(mfrow = c(1,1))

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- filter(df.red, mod_peche == "peche du bord")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2020-2024")

df.plora <- filter(df.red, mod_peche == "peche embarquée")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2020-2024")

df.plora <- filter(df.red, mod_peche == "chasse sous-marine")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2020-2024")

par(op)
par(mfrow = c(1,1))

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- filter(df.red, mod_peche == "peche du bord", a %in% c("2023", "2024"))
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2023-2024")

df.plora <- filter(df.red, mod_peche == "peche embarquée", a %in% c("2023", "2024"))
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2023-2024")

df.plora <- filter(df.red, mod_peche == "chasse sous-marine", a %in% c("2023", "2024"))
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2023-2024")

par(op)
par(mfrow = c(1,1))

#2020-2024 although there are NAs in 2023 and 2024 for many variables
df.full <- #na.omit
  (filter(df.msa[, c(
  "a",
  "mod_peche",
  "sexe",
  "res_tour",
  "Temps_peche_estime.cat2h",
  "zone.gp",
  "age_classe",
  "cat_pro.gp",
  "nb_sort_an_cat",
  "classe_avis_pnm",
  "poss_bat",
  "time.day.gp",
  #"dep.median.rd.chr",
  "dep.mod_peche.chr")
], a %in% c("2020", "2021", "2022", "2023", "2024")))
#df.full <- filter(df.full, cat_pro.gp != "etudiant")
#df.full <- filter(df.full, age_classe != "13-24")
#df.full <- filter(df.full, mod_peche != "peche à l'oursin")

# Calculer les pourcentages pour les catégories
df_summary <- as.data.frame(sapply(
  #df.[, c("mod_peche", "nb_sort_an_cat", "classe_avis_pnm", "zone")],
  df.full, function(x) sort(table(x)/length(x)))[1][1])

df.radar <- df.full #df.[, c("mod_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat")]

p.radar <- data.frame(matrix(nrow = 2, ncol = ncol(df.full)))
colnames(p.radar) <-  names(df.full) #c("mod_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat")

for (i in 1:ncol(df.radar)) {
  #i=1
  p.radar[1, names(df.radar)[i]] <- paste0(names(sort(table(na.omit(df.radar[, i]))/length(na.omit(df.radar[, i])), decreasing = T)[1]), " (n=", length(na.omit(df.radar[, i])), ")") 
  p.radar[2, names(df.radar)[i]] <- sort(table(na.omit(df.radar[, i]))/length(na.omit(df.radar[, i])), decreasing = T)[1]
}

names(p.radar) <- p.radar[1,]
p.radar <- p.radar[-1,]
p.radar <- rbind(0,p.radar)
p.radar <- rbind(1,p.radar)
p.radar <- lapply(p.radar,as.numeric) 
names.radar <- names(p.radar)
p.radar <- data.frame(p.radar)
names(p.radar) <- names.radar
#write.csv2(p.radar, "p.radar.csv") #for ChatGPT

radarchart(p.radar
           , axistype=1
           , pcol=rgb(0.2,0.5,0.5,0.9) 
           , pfcol=rgb(0.2,0.5,0.5,0.5) 
           , plwd=4 
           #, cglcol="grey"
           , cglty=1
           , axislabcol="grey"
           , caxislabels= c(0, 20, 50, 75, 100)
           , cglwd=0.8
           , vlcex=1
           , title = "trois mode de peche confondus, 2020-2024"
)

#ggsave doesn't work, do it manually; adjust the 600x1200

plot.radar <- function(col., col.fill, title.) {
  
  #df.plora <- filter(df.full, mod_peche == "peche du bord")
  #df.plora <- droplevels(df.plora)
  #unique(df.plora$mod_peche)
  ##unique(df.plora$a)
  #df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
  
  p.radar <- data.frame(matrix(nrow = 2, ncol = ncol(df.plora))) # define df.radar prior running the function
  colnames(p.radar) <- names(df.plora) #c("mode_peche","age_classe","cat_pro","classe_avis_pnm","zone","nb_sort_an_cat"
  
  for (i in 1:ncol(df.plora)) {
    #i=1
    #df.plora = df.radar.pdb 
    #col. = "#F8766D" 
    #col.fill = "#F8766D50"
    #title. = "peche du bord"
    p.radar[1, names(df.plora)[i]] <- paste0(names(sort(table(na.omit(df.plora[, i]))/length(na.omit(df.plora[, i])), decreasing = T)[1]), " (n=", length(na.omit(df.plora[, i])), ")") 
    p.radar[2, names(df.plora)[i]] <- sort(table(na.omit(df.plora[, i]))/length(na.omit(df.plora[, i])), decreasing = T)[1]
  }
  
  names(p.radar) <- p.radar[1,]
  p.radar <- p.radar[-1,]
  p.radar <- rbind(0,p.radar)
  p.radar <- rbind(1,p.radar)
  p.radar <- lapply(p.radar,as.numeric) 
  names.radar <- names(p.radar)
  p.radar <- data.frame(p.radar)
  names(p.radar) <- names.radar
  #write.csv2(p.radar, "p.radar.csv") #for ChatGPT
  
  p.radar <<- within(p.radar, rm("NA")) # same function than above, no NA in the p.radar df in this case when considering all the years by mode de peche, so Message d'avis when running the function is normal. (Message d'avis : Dans rm("NA") : objet 'NA' introuvable)
  
  radarchart(p.radar
             , axistype=1
             , pcol=col. #rgb(0.2,0.5,0.5,0.9) 
             , pfcol=col.fill #rgb(0.2,0.5,0.5,0.5) 
             , plwd=4 
             #, cglcol="grey"
             , cglty=1
             , axislabcol="grey"
             , caxislabels= c(0, 20, 50, 75, 100)
             , cglwd=0.8
             , vlcex=1
             , title = title.
  )
  
}

# Create a function to generate summarized statistics for each variable, excluding NAs
summarize_variable <- function(df) {
  # Apply to each column and calculate count and percentage excluding NAs
  summary_stats <- lapply(names(df), function(var) {
    # Remove NAs for the current variable
    df_no_na <- df %>% filter(!is.na(.data[[var]]))
    
    # Group by the variable and calculate count and percentage
    df_no_na %>%
      group_by(.data[[var]]) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round((Count / sum(Count)) * 100, 2)) %>%
      arrange(desc(Count)) %>%
      mutate(Variable = var) %>%
      rename(Category = .data[[var]])
  })
  
  # Combine all summary tables into one dataframe
  summary_table <- bind_rows(summary_stats)
  return(summary_table)
}

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- filter(df.full, mod_peche == "peche du bord")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
#saveRDS(df.plora, "df.plora.rds") #for ChatGPT
summary_table <- summarize_variable(df.plora)
print(summary_table)
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2020-2024")
rm(p.radar)

df.plora <- filter(df.full, mod_peche == "peche embarquée")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
summary_table <- summarize_variable(df.plora)
print(summary_table)
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2020-2024")
rm(p.radar)

df.plora <- filter(df.full, mod_peche == "chasse sous-marine")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
summary_table <- summarize_variable(df.plora)
print(summary_table)
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2020-2024")
rm(p.radar)

par(op)
par(mfrow = c(1,1))

#ggsave doesn't work, do it manually; 600x1200 ; idem for the next ones

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red")))


# table enquête en ligne + figs ####

# I will now work with the df.full df. of above multivariate spatial analysis and Kiviat diagrams.
# so all the previous code was removed, copy paste in another R script just in case, but always available in the above code since same preparation work of variables.

# Function to calculate percentages for each categorical variable
calculate_percentages <- function(df.red) {
  tbl <- table(df.red)
  perc <- prop.table(tbl) * 100
  return(perc)
}
# Function to calculate occurrence for each categorical variable
calculate_occurences <- function(df.red) {
  tbl <- table(df.red)
  return(tbl)
}

# Function to display the results
display_results <- function() {
print(percentage_results)
results_perc <- do.call(rbind, lapply(percentage_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
print(occurence_results)
results_occ <- do.call(rbind, lapply(occurence_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
results_df <- bind_cols(results_perc, results_occ)
setdiff(results_df$df.red...2, results_df$df.red...5)
results_df <- results_df[, c(1,2,3,6)]
results_df$Var1...1 <- rownames(results_df)
results_df$Var1...1 <- str_sub(results_df$Var1, end=-3)
#results_df$Var1...1 <- ifelse(stri_sub(results_df$Var1...1,nchar(results_df$Var1...1)-1,-1) %in% c(".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", ".10"), str_sub(results_df$Var1, end=-3), results_df$Var1...1)  
names(results_df)[names(results_df) == "Var1...1"] <- "parametre"
names(results_df)[names(results_df) == "df.red...2"] <- "categorie"
names(results_df)[names(results_df) == "Freq...3"] <- "pourcentage"
names(results_df)[names(results_df) == "Freq...6"] <- "n"
results_df %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
results <<- left_join(results_df, N)
return(results)
}

# I will here rename the df of above multivariate spatial analysis in order not to change the script below
unique(df.msa$a)
df.red <- #na.omit
  (filter(df.msa[, c(
  "a",
  "mod_peche",
  "sexe",
  "res_tour",
  "Temps_peche_estime.cat2h",
  "zone.gp",
  "age_classe",
  "cat_pro.gp",
  "nb_sort_an_cat",
  "classe_avis_pnm",
  "poss_bat",
  "time.day.gp",
  #"dep.median.rd.chr",
  "dep.mod_peche.chr")
], a %in% c("2020", "2021", "2022", "2023", "2024")))
df.red <- droplevels(df.red) # year 2023, 2024 removed when na.omit cfr many variables without data
unique(df.red$a)

# Apply this function to each column in the DataFrame
# Use sapply if you want a list of results for each column
percentage_results <- sapply(df.red, calculate_percentages)
occurence_results <- sapply(df.red, calculate_occurences)
#saveRDS(percentage_results, "percentage_results.rds") #for ChatGPT
# Display the results
display_results()
results_df.0 <- results ; rm(results)
write.csv2(results_df.0, "Tables/Summary/summary_prop_2020-2024.csv", row.names = F)
write_xlsx(results_df.0, "Tables/Summary/summary_prop_2020-2024.xlsx")

df.1 <- filter(df.red, a %in% c("2020", "2021", "2022"))
df.1[, colSums(is.na(df.1)) != nrow(df.1)]
df.1 <- droplevels(df.1)

percentage_results <- sapply(df.1, calculate_percentages)
occurence_results <- sapply(df.1, calculate_occurences)
display_results()
results_df.1 <- results ; rm(results)
write.csv2(results_df.1, "Tables/Summary/summary_prop_2020-2022.csv", row.names = F)
write_xlsx(results_df.1, "Tables/Summary/summary_prop_2020-2022.xlsx")

df.2 <- filter(df.red, a %in% c("2023", "2024"))
df.2[, colSums(is.na(df.2)) != nrow(df.2)]
df.2 <- droplevels(df.2)

percentage_results <- sapply(df.2, calculate_percentages)
occurence_results <- sapply(df.2, calculate_occurences)
display_results()
results_df.2 <- results ; rm(results)
write.csv2(results_df.2, "Tables/Summary/summary_prop_2023-2024.csv", row.names = F)
write_xlsx(results_df.2, "Tables/Summary/summary_prop_2023-2024.xlsx")

# tables by year and/or mod_peche

results_year <- data.frame(matrix(nrow = 1, ncol = 6))
colnames(results_year) <-  c("a", "parametre", "categorie", "pourcentage", "n", "N")

for (i in 1:length(unique(df.red$a))) {
  
#i=1  
df.a <- filter(df.red, a %in% unique(df.red$a)[i])
df.a[, colSums(is.na(df.a)) != nrow(df.a)]
df.a <- droplevels(df.a)

percentage_results <- sapply(df.a, calculate_percentages)
occurence_results <- sapply(df.a, calculate_occurences)
display_results()
results <- add_column(results, a = unique(unique(df.red$a)[i]), .before = "parametre")
results_year <- bind_rows(results_year, results)
results_year$parametre <- ifelse(results_year$categorie %in% c("2020", "2021", "2022", "2023", "2024"), "a", results_year$parametre)
#assign(paste0("results_", unique(df.red$a)[i]), results) 
results_year <<- results_year
rm(results)

}

results_year <- results_year[-1,]

write.csv2(results_year, "Tables/Summary/summary_prop_year.csv", row.names = F)
write_xlsx(results_year, "Tables/Summary/summary_prop_year.xlsx")

results_mod_peche <- data.frame(matrix(nrow = 1, ncol = 6))
colnames(results_mod_peche) <-  c("mod_peche", "parametre", "categorie", "pourcentage", "n", "N")

for (i in 1:length(unique(df.red$mod_peche))) {
  
  #i=1  
  df.mod_peche <- filter(df.red, mod_peche %in% unique(df.red$mod_peche)[i])
  df.mod_peche[, colSums(is.na(df.mod_peche)) != nrow(df.mod_peche)]
  df.mod_peche <- droplevels(df.mod_peche)
  
  percentage_results <- sapply(df.mod_peche, calculate_percentages)
  occurence_results <- sapply(df.mod_peche, calculate_occurences)
  display_results()
  results <- add_column(results, mod_peche = unique(unique(df.red$mod_peche)[i]), .before = "parametre")
  results_mod_peche <- bind_rows(results_mod_peche, results)
  results_mod_peche$parametre <- ifelse(results_mod_peche$categorie %in% c("peche du bord", "chasse sous-marine", "peche embarquée", "peche à l'oursin"), "mod_peche", results_mod_peche$parametre)
  #assign(paste0("results_", unique(df.red$mod_peche)[i]), results) 
  results_mod_peche <<- results_mod_peche
  rm(results)
  
}

results_mod_peche <- results_mod_peche[-1,]

write.csv2(results_mod_peche, "Tables/Summary/summary_prop_mod_peche.csv", row.names = F)
write_xlsx(results_mod_peche, "Tables/Summary/summary_prop_mod_peche.xlsx")

results_year_mod_peche <- data.frame(matrix(nrow = 1, ncol = 7))
colnames(results_year_mod_peche) <-  c("a", "mod_peche", "parametre", "categorie", "pourcentage", "n", "N")
# Do not consider peche à l'oursin cfr only 2 observations so the for loop does not work with the functions under their current status; no reason to change it
#df.red.bis <- filter(df.red, mod_peche != "peche à l'oursin")
df.red.bis <- filter(df.red)

for (i in 1:nrow(unique(df.red.bis[, c("a", "mod_peche")]))) {
  
  #i=7  
  df.a_mod_peche <- filter(df.red.bis, a == unique(df.red.bis[, c("a", "mod_peche")])[i,"a"] & mod_peche == unique(df.red.bis[, c("a", "mod_peche")])[i,"mod_peche"])
  df.a_mod_peche[, colSums(is.na(df.a_mod_peche)) != nrow(df.a_mod_peche)]
  df.a_mod_peche <- droplevels(df.a_mod_peche)
  
  percentage_results <- sapply(df.a_mod_peche, calculate_percentages)
  occurence_results <- sapply(df.a_mod_peche, calculate_occurences)

  print(percentage_results)
  results_perc <- do.call(rbind, lapply(percentage_results, function(x) {
    if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
  }))
  print(occurence_results)
  results_occ <- do.call(rbind, lapply(occurence_results, function(x) {
    if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
  }))
  results_df <- bind_cols(results_perc, results_occ)
  
  if (ncol(results_df) == 6) {
  
  setdiff(results_df$df.red...2, results_df$df.red...5)
  results_df <- results_df[, c(1,2,3,6)]
  results_df$Var1...1 <- rownames(results_df)
  results_df$Var1...1 <- ifelse(stri_sub(results_df$Var1...1,nchar(results_df$Var1...1)-1,-1) %in% c(".1", ".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", ".10"), str_sub(results_df$Var1, end=-3), results_df$Var1...1)  
  names(results_df)[names(results_df) == "Var1...1"] <- "parametre"
  names(results_df)[names(results_df) == "df.red...2"] <- "categorie"
  names(results_df)[names(results_df) == "Freq...3"] <- "pourcentage"
  names(results_df)[names(results_df) == "Freq...6"] <- "n"
  results_df %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
  results <- left_join(results_df, N)
  
  } else if  (ncol(results_df) == 2) {
    
    names(results_df)[names(results_df) == "V1...1"] <- "pourcentage"
    names(results_df)[names(results_df) == "V1...2"] <- "n"
    results_df <- add_column(results_df, parametre.categorie = rownames(results_df), .before = "pourcentage")
    results_df <- add_column(results_df, parametre = colnames(df.a_mod_peche), .before = "pourcentage")
    results_df <- add_column(results_df, categorie = NA, .before = "pourcentage")
    for (j in 1:nrow(results_df)) { 
      results_df$categorie[j] <- gsub(paste0(results_df$parametre[j], "."), "", results_df$parametre.categorie[j])
      }
    results_df %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
    results <- left_join(results_df, N)
    rownames(results) <- NULL
    results <- results[,2:6]
  } 
  
  results <- add_column(results, mod_peche = unique(df.red.bis[, c("a", "mod_peche")])[i,"mod_peche"], .before = "parametre")
  results <- add_column(results, a = unique(df.red.bis[, c("a", "mod_peche")])[i,"a"], .before = "mod_peche")
  results_year_mod_peche <- bind_rows(results_year_mod_peche, results)
  results_year_mod_peche$parametre <- ifelse(results_year_mod_peche$categorie %in% c("peche du bord", "chasse sous-marine", "peche embarquée", "peche à l'oursin"), "mod_peche", results_year_mod_peche$parametre)
  results_year_mod_peche$parametre <- ifelse(results_year_mod_peche$categorie %in% c("2020", "2021", "2022", "2023", "2024"), "a", results_year_mod_peche$parametre)
  #assign(paste0("results_", unique(df.red$mod_peche)[i]), results) 
  results_year_mod_peche <<- results_year_mod_peche
  rm(results)
  
}

results_year_mod_peche <- results_year_mod_peche[-1,]

write.csv2(results_year_mod_peche, "Tables/Summary/summary_prop_year_mod_peche.csv", row.names = F)
write_xlsx(results_year_mod_peche, "Tables/Summary/summary_prop_year_mod_peche.xlsx")


unique(results_df.0$parametre)
results_df.0 %>% filter(parametre == "zone.gp") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("Secteurs de pêches préférés 2020-2024 (N=", unique(df.plot$N), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(pourcentage/100)), position = position_stack(vjust = 0.55))  # Ajouter les pourcentages
ggsave("Figs/Secteurs/secteurs_2020-2024.png", width = 7, height = 7)

results_df.1 %>% filter(parametre == "zone.gp") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("Secteurs de pêches préférés 2020-2022 (N=", unique(df.plot$N), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(pourcentage/100)), position = position_stack(vjust = 0.55))  # Ajouter les pourcentages
ggsave("Figs/Secteurs/secteurs_2020-2022.png", width = 7, height = 7)

results_df.2 %>% filter(parametre == "zone.gp") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("Secteurs de pêches préférés 2023-2024 (N=", unique(df.plot$N), ")")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::percent(pourcentage/100)), position = position_stack(vjust = 0.55))  # Ajouter les pourcentages
ggsave("Figs/Secteurs/secteurs_2023-2024.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df.0", "results_df.1", "results_df.2")))


# plot species targeted ####

survey$espcib1
table(survey$espcib1)
survey$espcib2
table(survey$espcib2)
survey$espcib3
table(survey$espcib3)

df. <- survey[, c("a", "espcib1", "espcib2", "espcib3")]
#saveRDS(df., "df.rds") # for ChatGPT

plot.bar.sp <- function(var., title.) {
  
  df_clean <- df.sp[,var.] %>%
    filter(df.sp[,var.] != "NA") %>%
    na.omit() %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable, value) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(total = sum(count), percentage = (count / total) * 100) %>% 
    arrange(desc(count))
  
  ggplot(df_clean, aes(x = reorder(value, percentage), y = percentage)) +
    geom_bar(stat = "identity", colour = 'mediumblue', fill = "mediumblue", width = 0.5) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(label = count, y = percentage + 1), hjust = 1.1, color = "black", size = 3.5) +
    #facet_wrap(~variable, scales = "free_y") +
    labs(x = "Species", y = "Percentage of fishers") +
    ggtitle(paste0(title., " (N=", sum(df_clean$count),")")) +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    coord_flip() +
    theme(panel.background=element_blank(), axis.line = element_line(), axis.ticks.y =element_blank())

}

df. %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2020-2024")
ggsave("Figs/Espece/espece.cible.1_2020-2024.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2020-2024")
ggsave("Figs/Espece/espece.cible.2_2020-2024.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2020-2024")
ggsave("Figs/Espece/espece.cible.3_2020-2024.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2020-2022")
ggsave("Figs/Espece/espece.cible.1_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2020-2022")
ggsave("Figs/Espece/espece.cible.2_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2020-2022")
ggsave("Figs/Espece/espece.cible.3_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2023-2024")
ggsave("Figs/Espece/espece.cible.1_2023-2024.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2023-2024")
ggsave("Figs/Espece/espece.cible.2_2023-2024.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023", "2024")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2023-2024")
ggsave("Figs/Espece/espece.cible.3_2023-2024.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df.0", "results_df.1", "results_df.2")))


# plot species and biomass fished ####

df.fsh <- fishingdf.fsh <- fishingdf.fsh <- fishing[, c("a", "nom_scien", "nb_ind", "tail_cm", "poids_g")]
#saveRDS(df.fsh, "df.fsh.rds") # for ChatGPT

unique(df.fsh$nom_scien)
df.fsh %>% filter(nom_scien %notin% c("0", "NA")) -> df.fsh
sum(df.fsh$poids_g, na.rm = TRUE)
sum(df.fsh$nb_ind, na.rm = TRUE)
#NB: I think there are some species fished but not weighed!

plot.bar.fsh <- function(title.) {
  
  df_sum <- df.fsh.red %>%
    group_by(nom_scien) %>%
    summarise(total_poids_g = round(sum(poids_g, na.rm = TRUE), digits = 0), 
              perc_poids_g = round((sum(poids_g, na.rm = TRUE)/(sum(df.fsh$poids_g, na.rm = TRUE)))*100, digits = 2),
              .groups = 'drop')
  
  ggplot(df_sum, aes(x = reorder(nom_scien, perc_poids_g), y = log10(perc_poids_g+1), group = 1)) +
    geom_point(colour = 'mediumblue') +
    geom_segment(aes(xend=nom_scien), yend=0, colour = 'mediumblue') +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(label = total_poids_g, y = log10(perc_poids_g+1)+0.05), hjust = 1.1, color = "black", size = 2.5) +
    #facet_wrap(~variable, scales = "free_y") +
    labs(x = "Species", y = expression(Log[10](biomasse~pêchée~+~1))) +
    ggtitle(paste0(title., " (somme = ", sum(df_sum$total_poids_g)," g)")) +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    coord_flip() +
    theme(panel.background=element_blank(), axis.line = element_line(), axis.ticks.y =element_blank())
  
}

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2020-2024")
ggsave("Figs/Biomasse/biomasse_2020-2024.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2020", "2021", "2022") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2020-2022")
ggsave("Figs/Biomasse/biomasse_2020-2022.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2023", "2024") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2023-2024")
ggsave("Figs/Biomasse/biomasse_2023_2024.png", width = 7, height = 7)

plot.bar.fsh <- function(title.) {
  
  df_sum <- df.fsh.red %>%
  group_by(nom_scien) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh$nb_ind, na.rm = TRUE)))*100,
            .groups = 'drop')
  
  ggplot(df_sum, aes(x = reorder(nom_scien, perc_nb_ind), y = log10(perc_nb_ind+1), group = 1)) +
    geom_point(colour = 'mediumblue') +
    geom_segment(aes(xend=nom_scien), yend=0, colour = 'mediumblue') +
    scale_y_continuous(expand = c(0, 0)) +
    geom_text(aes(label = total_nb_ind, y = log10(perc_nb_ind+1)+0.05), hjust = 1.1, color = "black", size = 2.5) +
    #facet_wrap(~variable, scales = "free_y") +
    labs(x = "Species", y = expression(Log[10](percentage~of~individuals~+~1))) +
    ggtitle(paste0(title., " (N=", sum(df_sum$total_nb_ind),")")) +
    #theme_minimal() +
    #theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    coord_flip() +
    theme(panel.background=element_blank(), axis.line = element_line(), axis.ticks.y =element_blank())
  
}

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2020-2024")
ggsave("Figs/Individu/individu_2020-2024.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2020", "2021", "2022")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2020-2022")
ggsave("Figs/Individu/individu_2020-2022.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2023", "2024")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2023-2024")
ggsave("Figs/Individu/individu_2023-2024.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df.0", "results_df.1", "results_df.2")))


# plot main fish species caught by size class + mesh size ####

fishing <- readRDS("Raw data_mined/terrain.rds")

# upload terrain data corrected by Michel directly in the initial compiled terrain csv and opened in excel
fishing.new <- read_excel("Raw data_mined/BDD_peche_loisir_2024_09_27.xlsx", sheet = "Pêche du jour", col_names = T)
names(fishing)
names(fishing.new)
fishing.new$Temps_peche_effectif <- as_hms(fishing.new$Temps_peche_effectif)

head(fishing$date); class(fishing$date) 
head(fishing.new$date); class(fishing$date) 
#as.POSIXct(fishing.new$date, origin = "1970-01-01", format = "%Y-%m-%d")
fishing.new$date <- as.POSIXct(as.integer(fishing.new$date) * 86400, origin = "1899-12-30", tz = "UTC")

fishing.new$tail_cm <- as.numeric(fishing.new$tail_cm)

head(fishing$y_lat_DD) ; class(fishing$y_lat_DD) 
fishing$y_lat_DD <- round(fishing$y_lat_DD, digits = 5)
head(fishing.new$y_lat_DD) ; class(fishing.new$y_lat_DD) 
fishing.new$y_lat_DD <- round(fishing.new$y_lat_DD, digits = 5)

head(fishing$x_lon_DD) ; class(fishing$x_lon_DD) 
fishing$x_lon_DD <- round(fishing$x_lon_DD, digits = 5)
head(fishing.new$x_lon_DD) ; class(fishing.new$x_lon_DD)
fishing.new$x_lon_DD <- as.numeric(fishing.new$x_lon_DD)
fishing.new$x_lon_DD <- round(fishing.new$x_lon_DD, digits = 5)

names(fishing)
names(fishing.new)
intersect(names(fishing), names(fishing.new))

df.fsh <- fishing[, c("a", "nom_scien", "nb_ind", "tail_cm")]
#saveRDS(df.fsh, "df.fsh.rds") # for ChatGPT
# Do it on Michel fishing.new corrected data
df.fsh <- fishing.new[, c("a", "nom_scien", "nb_ind", "tail_cm")]
#saveRDS(df.fsh, "df.fsh.rds") # for ChatGPT

unique(df.fsh$nom_scien)
df.fsh %>% filter(nom_scien %notin% c("0", "NA")) -> df.fsh
unique(df.fsh$nom_scien)

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023", "2024")) -> df.fsh.red

df_sum <- df.fsh.red %>%
  group_by(nom_scien) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh.red$nb_ind, na.rm = TRUE)))*100,
            .groups = 'drop')
df_sum <-df_sum[order(df_sum$total_nb_ind, decreasing = T),]

df.fsh %>% filter(a %in% c("2020", "2021", "2022")) -> df.fsh.red.1

df_sum.1 <- df.fsh.red.1 %>%
  group_by(nom_scien) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh.red.1$nb_ind, na.rm = TRUE)))*100,
            .groups = 'drop')
df_sum.1 <-df_sum.1[order(df_sum.1$total_nb_ind, decreasing = T),]

df.fsh %>% filter(a %in% c("2023", "2024")) -> df.fsh.red.2

df_sum.2 <- df.fsh.red.2 %>%
  group_by(nom_scien) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh.red.2$nb_ind, na.rm = TRUE)))*100,
            .groups = 'drop')
df_sum.2 <-df_sum.2[order(df_sum.2$total_nb_ind, decreasing = T),]

sort(df_sum$nom_scien[1:8])
sort(df_sum.1$nom_scien[1:8])
sort(df_sum.2$nom_scien[1:8])

sp. <- unique(sort(c(df_sum$nom_scien[1:8], df_sum.1$nom_scien[1:8])))
sp. <- unique(sort(c(sp., df_sum.2$nom_scien[1:8])))
sort(sp.)
size.at.maturity <- data.frame(species = sp.,
           size = c(18, #"Coris julis", Michel
                        #round(34.6), #Dentex dentex (https://fishbase.mnhn.fr/)
                        12, #"Diplodus annularis", Michel
                        round(20.5), #"Diplodus sargus", Tsikliras & Stergiou (2013) - Rev Fish Biol Fisheries "Size at maturity of Mediterranean marine fishes"
                        round((17.6+15.3)/2,0), #"Loligo sp", "The ML 50% of females was estimated to be 17.6 cm. The ML 50% of males was estimated to be 15.3 cm." Moreno et al., Aquat. Living Resour. 18, 377–384 (2005), Environmental influences on age and size at maturity of Loligo vulgaris 
                        round(18.8), #Oblada melanura (https://fishbase.mnhn.fr/)
                        10, #"Pagellus erythrinus", Michel
                        round(13.7), #"Serranus cabrilla" (https://fishbase.mnhn.fr/)
                        16, #"Serranus scriba", Michel
                        19 #"Spondyliosoma cantharus", Michel
                        ))
# NB: we have to round the data otherwize the function to plot values is not working cfr round data to text values

# Plaisanciers AM relative aux TAILLES OU POIDS MINIMAUX DE CAPTURE DES POISSONS ET AUTRES ORGANISMES MARINS EN MER MEDITERRANEE (https://www.dirm.mediterranee.developpement-durable.gouv.fr/)

sort(sp.)
sp.catch <- sp.[sp.%in%c("Diplodus annularis", "Diplodus sargus", "Pagellus erythrinus", "Spondyliosoma cantharus")] 
size.catch <- data.frame(species = sp.catch,
                               size = c(12, #"Diplodus annularis"
                                            23,  #"Diplodus sargus",
                                            15, #"Pagellus erythrinus", Michel
                                            23 #"Spondyliosoma cantharus", Michel
                               ))

# according to maturity size

size.sp. <- function(df.fsh., df.size, df., years.) {

#df.fsh.=
#df.=
#df.size=
#years.=

for(i in c(1:6)) {

#i=6
#df.fsh. = df.fsh.red
#df.size = size.at.maturity
#df. = df_sum
#years. = "2020-2024"

df.i <- filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])
print(sp. <- unclass(unique(filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])["nom_scien"]))$nom_scien)
names(df.i)
#df.i <- df.i[, c("nom_scien", "nb_ind", "tail_cm")] #do not consider var "poids_g" cfr NAs ; can also remove year cfr not needed in the analysis below
(sexsize=(filter(df.size,species==sp.))["size"]$size)

# Remove NAs prior to use the rep function
nrow(df.i)
nrow(na.omit(df.i))
df.i <- na.omit(df.i)
# Use the rep function to repeat rows
df.i <- df.i[rep(1:nrow(df.i), df.i$nb_ind), ]
df.i$nb_ind <- 1
df.i <- data.frame(df.i)

hist(unlist(filter(df.fsh., nom_scien == sp.)[,"tail_cm"]), xlab = "size (cm)", main = sp.)

(data <- df.i$tail_cm)
(border_value <- sexsize)

show(na.omit(nrow(df.i)))
show(min(df.i$tail_cm))
show(max(df.i$tail_cm))
show(mean(df.i$tail_cm))
show(sd(df.i$tail_cm))
df.i %>% filter(tail_cm >= (sexsize=(filter(df.size,species==sp.))["size"]$size)) %>% summarize(nb.mature = n()) -> mature
show((mature$nb.mature/nrow(df.i))*100)

# Split data into two parts based on the border value
(data_left <- na.omit(data[data < border_value]))
(data_right <- na.omit(data[data >= border_value]))

# Determine the number of groups on each side
total_groups <- 5
(left_proportion <- (sexsize-min(data, na.rm = T))/(max(data, na.rm = T)-min(data, na.rm = T)))
(left_proportion <- ifelse(left_proportion < 0, 0, left_proportion))
(right_proportion <- (max(data, na.rm = T)-sexsize)/(max(data, na.rm = T)-min(data, na.rm = T))) 
(right_proportion <- ifelse(right_proportion > 1, 1, right_proportion))

(left_groups <- round(left_proportion * total_groups))
(right_groups <- total_groups - left_groups)

# Define breaks for left side
(if (length(data_left) > 0) {
  left_breaks <- seq(min(data_left), sexsize, length.out = left_groups + 1)
} else {
  left_breaks <- NULL
})

# Define breaks for right side
(if (length(data_right) > 0) {
  right_breaks <- seq(sexsize, max(data_right), length.out = right_groups + 1)
} else {
  right_breaks <- NULL
})

# Combine breaks
(breaks <- unique(c(left_breaks, right_breaks)))
(breaks <- round(breaks))

df.i$tail_cm <- round(df.i$tail_cm, digits = 0)

s.cl <- breaks
(s.cl.range <- c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
df.i$tail_cm_classe <- NA
df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[2], paste0("[",s.cl[1],"-",s.cl[2],"["), df.i$tail_cm_classe)
df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[3] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[2],"-",s.cl[3],"["), df.i$tail_cm_classe)
df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[4] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[3],"-",s.cl[4],"["), df.i$tail_cm_classe)
df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[5] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[4],"-",s.cl[5],"["), df.i$tail_cm_classe)
df.i$tail_cm_classe <- ifelse(df.i$tail_cm <= s.cl[6] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[5],"-",s.cl[6],"]"), df.i$tail_cm_classe)
df.i$tail_cm_classe <- as.factor(df.i$tail_cm_classe)

as.character(na.omit(unique(df.i$tail_cm_classe)))
setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))
ifelse(length(setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))) >=1, df.i <- add_row(df.i, tail_cm_classe = setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))), df.i)
df.i$tail_cm_classe <- factor(df.i$tail_cm_classe, levels = c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
levels(df.i$tail_cm_classe)

#df.i <- na.omit(df.i[,c("tail_cm", "tail_cm_classe")]) 
#levels(df.i$tail_cm_classe)
df.i$tail_cm_classe.num <- NA
df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[1],"-",s.cl[2],"["), 1, df.i$tail_cm_classe.num)
df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[2],"-",s.cl[3],"["), 2, df.i$tail_cm_classe.num)
df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[3],"-",s.cl[4],"["), 3, df.i$tail_cm_classe.num)
df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[4],"-",s.cl[5],"["), 4, df.i$tail_cm_classe.num)
df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[5],"-",s.cl[6],"]"), 5, df.i$tail_cm_classe.num)
df.i$tail_cm_classe.num <- as.integer(df.i$tail_cm_classe.num)
(df.i$tail_cm_classe.num)

(txt. <- paste0("[",sexsize,"-"))
(threshold_category <- unlist(unique((df.i[grep(paste0("\\",txt.), df.i$tail_cm_classe), ])$tail_cm_classe.num)))
df.i$mature <- NA
df.i$mature <- ifelse(df.i$tail_cm_classe.num < threshold_category, "non", "oui")
df.i$mature <- as.factor(df.i$mature)
df.i.narm <- na.omit(df.i[, c("tail_cm", "tail_cm_classe", "tail_cm_classe.num", "mature")])
print(table(df.i.narm[, c("tail_cm_classe", "tail_cm_classe.num", "mature")]))
df.i.narm %>% group_by(tail_cm_classe, mature) %>% summarise(n())

barplot.i <- ggplot(df.i.narm, aes(x = tail_cm_classe, fill = mature)) +
  geom_bar() +
  scale_x_discrete(drop=FALSE) +
  scale_fill_manual(values = c("non" = "orange", "oui" = "lightblue")) +
  labs(title = paste0(unique(df.i$nom_scien), " (N = ", nrow(df.i.narm), ", ", years., ")"),
       x = "Taille (cm)",
       y = "Nombre d'individus",
       fill = "Mature") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_y_continuous(expand = c(0,0))
barplot.i <- barplot.i + ggtitle(bquote(italic(.(unique(df.i$nom_scien)))~.(years.)~"(N ="~.(nrow(df.i.narm))*")"))
print(barplot.i)
ggsave(paste0("Figs/Sc_nb_sp/", unique(na.omit(df.i$nom_scien)), "_", years.,"_barplot.png"), barplot.i, width = 5, height = 4)

}

}

size.sp.(df.fsh. = df.fsh.red, df.size = size.at.maturity, df. = df_sum, years. = "2020-2024")
size.sp.(df.fsh. = df.fsh.red.1, df.size = size.at.maturity, df. = df_sum.1, years. = "2020-2022")
size.sp.(df.fsh. = df.fsh.red.2, df.size = size.at.maturity, df. = df_sum.2, years. = "2023-2024")

df_sum[1:6, ]
df.fsh.red %>% filter(nom_scien %in% df_sum[1:6, ]$nom_scien) -> df.fsh.red.6sp
df.fsh.red.6sp$matu.size_cm <- NA
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[1] #Coris julis
       , 18, df.fsh.red.6sp$matu.size_cm)
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[2] #Diplodus annularis
       , 12, df.fsh.red.6sp$matu.size_cm)
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[3] #Diplodus sargus
       , round(20.5), df.fsh.red.6sp$matu.size_cm)
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[4] #Serranus cabrilla
       , round(13.7), df.fsh.red.6sp$matu.size_cm)
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[5] #Serranus scriba
       , 16, df.fsh.red.6sp$matu.size_cm)
df.fsh.red.6sp$matu.size_cm <- ifelse(df.fsh.red.6sp$nom_scien == unique(df.fsh.red.6sp$nom_scien)[6] #Spondyliosoma cantharus
       , 19, df.fsh.red.6sp$matu.size_cm)

df.fsh.red.6sp$mature <- NA
df.fsh.red.6sp$mature <- ifelse(df.fsh.red.6sp$tail_cm>= df.fsh.red.6sp$matu.size_cm, "oui", "non")

# Calculate occurrence using table()
occurrences <- table(df.fsh.red.6sp$mature)

# Calculate percentage using prop.table()
percentages <- prop.table(occurrences) * 100

# Combine results into a dataframe
data <- data.frame(
  Mature = names(occurrences),
  Occurrence = as.numeric(occurrences),
  Percentage = round(as.numeric(percentages), 2)
)

# Compute positions for the labels and create a custom label combining percentage and count
data$ypos <- cumsum(data$Percentage) - 0.5 * data$Percentage
data$label <- paste0(data$Percentage, "%", "\n(n = ", data$Occurrence, ")")

# Create the donut chart
ggplot(data, aes(x = 2, y = Percentage, fill = Mature)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Create the donut segments
  coord_polar(theta = "y") +  # Convert to polar coordinates
  geom_text(aes(y = ypos, label = label), color = "black", size = 5) +  # Add percentage and count labels
  theme_void() +  # Remove background and axis
  xlim(0.5, 2.5) +  # Adjust the x-axis limits to create a donut
  theme(legend.position = "right") +  # Place the legend on the right
  scale_fill_manual(values = c("lightblue", "mediumblue")) +  # Set custom colors
  labs(
    title = "Pourcentage",
    fill = "Mature"
  )

ggplot(data, aes(x="", y=Percentage, fill=Mature)) + #y=n
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("Maturité des 6 espèces les plus pêchées\n(n = ", sum(data$Occurrence, na.rm = TRUE), ", 2020-2024)")) +
  theme_bw() +
  theme(#axis.line=element_blank(),
    #axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    #panel.grid.major=element_blank(),
    #panel.grid.minor=element_blank(),
    #plot.background=element_blank()
  ) + 
  scale_fill_brewer(palette="Blues") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = ypos, label = label), color = "black", size = 4)

ggsave("Figs/maturity.png", width = 7, height = 4)

# according to maille size

maille.sp. <- function(df.fsh., df.size, df., years.) {
  
  #df.fsh.=df.fsh.red.3
  #df.=df_sum.3
  #df.size=size.catch
  #years.="2020-2024"
  
  for(i in c(1:nrow(df.size))) {
    
    #i=1
    
    df.i <- filter(df.fsh., nom_scien == df.size$species[i])
    print(sp. <- unclass(unique(df.i$nom_scien)))
    names(df.i)
    #df.i <- df.i[, c("nom_scien", "nb_ind", "tail_cm")] #do not consider var "poids_g" cfr NAs ; can also remove year cfr not needed in the analysis below
    (sexsize=(filter(df.size,species==sp.))["size"]$size)
    
    # Remove NAs prior to use the rep function
    nrow(df.i)
    nrow(na.omit(df.i))
    df.i <- na.omit(df.i)
    # Use the rep function to repeat rows
    df.i <- df.i[rep(1:nrow(df.i), df.i$nb_ind), ]
    df.i$nb_ind <- 1
    df.i <- data.frame(df.i)
    
    hist(unlist(filter(df.fsh., nom_scien == sp.)[,"tail_cm"]), xlab = "size (cm)", main = sp.)
    
    (data <- df.i$tail_cm)
    (border_value <- sexsize)
    
     show(na.omit(nrow(df.i)))
     show(min(df.i$tail_cm))
     show(max(df.i$tail_cm))
     show(mean(df.i$tail_cm))
     show(sd(df.i$tail_cm))
     df.i %>% filter(tail_cm >= (sexsize=(filter(df.size,species==sp.))["size"]$size)) %>% summarize(nb.mature = n()) -> mature
     show((mature$nb.mature/nrow(df.i))*100)
    
    # Split data into two parts based on the border value
    (data_left <- na.omit(data[data < border_value]))
    (data_right <- na.omit(data[data >= border_value]))
    
    # Determine the number of groups on each side
    total_groups <- 5
    (left_proportion <- (sexsize-min(data, na.rm = T))/(max(data, na.rm = T)-min(data, na.rm = T)))
    (left_proportion <- ifelse(left_proportion < 0, 0, left_proportion))
    (right_proportion <- (max(data, na.rm = T)-sexsize)/(max(data, na.rm = T)-min(data, na.rm = T))) 
    (right_proportion <- ifelse(right_proportion > 1, 1, right_proportion))
    
    (left_groups <- round(left_proportion * total_groups))
    (right_groups <- total_groups - left_groups)
    
    # Define breaks for left side
    (if (length(data_left) > 0) {
      left_breaks <- seq(min(data_left), sexsize, length.out = left_groups + 1)
    } else {
      left_breaks <- NULL
    })
    
    # Define breaks for right side
    (if (length(data_right) > 0) {
      right_breaks <- seq(sexsize, max(data_right), length.out = right_groups + 1)
    } else {
      right_breaks <- NULL
    })
    
    # Combine breaks
    (breaks <- unique(c(left_breaks, right_breaks)))
    (breaks <- round(breaks))
    
    df.i$tail_cm <- round(df.i$tail_cm, digits = 0)
    
    s.cl <- breaks
    (s.cl.range <- c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
    df.i$tail_cm_classe <- NA
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[2], paste0("[",s.cl[1],"-",s.cl[2],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[3] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[2],"-",s.cl[3],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[4] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[3],"-",s.cl[4],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[5] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[4],"-",s.cl[5],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm <= s.cl[6] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[5],"-",s.cl[6],"]"), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- as.factor(df.i$tail_cm_classe)
    
    as.character(na.omit(unique(df.i$tail_cm_classe)))
    setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))
    ifelse(length(setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))) >=1, df.i <- add_row(df.i, tail_cm_classe = setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))), df.i)
    df.i$tail_cm_classe <- factor(df.i$tail_cm_classe, levels = c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
    levels(df.i$tail_cm_classe)
    
    #df.i <- na.omit(df.i[,c("tail_cm", "tail_cm_classe")]) 
    #levels(df.i$tail_cm_classe)
    df.i$tail_cm_classe.num <- NA
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[1],"-",s.cl[2],"["), 1, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[2],"-",s.cl[3],"["), 2, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[3],"-",s.cl[4],"["), 3, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[4],"-",s.cl[5],"["), 4, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[5],"-",s.cl[6],"]"), 5, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- as.integer(df.i$tail_cm_classe.num)
    (df.i$tail_cm_classe.num)
    
    (txt. <- paste0("[",sexsize,"-"))
    (threshold_category <- unlist(unique((df.i[grep(paste0("\\",txt.), df.i$tail_cm_classe), ])$tail_cm_classe.num)))
    df.i$catch <- NA
    df.i$catch <- ifelse(df.i$tail_cm_classe.num < threshold_category, "non", "oui")
    df.i$catch <- as.factor(df.i$catch)
    df.i.narm <- na.omit(df.i[, c("tail_cm", "tail_cm_classe", "tail_cm_classe.num", "catch")])
    print(table(df.i.narm[, c("tail_cm_classe", "tail_cm_classe.num", "catch")]))
    df.i.narm %>% group_by(tail_cm_classe, catch) %>% summarise(n())
    
    barplot.i <- ggplot(df.i.narm, aes(x = tail_cm_classe, fill = catch)) +
      geom_bar() +
      scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values = c("non" = "orange", "oui" = "lightblue")) +
      labs(title = paste0(unique(df.i$nom_scien), " (N = ", nrow(df.i.narm), ", ", years., ")"),
           x = "Taille (cm)",
           y = "Nombre d'individus",
           fill = "Maille") +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      scale_y_continuous(expand = c(0,0))
    barplot.i <- barplot.i + ggtitle(bquote(italic(.(unique(df.i$nom_scien)))~.(years.)~"(N ="~.(nrow(df.i.narm))*")"))
    print(barplot.i)
    ggsave(paste0("Figs/Maille_nb_sp/", unique(na.omit(df.i$nom_scien)), "_", years.,"_barplot.png"), barplot.i, width = 5, height = 4)
    
  }
  
}

maille.sp.(df.fsh. = df.fsh.red, df.size = size.catch, df. = df_sum, years. = "2020-2024")
maille.sp.(df.fsh. = df.fsh.red.1, df.size = size.catch, df. = df_sum.1, years. = "2020-2022")
maille.sp.(df.fsh. = df.fsh.red.2, df.size = size.catch, df. = df_sum.2, years. = "2023-2024")

# Animated ggplot for fish maille size

df_sum.a <- df.fsh.red %>%
  group_by(nom_scien, a) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh.red$nb_ind, na.rm = TRUE)))*100, 
            .groups = 'drop')
df_sum.a <-df_sum.a[order(df_sum.a$total_nb_ind, decreasing = T),]

#df.anim. <- filter(df.fsh.red, nom_scien %in% size.catch[,"species"])
#df.2020 <- filter(df.anim., a == "2020")

df.fsh.red$a <- factor(df.fsh.red$a, levels = c("2020", "2021", "2022", "2023", "2024"))

  df.fsh.=df.fsh.red
  df.=df_sum.a
  df.size=size.catch
  
  for(i in c(1:nrow(df.size))) {
    
    #i=4
    
    df.i <- filter(df.fsh., nom_scien == df.size$species[i])
    print(sp. <- unclass(unique(df.i$nom_scien)))
    names(df.i)
    #df.i <- df.i[, c("nom_scien", "nb_ind", "tail_cm")] #do not consider var "poids_g" cfr NAs ; can also remove year cfr not needed in the analysis below
    (sexsize=(filter(df.size,species==sp.))["size"]$size)
    
    # Remove NAs prior to use the rep function
    nrow(df.i)
    nrow(na.omit(df.i))
    df.i <- na.omit(df.i)
    # Use the rep function to repeat rows
    df.i <- df.i[rep(1:nrow(df.i), df.i$nb_ind), ]
    df.i$nb_ind <- 1
    df.i <- data.frame(df.i)
    
    for (j in 1:length(unique(df.fsh.$a))) {
    
      #j=3
      df.fsh.a <- filter(df.fsh., a == sort(unique(df.fsh.$a))[j])
      (vec.size <- unlist(filter(df.fsh.a, nom_scien == sp.)[,"tail_cm"]))
      
      if (length(vec.size) == 0) {
        plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
             main = paste0(sp., " ", sort(unique(df.fsh.$a))[j]), xlab = "size (cm)", ylab = "Frequency", bty = "l", axes = FALSE)
        # Manually add only the desired axes
        axis(1, at = seq(0, 1, by = 0.2))  # Custom x-axis with ticks
        axis(2, at = seq(0, 1, by = 0.2))  # Custom y-axis with ticks
      } else {
        # Normal histogram plot when data is available
        hist(vec.size, xlab = "size (cm)", main = paste0(sp., " ", sort(unique(df.fsh.$a))[j]))
      }
      
      }
      
    (data <- df.i$tail_cm)
    (border_value <- sexsize)
    
    # Split data into two parts based on the border value
    (data_left <- na.omit(data[data < border_value]))
    (data_right <- na.omit(data[data >= border_value]))
    
    # Determine the number of groups on each side
    total_groups <- 5
    (left_proportion <- (sexsize-min(data, na.rm = T))/(max(data, na.rm = T)-min(data, na.rm = T)))
    (left_proportion <- ifelse(left_proportion < 0, 0, left_proportion))
    (right_proportion <- (max(data, na.rm = T)-sexsize)/(max(data, na.rm = T)-min(data, na.rm = T))) 
    (right_proportion <- ifelse(right_proportion > 1, 1, right_proportion))
    
    (left_groups <- round(left_proportion * total_groups))
    (right_groups <- total_groups - left_groups)
    
    # Define breaks for left side
    (if (length(data_left) > 0) {
      left_breaks <- seq(min(data_left), sexsize, length.out = left_groups + 1)
    } else {
      left_breaks <- NULL
    })
    
    # Define breaks for right side
    (if (length(data_right) > 0) {
      right_breaks <- seq(sexsize, max(data_right), length.out = right_groups + 1)
    } else {
      right_breaks <- NULL
    })
    
    # Combine breaks
    (breaks <- unique(c(left_breaks, right_breaks)))
    (breaks <- round(breaks))
    
    s.cl <- breaks
    (s.cl.range <- c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
    df.i$tail_cm_classe <- NA
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[2], paste0("[",s.cl[1],"-",s.cl[2],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[3] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[2],"-",s.cl[3],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[4] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[3],"-",s.cl[4],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[5] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[4],"-",s.cl[5],"["), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- ifelse(df.i$tail_cm <= s.cl[6] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[5],"-",s.cl[6],"]"), df.i$tail_cm_classe)
    df.i$tail_cm_classe <- as.factor(df.i$tail_cm_classe)
    
    as.character(na.omit(unique(df.i$tail_cm_classe)))
    setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))
    ifelse(length(setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))) >=1, df.i <- add_row(df.i, tail_cm_classe = setdiff(s.cl.range, as.character(na.omit(unique(df.i$tail_cm_classe))))), df.i)
    df.i$tail_cm_classe <- factor(df.i$tail_cm_classe, levels = c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
    levels(df.i$tail_cm_classe)
    
    #df.i <- na.omit(df.i[,c("tail_cm", "tail_cm_classe")]) 
    #levels(df.i$tail_cm_classe)
    df.i$tail_cm_classe.num <- NA
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[1],"-",s.cl[2],"["), 1, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[2],"-",s.cl[3],"["), 2, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[3],"-",s.cl[4],"["), 3, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[4],"-",s.cl[5],"["), 4, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[5],"-",s.cl[6],"]"), 5, df.i$tail_cm_classe.num)
    df.i$tail_cm_classe.num <- as.integer(df.i$tail_cm_classe.num)
    (df.i$tail_cm_classe.num)
    
    (txt. <- paste0("[",sexsize,"-"))
    (threshold_category <- unlist(unique((df.i[grep(paste0("\\",txt.), df.i$tail_cm_classe), ])$tail_cm_classe.num)))
    df.i$catch <- NA
    df.i$catch <- ifelse(df.i$tail_cm_classe.num < threshold_category, "non", "oui")
    df.i$catch <- as.factor(df.i$catch)
    df.i.narm <- na.omit(df.i[, c("a", "tail_cm", "tail_cm_classe", "tail_cm_classe.num", "catch")])
    print(table(df.i.narm[, c("tail_cm_classe", "tail_cm_classe.num", "catch")]))
    df.i.narm %>% group_by(a, tail_cm_classe) %>% summarize(nb = n()) -> nb.catch
    
    for (j in 1:length(unique(df.fsh.$a))) {
      
    #j=1
    df.i.narm.a <- filter(df.i.narm, a == sort(unique(df.fsh.$a))[j])
    (df.i.narm.a %>% group_by(tail_cm_classe, catch) %>% summarise(n()) -> stats)
    sum(stats$`n()`, na.rm = T) -> N
    
    if (nrow(df.i.narm.a) == 0) {
      
      df.i.narm.a <- rbind(df.i.narm.a, data.frame(a = rep(sort(unique(df.fsh.$a))[j], length(unique(df.fsh.$a))),
                                  tail_cm = rep(NA, length(unique(df.fsh.$a))),
                                  tail_cm_classe = unique(df.i.narm$tail_cm_classe),
                                  tail_cm_classe.num = unique(df.i.narm$tail_cm_classe.num),
                                  catch = rep(#c("non", "oui"),
                                    NA, length.out= (length(unique(df.fsh.$a))))
                                    #rep(NA, length(unique(df.fsh.$a)))
                                  ))
      df.i.narm.a <- arrange(df.i.narm.a, tail_cm_classe.num)
      df.i.narm.a$tail_cm_classe <- factor(as.character(df.i.narm.a$tail_cm_classe), levels = as.character(df.i.narm.a$tail_cm_classe))
      
      barplot.i <- ggplot(df.i.narm.a, aes(x = tail_cm_classe, fill = catch)) +
        geom_bar(alpha = 0, color = NA) +
        #ylim(0, max(nb.catch$nb, na.rm = T)) +
        scale_x_discrete(drop = FALSE) +
        scale_fill_manual(values = c("non" = "orange", "oui" = "lightblue")) +
       labs(title = paste0(unique(df.i$nom_scien), " (N = 0, ", sort(unique(df.fsh.$a))[j], ")"),
             x = "Taille (cm)",
             y = "Nombre d'individus",
             fill = "Maille") +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank()) +
        theme(legend.text = element_blank(),          
              legend.title = element_blank(),          
              legend.key = element_blank()) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,max(nb.catch$nb, na.rm = T)))
        } else {
    barplot.i <- ggplot(df.i.narm.a, aes(x = tail_cm_classe, fill = catch)) +
      geom_bar() +
      #ylim(0, max(nb.catch$nb, na.rm = T)) +
      scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values = c("non" = "orange", "oui" = "lightblue")) +
      labs(title = paste0(unique(df.i$nom_scien), " (N = ", nrow(df.i.narm.a), ", ", sort(unique(df.fsh.$a))[j], ")"),
           x = "Taille (cm)",
           y = "Nombre d'individus",
           fill = "Maille") +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,max(nb.catch$nb, na.rm = T)))
        }
    
    barplot.i <<- barplot.i + ggtitle(bquote(italic(.(unique(df.i$nom_scien)))~.(as.character(sort(unique(df.fsh.$a))[j]))~"(N ="~.(N)*")"))
    print(barplot.i)
    
    ggsave(paste0("Figs/Maille_animated/ind.plots/", unique(df.i$nom_scien), "_", as.character(sort(unique(df.fsh.$a))[j]),"_barplot.png"), barplot.i, width = 5, height = 4)
    
    assign(paste0("barplot.", j), barplot.i)
    rm(barplot.i)
    
    }
    
    ## Dynamic ggplot
    
    # Load required libraries
    library(ggplot2)
    library(dplyr)
    library(magick)
    library(gganimate)
    
    # Assuming that the original code block has been executed and barplots have been created and assigned
    # Save each barplot as an image file
    plot_names <- ls(pattern = "barplot\\.\\d+")  # List all barplot objects in the environment
    file_names <- paste0(plot_names, ".png")  # Create corresponding filenames
    
    # Save each barplot as a PNG
    for (i in seq_along(plot_names)) {
      ggsave(file_names[i], plot = get(plot_names[i]), device = "png", width = 4, height = 6)
    }
    
    # Read the saved images into a magick image object
    img_list <- image_read(file_names)
    # Create an animation using magick
    animation <- image_animate(img_list, fps = 0.5)  # 1 frame per second (adjust `fps` as needed)
    # Save the final animation
    image_write(animation, "barplot_animation_magick.gif")
    image_write(animation, paste0("Figs/Maille_animated/barplot_animation_magick_", unique(df.i$nom_scien), ".gif"))
                
    # Clean up temporary files
    file.remove(file_names)
    
    # Step 1: Read the existing animated GIF file
    animation <- image_read("barplot_animation_magick.gif")

    # Step 2: Create smooth transitions using `image_morph()`
    smooth_animation <- image_morph(animation, frames = 10)  # Add intermediate frames for smooth transitions

    # Step 3: Manually duplicate the final frame of each transition to create a pause effect
    # Get the total number of frames in the smooth animation
    num_frames <- length(smooth_animation)

    # Create an empty list to store the frames with pauses
    final_frames <- list()

    # Define the length of the pause (adjust this value to increase the pause duration)
    pause_length <- 30  # Number of duplicate frames for the pause between images

    # Step 4: Add a pause at the beginning (before the first frame transition)
    initial_frame <- smooth_animation[1]  # Select the very first frame
    initial_pause <- image_join(replicate(pause_length, initial_frame, simplify = FALSE))  # Create initial pause
    final_frames[[length(final_frames) + 1]] <- initial_pause

    # Step 5: Step through each original frame index (increment by 10 because we added 10 smooth frames)
    for (i in seq(1, num_frames, by = 10)) {
    # Determine the end index of the current smooth sequence
    end_index <- min(i + 9, num_frames)  # Make sure we don't go out of bounds

    # Append the current sequence of smooth frames
    final_frames[[length(final_frames) + 1]] <- smooth_animation[i:end_index]

    # Manually duplicate the final frame to create a longer pause between frames
    pause_frame <- smooth_animation[end_index]  # Last frame in this transition sequence
    pause_frames <- image_join(replicate(pause_length, pause_frame, simplify = FALSE))  # Create multiple copies for a pause
    final_frames[[length(final_frames) + 1]] <- pause_frames
}

    # Step 6: Combine all frames into a single image object
    final_animation <- image_join(do.call(c, final_frames))

    # Step 7: Animate with a defined frame rate
    final_gif <- image_animate(final_animation, fps = 10)  # Adjust fps to control speed

    # Step 8: Save the final GIF with the desired filename
    image_write(final_gif, paste0("Figs/Maille_animated/smooth_barplot_with_pause_", unique(df.i$nom_scien), ".gif"))

    # Step 7: Clean up temporary files
    #file.remove(dynamic_filenames)
    #file.remove(static_filenames)
  
    files_to_remove <- list.files(pattern = "barplot", full.names = TRUE)
    print(paste("Deleting files:", files_to_remove))
    file.remove(files_to_remove)
    
    gc()
    
    # Now do another approach, with a new dataframe, to try to use the gganimate pachage for image transition
    
    df.i.narm
    nb.catch
    df.i.narm %>% group_by(a, tail_cm_classe, catch) %>% summarize(nb = n()) -> nb.catch
    nb.catch
    # Define the full range of years and tail_cm_classes
    all_years <- 2020:2024
    all_tail_classes <- unique(df.i.narm$tail_cm_classe[order(df.i.narm$tail_cm_classe)])
    complete_df <- expand.grid(a = all_years, tail_cm_classe = all_tail_classes)
    complete_df$a <- as.factor(complete_df$a)
    complete_df$tail_cm_classe <- as.factor(complete_df$tail_cm_classe)
    complete_df <- data.frame(complete_df)
    nb.catch <- data.frame(nb.catch)
    # Merge with the original data and fill missing `nb` values with NA
    nb.catch.cpl <- complete_df %>%
      left_join(nb.catch, by = c("a", "tail_cm_classe")) %>%
      arrange(a, tail_cm_classe)
    nb.catch.cpl
    nb.catch.cpl$catch
    
    # Step 1: Calculate the total sum of `nb` for each year
    # Assuming `a` represents the year in your dataset
    nb.catch.cpl <- nb.catch.cpl %>%
      group_by(a, tail_cm_classe, catch) %>%
      summarize(nb = sum(nb, na.rm = TRUE)) %>%
      ungroup()
    
    # Create a summary dataframe to get the total `nb` for each year
    yearly_total <- nb.catch.cpl %>%
      group_by(a) %>%
      summarize(total_nb = sum(nb, na.rm = TRUE))
    
    # Step 2: Merge the total count back to the original dataframe for reference in the animation
    nb.catch.cpl <- left_join(nb.catch.cpl, yearly_total, by = "a")
    sp. <- unique(df.i$nom_scien)
    nb.catch.cpl <- nb.catch.cpl %>%
      mutate(dynamic_label = paste0(unique(df.i$nom_scien), " (N = ", total_nb, ", ", a, ")"))
    nb.catch.cpl$dynamic_label <- factor(nb.catch.cpl$dynamic_label, levels = unique(nb.catch.cpl$dynamic_label))
    levels(nb.catch.cpl$dynamic_label)
    
    ggplot(nb.catch.cpl, aes(x=tail_cm_classe , y=nb, fill=catch )) + 
      geom_bar(stat='identity') +
      scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values = c("non" = "orange", "oui" = "lightblue"), na.value = "transparent") +
      labs(
        x = "Taille (cm)",
        y = "Nombre d'individus",
        fill = "Maille"
      ) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(size = 20),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text = element_text(size = 14)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,max(nb.catch$nb, na.rm = T))) +
      # gganimate specific bits:
      transition_states(dynamic_label,
                        transition_length = 3,
                        state_length = 1, 
                        wrap = TRUE) +
      ggtitle("{closest_state}")
    
    # Save as gif
    anim_save(paste0("Figs/Maille_animated/barplot_gganimate_", unique(df.i$nom_scien), ".gif"))
    
    # Clean up temporary files
    #file.remove(dynamic_filenames)
    #file.remove(static_filenames)
    
    # I have to find a way to automatically remove the dynamic plot
    graphics.off() 
    gc()

}       
  
rm(list=setdiff(ls(), c("fishing", 
                        "fishing.new",
                        "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df.0", "results_df.1", "results_df.2")))


# plot CPUE ####

# Les captures par unités d’effort (CPUE) par techniques de pêches ont été exprimées en g.jour.pêcheur -1.
# Actually, CPUE in the db is calculated as poids_g/1.5/nb_pecheur ; so this is not the same unit actually ...?!

#NB: remove urchins cfr the only row is:
#1 2022  peche à l'oursin Labrus merula  97.5

df. <- fishing[, c("a", "mod_peche", "nom_scien", "CPUE")]
unique(df.$a)
table(df.$a)
unique(df.$mod_peche)
table(df.$mod_peche)
filter(df., mod_peche != "po") -> df.

ggplot(filter(df., a %in% c("2020", "2021", "2022", "2023", "2024")), aes(x = mod_peche, y = CPUE, fill = mod_peche)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot()

table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
#df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée"
                                                  #, "peche à l'oursin"
                                                  ))
table(df.$mod_peche) 

df. %>% filter(a %in% c("2020","2021","2022", "2023", "2024")) -> data.1
data.1[sapply(data.1, is.infinite)] <- NA

ggplot(filter(data.1, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif (h) / nb pêcheurs)") +
  ggtitle("2020-2024") +
  ylim(0,max(data.1$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  #geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.1, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(500), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2020-2024.png", width = 5, height = 4)

p. <- ggplot(filter(data.1, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot(outliers = F)
  p. +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif (h) / nb pêcheurs)") +
  ggtitle("2020-2024") +
  #ylim(0,max(boxplot(CPUE~mod_peche, data.1)$stats[5,], na.rm = T)) +
  ylim(0,max(layer_data(p.)[,"ymax"])) +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") #+
  #geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.1, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red")
ggsave("Figs/CPUE/CPUE.out_2020-2024.png", width = 5, height = 4)

df. %>% filter(a %in% c("2020","2021","2022")) -> data.2
data.2[sapply(data.2, is.infinite)] <- NA

ggplot(filter(data.2, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif / nb pêcheurs)") +
  ggtitle("2020-2022") +
  ylim(0,max(data.2$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  #geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.2, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(500), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2020-2022.png", width = 5, height = 4)

p. <- ggplot(filter(data.2, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot(outliers = F)
  p. +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif (h) / nb pêcheurs)") +
  ggtitle("2020-2022") +
  #ylim(0,max(boxplot(CPUE~mod_peche, data.2)$stats[5,], na.rm = T)) +
  ylim(0,max(layer_data(p.)[,"ymax"])) +
  geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") #+
  #geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.2, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red")
ggsave("Figs/CPUE/CPUE.out_2020-2022.png", width = 5, height = 4)

df. %>% filter(a %in% c("2023", "2024")) -> data.3
data.3[sapply(data.3, is.infinite)] <- NA

ggplot(filter(data.3, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif / nb pêcheurs)") +
  ggtitle("2023-2024") +
  ylim(0,max(data.3$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  #geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.3, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(500), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2023-2024.png", width = 5, height = 4)

p. <- ggplot(filter(data.3, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot(outliers = F) 
  p. +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / tps pêche effectif (h) / nb pêcheurs)") +
  ggtitle("2023-2024") +
  #ylim(0,max(boxplot(CPUE~mod_peche, data.3)$stats[5,], na.rm = T)) +
  ylim(0,max(layer_data(p.)[,"ymax"])) +
  geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") #+
  #geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
  #           mapping = aes(x = mod_peche, y = mean(unclass((filter(data.3, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red")
ggsave("Figs/CPUE/CPUE.out_2023-2024.png", width = 5, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df.0", "results_df.1", "results_df.2")))


# answer to PNMCA BD questions cfr Aubin ####

# internet ressources: https://www.dirm.mediterranee.developpement-durable.gouv.fr/peche-de-loisir-r18.html

# Proportion de pêcheurs de loisirs connaissant la réglementation du Parc (panier familial, autorisation de pêche, zones de jachère) par rapport à l'échantillon de pêcheurs enquêtés
# Proportion de pêcheurs de loisirs respectant la réglementation (panier familial, autorisation de pêche, zone de jachère) par rapport à l'échantillon de pêcheurs enquêtés

# Proportion de pêcheurs de loisirs connaissant la réglementation générale (maillage, espèces pêchées, zones et périodes de pêche) par rapport à l'échantillon de pêcheurs enquétés
# Proportion de pêcheurs de loisirs respectant la réglementation (maillage, espèces pêchées, zones et périodes de pêche) par rapport à l'échantillon de pêcheurs enquêtés

survey <- readRDS("Raw data_mined/enquetes.rds")
fishing <- readRDS("Raw data_mined/terrain.rds")

# variables of interest in fishing db (but for PNMCA data only, not for Stareso data)
# infraction : Type d'infraction sur l'espèce renseignée
# infraction_nb :	Nombre de poissons concernés par l'infraction

head(fishing$infraction)
unique(fishing$infraction)
table(fishing$infraction)
table(fishing[, c("BD", "a", "infraction")])
table(fishing[, c("a", "infraction")])
# "NA"= pas de donnee, 
# "non"= pas d'infration, 
# "maille"= maille non respectée,
# "min_cons"= maille conseille non respectée, 
# "queue"= queue non coupee, 
# "esp"= espece interdite, 
# "quantite"= quantite limite depassee, 
# "periode"= periode non respectee
fishing$infraction <- ifelse(fishing$infraction == "NA", NA, fishing$infraction)
fishing$infraction <- as.factor(fishing$infraction)

fishing %>%
  group_by(BD) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

fishing %>%
  group_by(BD, infraction) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) -> infraction
add_column(infraction, perc = round(infraction$count/sum(infraction$count), digits = 3)) -> infraction
add_column(infraction, perc_na.omit = round(infraction$count/sum(filter(infraction, infraction != "NA")[,"count"]), digits = 3)) -> infraction
ifelse(infraction$infraction == "NA", NA, infraction$perc_na.omit) -> infraction$perc_na.omit
infraction

head(fishing$infraction_nb)
unique(fishing$infraction_nb)
fishing$infraction_nb <- ifelse(fishing$infraction_nb == "NA", NA, fishing$infraction_nb)
fishing$infraction_nb <- as.factor(fishing$infraction_nb)

fishing %>%
  group_by(BD, infraction_nb) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

#filter(fishing, BD != "Stareso") -> fishing.PNMCA 
infraction_type.vs.nb <- 
  #fishing.PNMCA 
  fishing %>%
  group_by(BD, infraction, infraction_nb) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(infraction)
unique(infraction$infraction)
ord <- c("NA", "non", "min_cons","maille", "queue")
infraction_type.vs.nb$infraction <- factor(infraction_type.vs.nb$infraction, levels = ord)
arrange(infraction_type.vs.nb, infraction) -> infraction_type.vs.nb
infraction_type.vs.nb[order(infraction_type.vs.nb$infraction, as.numeric(as.character(infraction_type.vs.nb$infraction_nb))),] -> infraction_type.vs.nb
add_column(infraction_type.vs.nb, perc = round(infraction_type.vs.nb$count/sum(infraction_type.vs.nb$count), digits = 3)) -> infraction_type.vs.nb
add_column(infraction_type.vs.nb, perc_na.omit = round(infraction_type.vs.nb$count/sum(filter(infraction_type.vs.nb, infraction != "NA")[,"count"]), digits = 3)) -> infraction_type.vs.nb
ifelse(infraction_type.vs.nb$infraction == "NA", NA, infraction_type.vs.nb$perc_na.omit) -> infraction_type.vs.nb$perc_na.omit
infraction_type.vs.nb

# variables of interest in survey db (NB: urchin related variables can be omitted cfr only two observations concerning urchin fishing ; NB only data from PNMCA in survey db)
#reg_pech_l	Connaissance de la réglementation de la pêche de loisir 
#reg_pech_o	Connaissance de la réglementation de la pêche à l'oursin
#sais_pech_o	Connaissance de la saison de la pêche à l'oursin
#tail_min_o	Connaissance de la taille minimale de la pêche à l'oursin
#quot_max_o	Le pêcheur est informé du qota maximal d'oursins autorisé
#inf_pech_l	La réglementation est-elle assez bien communiquée dans le communauté des pêcheurs

#reg_pech_l, in database	: "oui","non"
#reg_pech_l, in reality closer to inf_pech_l values	: 
# non pas du tout = "non_pdt"
# non pas vraiment = "non_pv"
# oui un peu = "oui_peu"
# oui tout à fait = "oui_taf"

survey %>%
  group_by(BD) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))
# no data from Stareso in survey db

head(survey$reg_pech_l)	
unique(survey$reg_pech_l)
ifelse(survey$reg_pech_l == "NA", NA, survey$reg_pech_l) -> survey$reg_pech_l
table(survey$reg_pech_l)
table(survey[, c("a", "reg_pech_l")])

head(survey$inf_pech_l)
unique(survey$inf_pech_l)
ifelse(survey$inf_pech_l == "NA", NA, survey$inf_pech_l) -> survey$inf_pech_l
table(survey$inf_pech_l)
table(survey[, c("a", "inf_pech_l")])
ifelse(survey$inf_pech_l == "npv", "non_pv", survey$inf_pech_l) -> survey$inf_pech_l
ifelse(survey$inf_pech_l == "n_s_p", NA, survey$inf_pech_l) -> survey$inf_pech_l
table(survey[, c("a", "inf_pech_l")])

# no PNMCA data for years 2023 & 2024

survey$reg_pech_l <- as.factor(survey$reg_pech_l)

survey %>%
  group_by(BD, reg_pech_l) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) -> regl
add_column(regl, perc = round(regl$count/sum(regl$count), digits = 3)) -> regl
add_column(regl, perc_na.omit = round(regl$count/sum(filter(regl, reg_pech_l != "NA")[,"count"]), digits = 3)) -> regl
ifelse(regl$reg_pech_l == "NA", NA, regl$perc_na.omit) -> regl$perc_na.omit
regl

survey$inf_pech_l <- as.factor(survey$inf_pech_l)

survey %>%
  group_by(BD, inf_pech_l) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) -> info
add_column(info, perc = round(info$count/sum(info$count), digits = 3)) -> info
add_column(info, perc_na.omit = round(regl$count/sum(filter(info, inf_pech_l != "NA")[,"count"]), digits = 3)) -> info
ifelse(info$inf_pech_l == "NA", NA, info$perc_na.omit) -> info$perc_na.omit
regl

