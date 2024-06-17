
#options(warn=-1) #remove warnings
options(warn=0) #print warnings

#Collapse — Alt+L
#Expand — Shift+Alt+L
#Collapse All — Alt+O
#Expand All — Shift+Alt+O

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
Meta.survey$Format <- ifelse(Meta.survey$Format == "Nombre", "character", Meta.survey$Format) #was numeric before, but some var supposed to be numric must be imported as character cfr e.g. age variable
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

fishing.Stareso <- read_excel("Enquetes_Pêche_Loisir_PNMCCA_Chabrier_2023 (1).xlsx", sheet = "BD_Stareso", col_names = T, col_types = c(
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
fishing <- bind_rows(fishing.PNMCA, fishing.Stareso)

fishing <- fishing[, c(c(intersect(names(fishing.Stareso), names(fishing.PNMCA))), c(setdiff(names(fishing.PNMCA), names(fishing.Stareso))), c(setdiff(names(fishing.Stareso), names(fishing.PNMCA))))]

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

saveRDS(survey, "Raw data_mined/enquetes.rds")
write.csv2(survey, "Raw data_mined/enquetes.csv")
saveRDS(fishing, "Raw data_mined/terrain.rds")
write.csv2(survey, "Raw data_mined/terrain.csv")


# update plot & analysis report 2022


# plot fishers vs sex  ####

unique(survey$sexe)
unique(survey$a)
df. <- na.omit(survey[, c("a", "sexe")])
unique(df.$a)

df. %>%  filter(a %in% c("2020","2021","2022", "2023")) %>% group_by(sexe) %>% count() -> data
data <- data.frame(data)
data$fraction = (data$n/sum(data$n))*100
data

ggplot(data, aes(x="", y=fraction, fill=sexe)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2023 (n=", sum(data$n), ")")) +
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

ggsave("Figs/Sex/sex_2020-2023.png", width = 5, height = 4)

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

df. %>%  filter(a %in% c("2023")) %>% group_by(sexe) %>% count() -> data.2
data.2 <- data.frame(data.2)
data.2$fraction = (data.2$n/sum(data.2$n))*100
data.2

ggplot(data.2, aes(x="", y=fraction, fill=sexe)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2023 (n=", sum(data.2$n), ")")) +
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

ggsave("Figs/Sex/sex_2023.png", width = 5, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# plot age pyramid by sex ####

unique(survey$sexe)
unique(survey$a)
unique(survey$age_moyen)
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
df. %>% tabyl(age_classe, sexe, a) #we have no info about age_moyen in 2023 so not possible to plot the age pyramid!

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

df. %>% filter(mod_peche != "pdb, csm") -> df.
table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée", "peche à l'oursin"))

table(df.$mod_peche) #we have no info about age_moyen in 2023 so not possible to plot the age pyramid!

df. %>% filter(a %in% c("2020","2021","2022")) -> data.1

ggplot(filter(data.1, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=age_moyen)) + 
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

df. %>%  filter(a %in% c("2020","2021","2022", "2023")) %>% group_by(mod_peche) %>% count() -> data
data <- data.frame(data)
data$fraction = (data$n/sum(data$n))*100
data
names(data) <- c("technique", "n", "fraction")

ggplot(data, aes(x="", y=n, fill=technique)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2020-2023 (n=", sum(data$n,na.rm = T), ")")) +
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
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))

ggsave("Figs/Technique.peche/tech_2020-2023.png", width = 7, height = 4)

df. %>%  filter(a %in% c("2020","2021","2022")) %>% group_by(mod_peche) %>% count() -> data.1
data.1 <- data.frame(data.1)
data.1$fraction = (data.1$n/sum(data.1$n))*100
data.1
names(data.1) <- c("technique", "n", "fraction")

ggplot(data.1, aes(x="", y=n, fill=technique)) +
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
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))

ggsave("Figs/Technique.peche/tech_2020-2022.png", width = 7, height = 4)

df. %>%  filter(a %in% c("2023")) %>% group_by(mod_peche) %>% count() -> data.2
data.2 <- data.frame(data.2)
data.2$fraction = (data.2$n/sum(data.2$n))*100
data.2
names(data.2) <- c("technique", "n", "fraction")

ggplot(data.2, aes(x="", y=n, fill=technique)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle(paste0("2023 (n=", sum(data.2$n,na.rm = T), ")")) +
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
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))

ggsave("Figs/Technique.peche/tech_2023.png", width = 7, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%")))


# prepare df. to perform multivariate analysis ####

# in below multivariate analysis, I have included all the variables of the survey df used in Michel 2022 report, among them those of the "AFM" (majority) but also those of the summary table and associated Kiviat diagram. For many varaibles, there are no data for year 2023 !
# => in 2O23: raw. var.: mod_peche, res_tour, sexe, Temps_peche_effectif, Temps_peche_estime, zone & zone.gp ("z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61")
# => not in 2023: raw var.: age_moyen (& age_classe), cat_pro (& cat_pro.gp), nb_sort_an (& nb_sort_an_cat), avis_pnm, poss_bat, dep (& dep.median, dep.mod.peche, etc) ; calculated var. : time.week ("we", "sem"), time.day & time.day.gp ("aube", "matin", "soir", "crepuscule", "nuit", "apm", "journee", "nimp_q")

unique(survey$mod_peche); length(na.omit(survey$mod_peche)) ; table(survey$mod_peche) ; table(survey$a, survey$mod_peche) 
unique(survey$age_moyen); length(na.omit(survey$age_moyen)) ; table(survey$age_moyen) ; table(survey$a, survey$age_moyen) #no data for 2023
#profil ?? #not possible to find out what is the "profil" group of/unique variables used by Michel in the MFA!
unique(survey$act_pro); length(na.omit(survey$act_pro)) ; table(survey$act_pro) ; table(survey$a, survey$act_pro) #no sense to many categories, or merge act_pro ; no data for 2023; not considered in the analysis.
unique(survey$cat_pro); length(na.omit(survey$cat_pro)) ; table(survey$cat_pro) ; table(survey$a, survey$cat_pro) #no data for 2023
unique(survey$nb_sort_an); length(na.omit(survey$nb_sort_an)) ; table(survey$nb_sort_an) ; table(survey$a, survey$nb_sort_an) #no data for 2023
unique(survey$avis_pnm); length(na.omit(survey$avis_pnm)) ; table(survey$avis_pnm) ; table(survey$a, survey$avis_pnm) #no data for 2023
unique(survey$res_tour); length(na.omit(survey$res_tour)) ; table(survey$res_tour) ; table(survey$a, survey$res_tour)
unique(survey$sexe); length(na.omit(survey$sexe)) ; table(survey$sexe) ; table(survey$a, survey$sexe) #no data for 2023
unique(survey$poss_bat); length(na.omit(survey$poss_bat)) ; table(survey$poss_bat) ; table(survey$a, survey$poss_bat) 
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
#we have no info about age_moyen in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis

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
unique(filter(survey[,c("a", "act_pro")], "a" == "2023"))
#we have no info about cat_pro in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

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
unique(filter(survey[,c("a", "nb_sort_an")], "a" == "2023"))
#we have no info about nb_sort_an in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

unique(df.$nb_sort_an); length(na.omit(df.$nb_sort_an)) ; table(df.$nb_sort_an) ; table(df.$a, df.$nb_sort_an) #no data for 2023
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
unique(df.$nb_sort_an_cat); length(na.omit(df.$nb_sort_an_cat)) ; table(df.$nb_sort_an_cat) ; table(df.$a, df.$nb_sort_an_cat) #no data for 2023

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
nrow(filter(df., a %in% c("2023")))


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

#There are variables with n ~ 517, when surveyed in 2023, and variables with n ~ 260, when only surveyed in 2020-2022, but not 2023. So we cannot do the full multivariate spatial analysis with all the variables for 2023 ; and for espece cible, even less data ...


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

#2020-2023
df.red <- na.omit(filter(df.msa[, c("a",
                          "mod_peche",
                          "sexe",
                          "res_tour",
                          "Temps_peche_estime.cat2h",
                          "zone.gp")
], a %in% c("2020", "2021", "2022", "2023")))
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
# color individuals using multiple categorical variables
fviz_ellipses(res.mca, c("a", "mod_peche"),
              geom = "point",
              )
ggsave("Figs/MCA/factor.map_2020-2022.png", width = 10, height = 5)
# Dimension description: to identify the most correlated variables with a given dimension
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

# df.short 2020-2023
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
ggsave("Figs/MCA/variables_2020-2023.png", width = 7, height = 4, bg = "white")
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
# color individuals using multiple categorical variables
fviz_ellipses(res.mca, c("a", "mod_peche"),
              geom = "point")
ggsave("Figs/MCA/factor.map_2020-2023.png", width = 10, height = 5)
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
#df.mod %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> de.mod

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

unique(df.mod$nb_sort_an); length(na.omit(df.mod$nb_sort_an)) ; table(df.mod$nb_sort_an) ; table(df.mod$a, df.mod$nb_sort_an) #no data for 2023
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
unique(df.mod$dep.mod_peche); length(na.omit(df.mod$dep.mod_peche)) ; table(df.mod$dep.mod_peche) ; table(df.mod$a, df.mod$dep.mod_peche) #no data for 2023

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
filter(df.mod, a != "2024") -> df.mod
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

multinom_model.short.sign <- multinom(mod_peche ~ . - res_tour, data = df.mod.short)
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

multinom_model.training <- multinom(mod_peche ~ . - res_tour, data = training)

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

multinom_model.short.red <- multinom(mod_peche ~ . - res_tour, data = df.mod.short)
summary(multinom_model.short.red)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.short.red))
(multinom.tbl.short.red <- tidy(multinom_model.short.red, conf.int = TRUE))
write.csv2(multinom.tbl.short.red, "Tables/Regression/multinom.2020-2023.csv", row.names = F)
(multinom.tbl.short.red.sign <- filter(multinom.tbl.short.red, p.value < 0.05))
# watch video https://www.youtube.com/watch?v=oxRy2DMrOF4 on how to write the model to add it in a report
(mod1vs2 <- paste0("ln[P(mod_peche=2)/P(mod_peche=1)] = ", (exp(coef(multinom_model.short.red)))[1,1], " + " , (exp(coef(multinom_model.short.red)))[1,2],"*",colnames(exp(coef(multinom_model.short.red)))[2], " + ", (exp(coef(multinom_model.short.red)))[1,3],"*",colnames(exp(coef(multinom_model.short.red)))[3], " + ..."))  
writeLines(mod1vs2, "Tables/Regression/multinom.2020-2023.P1vsP2.txt")
(mod1vs3 <- paste0("ln[P(mod_peche=3)/P(mod_peche=1)] = ", (exp(coef(multinom_model.short.red)))[2,1], " + " , (exp(coef(multinom_model.short.red)))[2,2],"*",colnames(exp(coef(multinom_model.short.red)))[2], " + ", (exp(coef(multinom_model.short.red)))[2,3],"*",colnames(exp(coef(multinom_model.short.red)))[3], " + ..."))
writeLines(mod1vs3, "Tables/Regression/multinom.2020-2023.P1vsP3.txt")

# Fit a decision tree
# http://www.milbo.org/rpart-plot/prp.pdf

tree_model <- rpart(mod_peche ~ . - res_tour, data = df.mod.short, method = "class")
rpart.plot(tree_model, type = 4, extra = 2)
#ggsave("Figs/Tree/tree_2020-2023.png", width = 7, height = 4) #doesn't save the plot with ggsave !
# Open a PNG device
#png("Figs/Tree/tree_2020-2023.png", width = 600, height = 350) # bad resolution, save it manually
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
                                      #- poss_bat -res_tour -classe_avis_pnm -Temps_peche_estime.cat2h -cat_pro.gp
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
                                    #- poss_bat -res_tour -classe_avis_pnm -Temps_peche_estime.cat2h -cat_pro.gp
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
                                     #- poss_bat -res_tour -classe_avis_pnm -Temps_peche_estime.cat2h
                                     , data = df.mod.large) # obviously cat_pro.gp still ameliorate the model although not having a significant effect so keep it as well
summary(multinom_model.large.red)
#interpret the coefficients in terms of odds ratios
exp(coef(multinom_model.large.red))
(multinom.tbl.large.red <- tidy(multinom_model.large.red, conf.int = TRUE))
write.csv2(multinom.tbl.large.red, "Tables/Regression/multinom.2020-2022.csv", row.names = F)
(multinom.tbl.large.red.sign <- filter(multinom.tbl.large.red, p.value < 0.05))
# watch video https://www.youtube.com/watch?v=oxRy2DMrOF4 on how to write the model to add it in a report
(mod1vs2 <- paste0("ln[P(mod_peche=2)/P(mod_peche=1)] = ", (exp(coef(multinom_model.large.red)))[1,1], " + " , (exp(coef(multinom_model.large.red)))[1,2],"*",colnames(exp(coef(multinom_model.large.red)))[2], " + ", (exp(coef(multinom_model.large.red)))[1,3],"*",colnames(exp(coef(multinom_model.large.red)))[3], " + ..."))
writeLines(mod1vs2, "Tables/Regression/multinom.2020-2022.P1vsP2.txt")
(mod1vs3 <- paste0("ln[P(mod_peche=3)/P(mod_peche=1)] = ", (exp(coef(multinom_model.large.red)))[2,1], " + " , (exp(coef(multinom_model.large.red)))[2,2],"*",colnames(exp(coef(multinom_model.large.red)))[2], " + ", (exp(coef(multinom_model.large.red)))[2,3],"*",colnames(exp(coef(multinom_model.large.red)))[3], " + ..."))
writeLines(mod1vs3, "Tables/Regression/multinom.2020-2022.P1vsP3.txt")

# Fit a decision tree
# http://www.milbo.org/rpart-plot/prp.pdf
tree_model <- rpart(mod_peche ~ . -a -res_tour -time.day.gp
                    #- poss_bat -res_tour -classe_avis_pnm -Temps_peche_estime.cat2h
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

#2020-2023
df.red <- na.omit(filter(df.msa[, c("a",
                                    "mod_peche",
                                    "sexe",
                                    "res_tour",
                                    "Temps_peche_estime.cat2h",
                                    "zone.gp")
], a %in% c("2020", "2021", "2022", "2023")))
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

df.plora <- df.red
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "trois mode de peche confondus, 2020-2023")

df.plora <- filter(df.red, a == "2023")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "trois mode de peche confondus, 2023")

#ggsave("Figs/Kiviat/kiviat_3.png", width = 5, height = 15) #ggsave doesn't work, do it manually; 400x1200 ; idem for the next ones

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
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2020-2023")

df.plora <- filter(df.red, mod_peche == "peche embarquée")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2020-2023")

df.plora <- filter(df.red, mod_peche == "chasse sous-marine")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2020-2023")

par(op)
par(mfrow = c(1,1))

op <- par(mar = c(0, 2, 2, 2))
par(mfrow = c(3,1))

df.plora <- filter(df.red, mod_peche == "peche du bord", a == "2023")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#F8766D", col.fill = "#F8766D50", title. = "peche du bord, 2023")

df.plora <- filter(df.red, mod_peche == "peche embarquée", a == "2023")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#00BA38", col.fill = "#00BA3850", title. = "peche embarquée, 2023")

df.plora <- filter(df.red, mod_peche == "chasse sous-marine", a == "2023")
df.plora <- droplevels(df.plora)
unique(df.plora$mod_peche)
unique(df.plora$a)
df.plora <- df.plora[ , -which(names(df.plora) %in% c("mod_peche"))]
plot.radar(col. = "#619CFF", col.fill = "#619CFF50", title. = "chasse sous-marine, 2023")

par(op)
par(mfrow = c(1,1))

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
# Function to calculate occurence for each categorical variable
calculate_occurences <- function(df.red) {
  tbl <- table(df.red)
  return(tbl)
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
], a %in% c("2020", "2021", "2022", "2023")))
df.red <- droplevels(df.red) # year 2023 removed when na.omit cfr many variables without data
unique(df.red$a)

# Apply this function to each column in the DataFrame
# Use sapply if you want a list of results for each column
percentage_results <- sapply(df.red, calculate_percentages)
occurence_results <- sapply(df.red, calculate_occurences)
#saveRDS(percentage_results, "percentage_results.rds") #for ChatGPT

# Display the results
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
names(results_df)[names(results_df) == "Var1...1"] <- "parametre"
names(results_df)[names(results_df) == "df.red...2"] <- "categorie"
names(results_df)[names(results_df) == "Freq...3"] <- "pourcentage"
names(results_df)[names(results_df) == "Freq...6"] <- "n"
results_df %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
results_df <- left_join(results_df, N)
results_df
write.csv2(results_df, "Tables/Summary/summary_prop_2020-2023.csv", row.names = F)

df.1 <- filter(df.red, a %in% c("2020", "2021", "2022"))
df.1 <- droplevels(df.1)

percentage_results <- sapply(df.1, calculate_percentages)
occurence_results <- sapply(df.1, calculate_occurences)
#saveRDS(percentage_results, "percentage_results.rds") #for ChatGPT

# Display the results
print(percentage_results)
results_perc <- do.call(rbind, lapply(percentage_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
print(occurence_results)
results_occ <- do.call(rbind, lapply(occurence_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
results_df.1 <- bind_cols(results_perc, results_occ)
setdiff(results_df.1$df.1red...2, results_df.1$df.1red...5)
results_df.1 <- results_df.1[, c(1,2,3,6)]
results_df.1$Var1...1 <- rownames(results_df.1)
results_df.1$Var1...1 <- str_sub(results_df.1$Var1, end=-3)
names(results_df.1)[names(results_df.1) == "Var1...1"] <- "parametre"
names(results_df.1)[names(results_df.1) == "df.red...2"] <- "categorie"
names(results_df.1)[names(results_df.1) == "Freq...3"] <- "pourcentage"
names(results_df.1)[names(results_df.1) == "Freq...6"] <- "n"
results_df.1 %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
results_df.1 <- left_join(results_df.1, N)
results_df.1
write.csv2(results_df.1, "Tables/Summary/summary_prop_2020-2022.csv", row.names = F)

df.2 <- filter(df.red, a %in% c("2023"))
df.2[, colSums(is.na(df.2)) != nrow(df.2)]
df.2 <- droplevels(df.2)

percentage_results <- sapply(df.2, calculate_percentages)
occurence_results <- sapply(df.2, calculate_occurences)
#saveRDS(percentage_results, "percentage_results.rds") #for ChatGPT

# Display the results
print(percentage_results)
results_perc <- do.call(rbind, lapply(percentage_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
print(occurence_results)
results_occ <- do.call(rbind, lapply(occurence_results, function(x) {
  if(is.matrix(x)) as.data.frame(x) else as.data.frame(t(x))
}))
results_df.2 <- bind_cols(results_perc, results_occ)
setdiff(results_df.2$df.2red...2, results_df.2$df.2red...5)
results_df.2 <- results_df.2[, c(1,2,3,6)]
results_df.2$Var1...1 <- rownames(results_df.2)
results_df.2$Var1...1 <- str_sub(results_df.2$Var1, end=-3)
names(results_df.2)[names(results_df.2) == "Var1...1"] <- "parametre"
names(results_df.2)[names(results_df.2) == "df.red...2"] <- "categorie"
names(results_df.2)[names(results_df.2) == "Freq...3"] <- "pourcentage"
names(results_df.2)[names(results_df.2) == "Freq...6"] <- "n"
results_df.2$parametre <- ifelse(results_df.2$categorie == "2023", "a", results_df.2$parametre)
results_df.2 %>% group_by(parametre) %>% summarise(N = sum(n, na.rm = T)) -> N
results_df.2 <- left_join(results_df.2, N)
results_df.2
write.csv2(results_df.2, "Tables/Summary/summary_prop_2023.csv", row.names = F)

unique(results_df$parametre)
results_df %>% filter(parametre == "zone.gp") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle("Secteurs de pêches préférés 2020-2023") +
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
ggsave("Figs/Secteurs/secteurs_2020-2023.png", width = 7, height = 7)

results_df.1 %>% filter(parametre == "zone.gp") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle("Secteurs de pêches préférés 2020-2022") +
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
  ggtitle("Secteurs de pêches préférés 2023") +
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
ggsave("Figs/Secteurs/secteurs_2023.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df", "results_df.1", "results_df.2")))


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

df. %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2020-2023")
ggsave("Figs/Espece/espece.cible.1_2020-2023.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2020-2023")
ggsave("Figs/Espece/espece.cible.2_2020-2023.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2020-2023")
ggsave("Figs/Espece/espece.cible.3_2020-2023.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2020-2022")
ggsave("Figs/Espece/espece.cible.1_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2020-2022")
ggsave("Figs/Espece/espece.cible.2_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2020", "2021", "2022")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2020-2022")
ggsave("Figs/Espece/espece.cible.3_2020-2022.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023")) -> df.sp
plot.bar.sp(var. = "espcib1", title. = "Espèce cible 1 2023")
ggsave("Figs/Espece/espece.cible.1_2023.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023")) -> df.sp
plot.bar.sp(var. = "espcib2", title. = "Espèce cible 2 2023")
ggsave("Figs/Espece/espece.cible.2_2023.png", width = 7, height = 7)

df. %>% filter(a %in% c("2023")) -> df.sp
plot.bar.sp(var. = "espcib3", title. = "Espèce cible 3 2023")
ggsave("Figs/Espece/espece.cible.3_2023.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df", "results_df.1", "results_df.2")))


# plot species and biomass fished ####

df.fsh <- fishing[, c("a", "nom_scien", "nb_ind", "tail_cm", "poids_g")]
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

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2020-2023")
ggsave("Figs/Biomasse/biomasse_2020-2023.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2020", "2021", "2022") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2020-2022")
ggsave("Figs/Biomasse/biomasse_2020-2022.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2023") & poids_g != 0) -> df.fsh.red
plot.bar.fsh(title. = "Biomasse pêchée en 2023")
ggsave("Figs/Biomasse/biomasse_2023.png", width = 7, height = 7)

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

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2020-2023")
ggsave("Figs/Individu/individu_2020-2023.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2020", "2021", "2022")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2020-2022")
ggsave("Figs/Individu/individu_2020-2022.png", width = 7, height = 7)

df.fsh %>% filter(a %in% c("2023")) -> df.fsh.red
plot.bar.fsh(title. = "Individus pêchés en 2023")
ggsave("Figs/Individu/individu_2023.png", width = 7, height = 7)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df", "results_df.1", "results_df.2")))


# plot main fish species caught by size class ####

df.fsh <- fishing[, c("a", "nom_scien", "nb_ind", "tail_cm", "poids_g")]
#saveRDS(df.fsh, "df.fsh.rds") # for ChatGPT

unique(df.fsh$nom_scien)
df.fsh %>% filter(nom_scien %notin% c("0", "NA")) -> df.fsh
unique(df.fsh$nom_scien)

df.fsh %>% filter(a %in% c("2020", "2021", "2022", "2023")) -> df.fsh.red

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

df.fsh %>% filter(a %in% c("2023")) -> df.fsh.red.2

df_sum.2 <- df.fsh.red.2 %>%
  group_by(nom_scien) %>%
  summarise(total_nb_ind = sum(nb_ind, na.rm = TRUE),
            perc_nb_ind = (sum(nb_ind, na.rm = TRUE)/(sum(df.fsh.red.2$nb_ind, na.rm = TRUE)))*100,
            .groups = 'drop')
df_sum.2 <-df_sum.2[order(df_sum.2$total_nb_ind, decreasing = T),]

sort(df_sum$nom_scien[1:6])
sort(df_sum.1$nom_scien[1:6])
sort(df_sum.2$nom_scien[1:6])

sp. <- unique(sort(c(df_sum$nom_scien[1:6], df_sum.1$nom_scien[1:6])))
sp. <- unique(sort(c(sp., df_sum.2$nom_scien[1:6])))
(sort(sp.))
size.at.maturity <- data.frame(species = sp.,
           size.mat = c(18, #"Coris julis", Michel
                        12, #"Diplodus annularis", Michel (7, Lovina)
                        6,  #"Diplodus sargus", Lovina "Listing medfish characteristics Dimarchopoulou et al. 2016.xlsx"
                        round((17.6+15.3)/2,0), #"Loligo sp", "The ML 50% of females was estimated to be 17.6 cm. The ML 50% of males was estimated to be 15.3 cm." Moreno et al., Aquat. Living Resour. 18, 377–384 (2005), Environmental influences on age and size at maturity of Loligo vulgaris 
                        10, #"Pagellus erythrinus", Michel (9, Lovina)
                        4, #"Serranus cabrilla", Lovina "Listing medfish characteristics Dimarchopoulou et al. 2016.xlsx"
                        16, #"Serranus scriba", Michel
                        19 #"Spondyliosoma cantharus", Michel
                        ))

size.sp. <- function(df.fsh., df., years.) {

#df.fsh.=df.fsh.red.1
#df.=df_sum.1
#years.="2020-2022"

for(i in c(1:6)) {

#i=1

df.i <- filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])
print(sp. <- unclass(unique(filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])["nom_scien"]))$nom_scien)
(sexsize=(filter(size.at.maturity,species==sp.))["size.mat"]$size.mat)
filter(df.fsh., nom_scien == sp.)
hist(unlist(filter(df.fsh., nom_scien == sp.)[,"tail_cm"]), xlab = "size (cm)", main = sp.)

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

size.sp.(df.fsh. = df.fsh.red, df. = df_sum, years. = "2020-2023")
size.sp.(df.fsh. = df.fsh.red.1, df. = df_sum.1, years. = "2020-2022")
size.sp.(df.fsh. = df.fsh.red.2, df. = df_sum.2, years. = "2023")

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df", "results_df.1", "results_df.2")))


# plot CPUE ####

# Les captures par unités d’effort (CPUE) par techniques de pêches ont été exprimées en g.jour.pêcheur -1.
# Actually, CPUE in the db is calculated as poids_g/1.5/nb_pecheur ; so this is not the same unit actually ...?!

df. <- fishing[, c("a", "mod_peche", "nom_scien", "CPUE")]
unique(df.$a)
table(df.$a)
unique(df.$mod_peche)
table(df.$mod_peche)

ggplot(filter(df., a %in% c("2020", "2021", "2022", "2023")), aes(x = mod_peche, y = CPUE, fill = mod_peche)) + 
  stat_boxplot(geom = "errorbar",
               width = 0.25) + 
  geom_boxplot()

table(df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "csm", "chasse sous-marine", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pdb", "peche du bord", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "pe", "peche embarquée", df.$mod_peche)
df.$mod_peche <- ifelse(df.$mod_peche == "po", "peche à l'oursin", df.$mod_peche)
table(df.$mod_peche)
df.$mod_peche <- as.factor(df.$mod_peche)
df.$mod_peche <- factor(df.$mod_peche, levels = c("chasse sous-marine", "peche du bord", "peche embarquée", "peche à l'oursin"))
table(df.$mod_peche) 

df. %>% filter(a %in% c("2020","2021","2022", "2023")) -> data.1

ggplot(filter(data.1, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / 1.5 / nb pêcheurs)") +
  ggtitle("2020-2023") +
  ylim(0,max(data.1$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_point(data = filter(data.1, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = mean(unclass((filter(data.1, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(750), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2020-2023.png", width = 5, height = 4)

df. %>% filter(a %in% c("2020","2021","2022")) -> data.2

ggplot(filter(data.2, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / 1.5 / nb pêcheurs)") +
  ggtitle("2020-2022") +
  ylim(0,max(data.2$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_point(data = filter(data.2, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = mean(unclass((filter(data.2, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(750), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2020-2022.png", width = 5, height = 4)

df. %>% filter(a %in% c("2023")) -> data.3

ggplot(filter(data.3, mod_peche != "peche à l'oursin"), aes(x=mod_peche, y=CPUE)) + 
  geom_boxplot() +
  xlab("mode de peche") +
  ylab("CPUE (poids (g) / 1.5 / nb pêcheurs)") +
  ggtitle("2023") +
  ylim(0,max(data.3$CPUE, na.rm = T)*1.1) +
  scale_x_discrete(drop=FALSE) + 
  scale_fill_manual(drop=FALSE) +
  geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = CPUE)) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
  geom_point(data = filter(data.3, mod_peche == "peche à l'oursin"), 
             mapping = aes(x = mod_peche, y = mean(unclass((filter(data.3, mod_peche == "peche à l'oursin")["CPUE"]))$CPUE)), shape=20, size=5, color="red", fill="red") +
  scale_y_cut(breaks = c(750), which=c(1), scales=c(0.5))
ggsave("Figs/CPUE/CPUE_2023.png", width = 5, height = 4)

rm(list=setdiff(ls(), c("fishing", "fishing.PNMCA", "fishing.Stareso", "Meta.survey", "survey", "%notin%", "df.msa", "df.mod", "df.mod.short", "multinom_model.short.red", "df.mod.large", "multinom_model.large.red", "results_df", "results_df.1", "results_df.2")))

