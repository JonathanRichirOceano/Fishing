
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
library(writexl)

`%notin%` <- Negate(`%in%`)

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


# plot main fish species caught by size class + mesh size ####

fishing <- readRDS("Raw data_mined/terrain.rds")

df.fsh <- fishing[, c("a", "nom_scien", "nb_ind", "tail_cm")]
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
(sort(sp.))
size.at.maturity <- data.frame(species = sp.,
                               size = c(18, #"Coris julis", Michel
                                        round(34.6), #Dentex dentex (https://fishbase.mnhn.fr/)
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

sp.catch <- sp.[sp.%in%c("Diplodus annularis", "Diplodus sargus", "Pagellus erythrinus", "Spondyliosoma cantharus")] 
size.catch <- data.frame(species = sp.catch,
                         size = c(12, #"Diplodus annularis"
                                  23, #"Diplodus sargus",
                                  15, #"Pagellus erythrinus", Michel
                                  23 #"Spondyliosoma cantharus", Michel
                         ))

# according to maturity size

size.sp. <- function(df.fsh., df.size, df., years.) {
  
  #df.fsh.=df.fsh.red.3
  #df.=df_sum.3
  #df.size=size.at.maturity
  #years.="2020-2024"
  
  for(i in c(1:6)) {
    
    #i=6
    
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
    #ggsave(paste0("Figs/Sc_nb_sp/", unique(na.omit(df.i$nom_scien)), "_", years.,"_barplot.png"), barplot.i, width = 5, height = 4)
    
  }
  
}

size.sp.(df.fsh. = df.fsh.red, df.size = size.at.maturity, df. = df_sum, years. = "2020-2024")
#size.sp.(df.fsh. = df.fsh.red.1, df.size = size.at.maturity, df. = df_sum.1, years. = "2020-2022")
#size.sp.(df.fsh. = df.fsh.red.2, df.size = size.at.maturity, df. = df_sum.2, years. = "2023-2024")

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
    #ggsave(paste0("Figs/Maille_nb_sp/", unique(na.omit(df.i$nom_scien)), "_", years.,"_barplot.png"), barplot.i, width = 5, height = 4)
    
  }
  
}

maille.sp.(df.fsh. = df.fsh.red, df.size = size.catch, df. = df_sum, years. = "2020-2024")
#maille.sp.(df.fsh. = df.fsh.red.1, df.size = size.catch, df. = df_sum.1, years. = "2020-2022")
#maille.sp.(df.fsh. = df.fsh.red.2, df.size = size.catch, df. = df_sum.2, years. = "2023-2024")

