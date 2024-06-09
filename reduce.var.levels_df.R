
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
