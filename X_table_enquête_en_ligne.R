# table enquête en ligne ####

df. <- survey[, c("fiche_n", "mod_peche", "a", "sexe", "res_tour", "age_moyen", "nb_sort_an", "cat_pro", "dep_pdb", "dep_pe", "dep_csm", "dep_po", "z_1","z_2","z_3","z_4","z_6","z_7","z_8","z_9","z_54","z_55","z_56","z_57","z_58","z_59","z_60","z_61", "avis_pnm")]

unique(df.$a)
unique(df.$sexe)
unique(df.$res_tour)

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
rm(cols_to_replace)
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
df. <- subset(df., select = -c(fiche_n...30))
names(df.)[names(df.) == "fiche_n...1"] <- "fiche_n"
unique(df.$zone) ; table(df.$zone)
df.$zone <- ifelse(df.$zone %in% c("Agriates", "Saint-Florent", "Ouest Cap", "Nord Cap", "Est Cap"), df.$zone, "multiple")

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

unique(df.$cat_pro)
df.$cat_pro <- ifelse(df.$cat_pro == "NA", NA, df.$cat_pro)
table(df.$cat_pro)
nrow(df.) ; length(na.omit(df.$cat_pro))
table(df.[, c("a", "cat_pro")])
table(survey[, c("a", "act_pro")])
unique(survey$a)
unique(filter(survey[,c("a", "act_pro")], "a" == "2023"))
#we have no info about cat_pro in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

unique(df.$nb_sort_an)
df.$nb_sort_an <- ifelse(df.$nb_sort_an == "NA", NA, df.$nb_sort_an)
df.$nb_sort_an <- ifelse(df.$nb_sort_an == "illimite", NA, df.$nb_sort_an)
table(df.$nb_sort_an) #or considering that variable as categorial, cfr idem classe_age? 
df.$nb_sort_an <- as.numeric(df.$nb_sort_an)
nrow(df.) ; length(na.omit(df.$nb_sort_an))
unique(filter(survey[,c("a", "nb_sort_an")], "a" == "2023"))
#we have no info about nb_sort_an in 2023 so not possible to plot the multivariate analysis, or we do not consider that variable neither in the analysis; idem for act_pro

unique(df.$nb_sort_an); length(na.omit(df.$nb_sort_an)) ; table(df.$nb_sort_an) ; table(df.$a, df.$nb_sort_an) #no data for 2023
df.$nb_sort_an_cat <- cut(df.$nb_sort_an, breaks=quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T), include.lowest=TRUE, labels=c(
  paste0("Q1 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[1]], "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[2]], ")") , 
  paste0("Q2 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[2]]+1, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[3]], ")") , 
  paste0("Q3 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[3]]+1, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[4]], ")") , 
  paste0("Q4 (", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[4]]+1, "-", quantile(df.$nb_sort_an, probs=0:4/4, na.rm = T)[[5]], ")") ))
unique(df.$nb_sort_an_cat); length(na.omit(df.$nb_sort_an_cat)) ; table(df.$nb_sort_an_cat) ; table(df.$a, df.$nb_sort_an_cat) #no data for 2023

unique(df.$avis_pnm)
df. <- add_column(df., classe_avis_pnm = df.$avis_pnm, .after = "avis_pnm")
unique(df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "NA", NA, df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "_", NA, df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "t_positif", "positif", df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "t_negatif", "negatif", df.$classe_avis_pnm)
df.$classe_avis_pnm <- ifelse(df.$classe_avis_pnm == "nsp", "p_avis", df.$classe_avis_pnm)
table(df.$classe_avis_pnm)

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

df.red <- df.[, c("mod_peche", "a", "sexe", "res_tour", "age_classe", "cat_pro", "classe_avis_pnm", "zone", "nb_sort_an_cat", "dep.median.rd", "dep.mod_peche")]
df.red <- lapply(df.red, as.character)
df.red <- data.frame(df.red)
#write.csv2(df.red, "df.red.csv") #for ChatGPT

# up to here, code removed from fishing.R script because identical to multivariate spatial analysis code. And from below on, same code than in the fishing.R script.

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

df.1 <- filter(df.red, a %in% c("2020", "2021", "2022"))

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

df.2 <- filter(df.red, a %in% c("2023"))
df.2[, colSums(is.na(df.2)) != nrow(df.2)]

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

results_df %>% group_by(parametre) %>% filter(pourcentage == max(pourcentage)) -> df.plot
df.plot <- df.plot[1:10, c("categorie", "pourcentage")]
#saveRDS(df.plot, "df.plot.rds") #for ChatGPT
df.plot <- pivot_wider(df.plot, names_from = categorie, values_from = pourcentage)
p.radar <- rbind(0,df.plot)
p.radar <- rbind(100,p.radar)
p.radar <- lapply(p.radar,as.numeric)
#p.radar[3,] <- p.radar[3,]/100
names.p.radar <- names(df.plot)
p.radar <- data.frame(p.radar)
names(p.radar) <- names.p.radar
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
)


results_df %>% filter(parametre == "zone") -> df.plot

ggplot(df.plot, aes(x="", y=pourcentage, fill=categorie)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y", start=0) +
  ggtitle("Secteurs de pêches préférés toutes années confondues") +
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

results_df.1 %>% filter(parametre == "zone") -> df.plot

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

results_df.2 %>% filter(parametre == "zone") -> df.plot

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
