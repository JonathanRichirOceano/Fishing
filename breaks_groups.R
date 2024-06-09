#ChatGPT

# Sample data
data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
border_value <- 6

# Split data into two parts based on the border value
data_left <- data[data < border_value]
data_right <- data[data >= border_value]

# Determine the number of groups on each side
total_groups <- 5
left_proportion <- length(data_left) / length(data)
right_proportion <- length(data_right) / length(data)

left_groups <- round(left_proportion * total_groups)
right_groups <- total_groups - left_groups

# Ensure at least one group on each side if there are data points
if (length(data_left) > 0 && left_groups == 0) left_groups <- 1
if (length(data_right) > 0 && right_groups == 0) right_groups <- 1

# Define breaks for left side
if (length(data_left) > 0) {
  left_breaks <- seq(min(data_left), max(data_left), length.out = left_groups + 1)
} else {
  left_breaks <- NULL
}

# Define breaks for right side
if (length(data_right) > 0) {
  right_breaks <- seq(min(data_right), max(data_right), length.out = right_groups + 1)
} else {
  right_breaks <- NULL
}

# Combine breaks
breaks <- unique(c(left_breaks, right_breaks))

# Cut data into groups
data_groups <- cut(data, breaks = breaks, include.lowest = TRUE, labels = FALSE)

# Result
data_frame <- data.frame(Value = data, Group = data_groups)
print(data_frame)

#All script with unsucesfull trials

for(i in c(1:6)) {
  
  #i=1
  
  df.i <- filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])
  print(sp. <- unclass(unique(filter(df.fsh.red, nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])["nom_scien"]))$nom_scien)
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
  #(left_proportion <- length(data_left) / length(data))
  (right_proportion <- (max(data, na.rm = T)-sexsize)/(max(data, na.rm = T)-min(data, na.rm = T))) 
  #(right_proportion <- length(data_right) / length(data))
  
  (left_groups <- round(left_proportion * total_groups))
  (right_groups <- total_groups - left_groups)
  
  # Ensure at least one group on each side if there are data points
  #(if (length(data_left) > 0 && left_groups == 0) left_groups <- 1)
  #(if (length(data_right) > 0 && right_groups == 0) right_groups <- 1)
  
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
  
  # Cut data into groups
  #(data_groups <- cut(data, breaks = breaks, include.lowest = FALSE, labels = FALSE))
  
  # Result
  #(data_frame <- data.frame(value = data, group = data_groups))
  #df.i <- bind_cols(df.i, data_frame)
  #print(unique(df.i$tail_cm-df.i$value))
  #df.i <- df.i[,-which(names(df.i) == "value")]
  
  #(max((unlist(df.i[,"tail_cm"])), na.rm = T) -> maxsz)
  #max((unlist(df.i[,"tail_cm"])), na.rm = T) - sexsize -> maxsz
  #(ifelse(maxsz<0, 0, maxsz) -> maxsz)
  #sexsize - (ifelse(maxsz<0, 0, maxsz) -> maxsz)
  #(min((unlist(df.i[,"tail_cm"])), na.rm = T) -> minsz)
  #sexsize - min((unlist(df.i[,"tail_cm"])), na.rm = T) -> minsz
  #(ifelse(minsz<0, 0, minsz) -> minsz)
  #c(minsz,maxsz)
  
  #(maxsz.sexsize <- ifelse(maxsz-sexsize<0,0,maxsz-sexsize))
  #(sexsize.minsz <- ifelse(sexsize-minsz<0,0,sexsize-minsz))
  #max(c(maxsz.sexsize,sexsize.minsz))/min(c(maxsz.sexsize,sexsize.minsz))
  #min(c(maxsz.sexsize,sexsize.minsz))/max(c(maxsz.sexsize,sexsize.minsz))
  
  #max(c(maxsz.sexsize,sexsize.minsz))/min(c(maxsz.sexsize,sexsize.minsz))
  #(cat.short <- 10/(max(c(maxsz.sexsize,sexsize.minsz))/min(c(maxsz.sexsize,sexsize.minsz))))
  #(cat.large <- 10-cat.short)
  #(cat.short <- round(cat.short/2, digits = 0))
  #(cat.large <- round(cat.large/2, digits = 0))
  #(cat.large <- ifelse(cat.short+cat.large==5,cat.large,cat.large-1))
  #c(cat.short, cat.large)
  
  #(5/(max(c(maxsz.sexsize,sexsize.minsz))/min(c(maxsz.sexsize,sexsize.minsz))) -> cat.short.i)
  #(5/(min(c(maxsz.sexsize,sexsize.minsz))/max(c(maxsz.sexsize,sexsize.minsz))) -> cat.long.i)
  #(round(5/(max(c(maxsz,minsz))/min(c(maxsz,minsz))), digits = 0) -> cat.short)
  #(cat.short <- ifelse(round(cat.short.i, digits = 0) == round(cat.long.i, digits = 0) & round(cat.short.i, digits = 0) + round(cat.long.i, digits = 0) > 5, (round(min(cat.short.i, cat.long.i), digits = 0)-1)/2, cat.short.i))
  #(cat.large <- 5-cat.short)
  
  #(round(cat.short, digits = 0)->cat.short)
  #(round(cat.long, digits = 0)->cat.long)
  #(round(5-(5/(max(c(maxsz.sexsize,sexsize.minsz))/min(c(maxsz.sexsize,sexsize.minsz)))), digits = 0) -> cat.long)
  #cat.short ; cat.large
  
  #issue here because min = 0
  #max(c(minsz,maxsz))/min(c(minsz,maxsz))
  #(round(5/(max(c(minsz,maxsz))/min(c(minsz,maxsz))), digits = 0) -> cat.short)
  #(round(5-(5/(max(c(minsz,maxsz))/min(c(minsz,maxsz)))), digits = 0) -> cat.large)
  
  #(sort(c(maxsz/minsz, minsz/maxsz)) -> r.)
  #(ifelse(is.finite(r.)==F,999,r.) -> r.)
  #5*(maxsz/minsz)
  #5/(minsz/maxsz)
  #(round(5*r.[1], digits = 0) -> cat.short)
  #(10 - cat.short -> cat.large)
  #(cat.short <- cat.short/2)
  #round(cat.short, digits = 0)
  #(cat.large <- cat.large/2)
  #round(cat.large, digits = 0)
  #cat.short <- ifelse(round(cat.short, digits = 0)+round(cat.large, digits = 0)>5 & round(cat.short, digits = 0)<round(cat.large, digits = 0), round(cat.short, digits = 0)-1,round(cat.short, digits = 0))
  #cat.large <- ifelse(round(cat.short, digits = 0)+round(cat.large, digits = 0)>5 & round(cat.short, digits = 0)>round(cat.large, digits = 0), round(cat.large, digits = 0)-1,round(cat.large, digits = 0))
  #cat.short ; cat.large
  #(ifelse(maxsz >= minsz, cat.short, cat.large) -> cut.inf.sex)
  #(setdiff(c(cat.short, cat.large), cut.inf.sex) -> cut.sup.sex)
  #((seq(1:cut.sup.sex) -> seq.up) * round((maxsz/cut.sup.sex), digits = 0) -> add.up)
  #(rev(seq(1:cut.inf.sex) -> seq.inf) * round((minsz/cut.inf.sex), digits = 0) -> add.down)
  
  #(round(ifelse(maxsz.sexsize >= sexsize.minsz, cat.large, cat.short), digits = 0) -> cut.sup.sex)
  #(ifelse(maxsz <= minsz, cat.short, cat.large) -> cut.sup.sex)
  #(round(setdiff(c(cat.short, cat.large), cut.sup.sex), digits = 0) -> cut.inf.sex)
  #((seq(1:cut.sup.sex) -> seq.up) * round((maxsz/cut.sup.sex), digits = 0) -> add.up)
  #(rev(seq(1:cut.inf.sex) -> seq.inf) * round((minsz/cut.inf.sex), digits = 0) -> add.down)
  #((seq(1:cut.sup.sex) -> seq.up) * ((maxsz.sexsize)/cut.sup.sex)-> add.up)
  #(rev(seq(1:cut.inf.sex) -> seq.inf) * ((sexsize.minsz)/cut.inf.sex) -> add.down)
  
  #print(min((unlist(filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])[,"tail_cm"])), na.rm = T)) -> min.
  #print(max((unlist(filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])[,"tail_cm"])), na.rm = T)) -> max.
  #minsz
  #maxsz
  #(c(sexsize-add.down#[1:length(add.down)]
  #  ,sexsize,sexsize+add.up) -> s.cl)
  #s.cl <- s.cl[!is.na(s.cl) & !is.infinite(s.cl)]
  #(s.cl <- round(s.cl, digits = 0))
  #(s.cl[1] <- ifelse(s.cl[1]>minsz, minsz,s.cl[1]))  
  #(s.cl[length(s.cl)] <- ifelse(s.cl[length(s.cl)]<maxsz, rmaxsz,s.cl[length#(s.cl)]))
  #s.cl
  #(s.cl <- ifelse(s.cl<0 ,0, s.cl))
  
  #df.i <- filter(df.fsh., nom_scien == sort(df.[1:6,"nom_scien"]$nom_scien)[i])
  s.cl <- breaks
  df.i$tail_cm_classe <- NA
  df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[2], paste0("[",s.cl[1],"-",s.cl[2],"["), df.i$tail_cm_classe)
  df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[3] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[2],"-",s.cl[3],"["), df.i$tail_cm_classe)
  df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[4] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[3],"-",s.cl[4],"["), df.i$tail_cm_classe)
  df.i$tail_cm_classe <- ifelse(df.i$tail_cm < s.cl[5] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[4],"-",s.cl[5],"["), df.i$tail_cm_classe)
  df.i$tail_cm_classe <- ifelse(df.i$tail_cm <= s.cl[6] & is.na(df.i$tail_cm_classe), paste0("[",s.cl[5],"-",s.cl[6],"]"), df.i$tail_cm_classe)
  df.i$tail_cm_classe <- as.factor(df.i$tail_cm_classe)
  (df.i$tail_cm_classe)
  
  levels(df.i$tail_cm_classe)
  df.i$tail_cm_classe <- factor(df.i$tail_cm_classe, levels = c(paste0("[",s.cl[1],"-",s.cl[2],"["), paste0("[",s.cl[2],"-",s.cl[3],"["), paste0("[",s.cl[3],"-",s.cl[4],"["),paste0("[",s.cl[4],"-",s.cl[5],"["), paste0("[",s.cl[5],"-",s.cl[6],"]")))
  levels(df.i$tail_cm_classe)
  df.i$tail_cm_classe.num <- NA
  df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[1],"-",s.cl[2],"["), 1, df.i$tail_cm_classe.num)
  df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[2],"-",s.cl[3],"["), 2, df.i$tail_cm_classe.num)
  df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[3],"-",s.cl[4],"["), 3, df.i$tail_cm_classe.num)
  df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[4],"-",s.cl[5],"["), 4, df.i$tail_cm_classe.num)
  df.i$tail_cm_classe.num <- ifelse(df.i$tail_cm_classe == paste0("[",s.cl[5],"-",s.cl[6],"]"), 5, df.i$tail_cm_classe.num)
  df.i$tail_cm_classe.num <- as.integer(df.i$tail_cm_classe.num)
  (df.i$tail_cm_classe.num)
  
  (txt. <- paste0("[",sexsize,"-")) #don't use the [ symbol because of error 
  threshold_category <- unlist(unique((df.i[grep(paste0("\\",txt.), df.i$tail_cm_classe), ])$tail_cm_classe.num))
  #threshold_category <- unclass(unlist(unique(filter(df.i, gsub(txt., tail_cm_classe))[,"tail_cm_classe.num"])))
  df.i$mature <- NA
  df.i$mature <- ifelse(df.i$tail_cm_classe.num < threshold_category, "non", "oui")
  df.i$mature <- as.factor(df.i$mature)
  df.i.narm <- na.omit(df.i[, c("tail_cm_classe", "tail_cm_classe.num", "mature")])
  print(table(na.omit(df.i[, c("tail_cm_classe", "tail_cm_classe.num", "mature")])))
  df.i.narm %>% group_by(tail_cm_classe, mature) %>% summarise(n())
  
  barplot.i <- ggplot(df.i.narm, aes(x = tail_cm_classe, fill = mature)) +
    geom_bar() +
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
  #print(barplot.i) 
  barplot.i <- barplot.i + ggtitle(bquote(italic(.(unique(df.i$nom_scien)))~.(years.)~"(N ="~.(nrow(df.i.narm))*")"))
  print(barplot.i)
  #ggsave(paste0("Figs/count/Calvi/Calvi_", unique(sp.i$`NOM LATIN`), ".hist.png"), hist.i, width = 5, height = 4)#update script
  
}
