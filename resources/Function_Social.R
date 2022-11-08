#Requirements for function
library(metagear) # forestplots, PRISMA plots
library(tibble) #to use data_frame

#### prisma plot function ####
# Returns PRISMA plot. Still need to change by hand the values of the first lines.
prisma <- function(data, nbtitlesWoS){
  titleWoS <- data[data$source=="WoS",] #after title selection
  NBtitleWoS <- nrow(titleWoS[!duplicated(titleWoS$ID),])
  titleOther <<- data[data$source!="WoS",] #after title selection
  NBtitleOther <<- nrow(titleOther[!duplicated(titleOther$ID),])
  totaltitlesFound <- nbtitlesWoS+NBtitleOther
  titles <- NBtitleWoS+NBtitleOther
  outtitles <- totaltitlesFound-titles
  abstract <- data[data$OUTabstract=="in",] #after abstract screening
  NBabstract <- nrow(abstract[!duplicated(abstract$ID),])
  outabstract <- titles-NBabstract
  texts <- data[data$OUTtext=="in" &
                  data$studytype=="field" | data$studytype=="laboratory",]
  NBtext <- nrow(texts[!duplicated(texts$ID),])
  outtext <- NBabstract-NBtext
  MA <- texts[texts$outMA=="in" ,]
  NBMA <- nrow(MA[!duplicated(MA$ID),])
  outMA <- NBtext-NBMA

  phases <- c(paste("START_PHASE: Studies identified through WoS searching n=", nbtitlesWoS),
              paste("START_PHASE: Additional studies identified n=",NBtitleOther),
              paste("Screening of titles n=", totaltitlesFound),
              paste("EXCLUDE_PHASE: n=", outtitles, "studies excluded"),
              paste("Screening of abstracts n=", titles),
              paste("EXCLUDE_PHASE: n=", outabstract, "studies excluded"),
              paste("Articles reading: n=", NBabstract),
              paste("EXCLUDE_PHASE: n=",  outtext ,"studies excluded"),
              paste("Studies included in vote counting: n=", NBtext),
              paste("EXCLUDE_PHASE: n=", outMA, "studies missing stat info"),
              paste("Studies included in MA: n=", NBMA))

  plot_PRISMA(phases, design = "classic")

} #28/12/21 EDIT: non field studies out text !
# 20/12 edit: added parameter "nbtitlesWoS" to indicate nb of titles from WoS.

#### moderators function ggplot (SOCIAL) ####
# Returns a barplot with the % of each category of a moderator
#!! removes duplicated lines for a given ID.
social_moderator <-  function(moderator){
  socialIN <- socialIN[!duplicated(socialIN$ID),]
  socialIN[,moderator] <- trimws(socialIN[,moderator])
  moderator_data <- as.data.frame(table(socialIN[,moderator]))
  names(moderator_data) <- c("moderator","N")
  moderator_data$proportion <- round(moderator_data$N/sum(moderator_data$N)*100,0)
  print(moderator_data)

  graph_indicator <<- ggplot(moderator_data, aes(fill=moderator, y=proportion, x=" ")) +
    geom_bar(position="stack", stat="identity")+
    #scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.title.y = element_blank(),legend.position = "none",panel.border = element_blank())+
    geom_text(aes(label=moderator), position=position_stack(vjust=0.5))+
    labs(x=moderator)
  #print(graph_indicator)
}

general_moderator <- function(data, moderator, duplicated = "yes"){
  if (duplicated=="yes") {
    data <- data[!duplicated(data$ID),]
  }
  data[,moderator] <- trimws(data[,moderator])
  moderator_data <- as.data.frame(table(data[,moderator]))
  names(moderator_data) <- c("moderator","N")
  moderator_data$proportion <- round(moderator_data$N/sum(moderator_data$N)*100,0)
  print(moderator_data)

  graph_indicator <<- ggplot(moderator_data, aes(fill=moderator, y=proportion, x=" ")) +
    geom_bar(position="stack", stat="identity")+
    #scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                       axis.title.y = element_blank(),legend.position = "none",panel.border = element_blank())+
    geom_text(aes(label=moderator), position=position_stack(vjust=0.5))+
    labs(x=moderator)
  print(graph_indicator)
}

#### vote counting function (stacked bar plot - SOCIAL) ####
# social_mech takes the mechanism name and returns:
# (1) the nb of unique studies, nb of quantitative data points and directions
# (2) a stacked bar plot (ggplot) showing the nb of data points for each direction globally
# (3) a stacked bar plot (ggplot) showing the nb of data points for each direction and indicator
social_mech <- function(Mechanism){
  mech <- social[social$OUTtext=="in" & social$mechanism==Mechanism
                 & social$studytype=="field",] # empirical studies reporting mechibility
  mech <- data.table(mech)
  mech$indicator <- as.factor(trimws(mech$indicator))
  mech$direction <- as.factor(trimws(mech$direction))

  unique <- nrow(mech[!duplicated(mech$ID),]) # nb of unique studies
  cat("nb of unique studies is",unique,"\n")

  quantitative <-  table(mech$quantitative)
  print("nb of quantitative data points")
  print(quantitative)

  indicators <- table(as.character(mech$indicator))
  print(indicators) #nb of data points for both indicators

  directionsAll <-  mech[,.(.N),by="direction"] #directions throughout indicators
  directionsAll$proportion <- round(directionsAll$N/sum(directionsAll$N)*100,0)
  directionsAll$direction <- factor(directionsAll$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  print("directions of data points throughout indicators")
  print(directionsAll)

  graph_global <- ggplot(directionsAll, aes(fill=direction, y=N, x=Mechanism)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))
  print(graph_global)

  directions <-  mech[,.(.N),by=.(indicator,direction)] #directions for each indicator
  for (k in levels(directions$indicator)){
    directions$proportion[directions$indicator==k] <- round(directions$N[directions$indicator==k] /indicators[k]*100,0)
  }
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions <- directions[order(indicator,direction)]
  print("directions of data points by indicators")
  print(directions)

  graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=indicator)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))+
    labs(x=paste(Mechanism,"by indicator"))
  print(graph_indicator)
}
# vote counting only for different indicators
vote_counting <- function(data,Mechanism){
  mech <- data[data$OUTtext=="in" &
                 data$studytype=="field" | data$studytype=="laboratory" ,]
  mech$indicator <- as.factor(trimws(mech$indicator))
  mech$direction <- as.factor(trimws(mech$direction))
  mech <- data.table(mech)

  unique <- nrow(mech[!duplicated(mech$ID),]) # nb of unique studies
  cat("nb of unique studies is",unique,"\n")

  indicators <- table(mech$indicator)
  print(indicators) #nb of data points per indicator

  directionsAll <-  mech[,.(.N),by="direction"] #directions throughout indicators
  directionsAll$proportion <- round(directionsAll$N/sum(directionsAll$N)*100,0)
  directionsAll$direction <- factor(directionsAll$direction,levels=c("positive", "neutral", "ambiguous","negative"))

  # general graph for the mechanism
  graph_global <- ggplot(directionsAll, aes(fill=direction, y=N, x=Mechanism)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))
  print(graph_global)

  # directions per indicator
  directions <-  mech[,.(.N),by=.(indicator,direction)] #directions for each indicator
  for (k in levels(directions$indicator)){
    directions$proportion[directions$indicator==k] <- round(directions$N[directions$indicator==k] /indicators[k]*100,0)
  }
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions <- directions[order(indicator,direction)]
  print("directions of data points by indicators")
  print(directions)

  graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=indicator)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))+
    labs(x=paste(Mechanism,"by indicator"))
  print(graph_indicator)
} #28/12/21
# vote counting for any moderator wanted
vote_counting_moderator <- function(data,Mechanism,moderator){
  mech <- data[data$OUTtext=="in" &
                 data$studytype=="field" | data$studytype=="laboratory" ,]
  mech[[moderator]] <- as.factor(trimws(mech[[moderator]]))
  mech$direction <- as.factor(trimws(mech$direction))
  mech <- data.table(mech)

  unique <- nrow(mech[!duplicated(mech$ID),]) # nb of unique studies
  cat("nb of unique studies is",unique,"\n")

  moderators <<- as.data.table(table(mech[[moderator]]))
  colnames(moderators) <<- c("variable","n")
  print(moderators) #nb of data points per indicator

  directionsAll <-  mech[,.(.N),by="direction"] #directions throughout moderators
  directionsAll$proportion <- round(directionsAll$N/sum(directionsAll$N)*100,0)
  directionsAll$direction <- factor(directionsAll$direction,
                                    levels=c("positive", "neutral", "ambiguous","negative"))

  # general graph for the mechanism
  graph_global <- ggplot(directionsAll, aes(fill=direction, y=N, x=Mechanism)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))
  print(graph_global)

  # directions per indicator
  directions <<-  as.data.frame(table(mech[[moderator]], mech$direction)) #directions for each indicator
  colnames(directions) <<- c("variable","direction","N")
  directions$proportion <- c(0)
  for (k in levels(directions$variable)){
    tot_variable <- moderators[variable==k,"n"]
    print(k)
    print(tot_variable)
    directions[directions$variable==k,"proportion"] <- directions[directions$variable==k,"N"]/as.integer(tot_variable)
  }
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions$proportion <- round(directions$proportion*100,0)
  directions <- as.data.table(directions)
  directions <- directions[order(variable,direction)]
  print("directions of data points by variable")
  print(directions)


  graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=variable)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=proportion), position=position_stack(vjust=0.5))+
    labs(x=paste(Mechanism,"by indicator"))
  print(graph_indicator)
} #28/12/21

#### foresplot and f(ei)=vei for the whole mechanism ####
# Returns the nb of unique studies, the general forest plot and f(ei) = vei
generalMA <- function(MAdata){ # 25/07/22 added funnel plot
  MAdata <- MAdata[MAdata$outMA=="in",]
  unique=nrow(MAdata[!duplicated(MAdata$ID),])
  cat("nb of unique studies is", unique, "\n")
  cat("nb of data point is", nrow(MAdata))

  MA=rma(yi=ei, vi=vei, data=MAdata)
  print(MA)

  forest1=forest(MA, slab = paste(MAdata$ID, MAdata$country, MAdata$ecosystem), showweights = TRUE)
  print(forest1)

  funnel(MA)

  var_plot=ggplot(MAdata, aes(x=ei, y=vei)) +
    geom_point()
  print(var_plot)
}
#### forestplot for one indicator ####
indicatorMA <- function(data,indicator){
  MAdata <- data[data$outMA=="in" & data$indicator==indicator,]
  unique=nrow(MAdata[!duplicated(MAdata$ID),])
  cat("nb of unique studies is", unique, "\n")
  MA <- rma(yi=ei, vi=vei, data=MAdata)
  print(MA)

  forest(MA, slab = paste(MAdata$ID,MAdata$MPAname,
                                     MAdata$MPAprotection, MAdata$MPAage),
                    showweights = TRUE)
  funnel(MA)
} #03/12/21
#### effect size for each indicator on a same plot ####
# Returns a plot with the effect size of each indicator
combined_indicatorsMA <- function(data){
  data$ei = as.numeric(as.character(data$ei))
  data$vei = as.numeric(as.character(data$vei))
  MAdata <- data[data$outMA=="in",]
  print(table(as.character(MAdata$indicator)))
  MAInt <- rma(yi=ei, vi=vei, data=MAdata, mods=~indicator)
  print(MAInt)
  MA <- rma(yi=ei, vi=vei, data=MAdata, mods=~indicator-1)
  print(MA)

  #Creating data table
  table <- data_frame(category=row.names(MA[["b"]]),
                      ei=as.vector(MA[["b"]]),
                      low=as.vector(MA[["ci.lb"]]),
                      high=as.vector(MA[["ci.ub"]]))
  minLow <- round(min(table$low,na.rm=TRUE),0)-0.5 # min x axis
  maxHigh <- round(max(table$high,na.rm=TRUE),0)+0.5 # max x axis
  return(table)

  forestplot(table$category,
             mean=table$ei,lower=table$low,upper=table$high,
             boxsize=0.1,fn.ci_norm = fpDrawCircleCI,
             xticks = seq(minLow,maxHigh,0.5),
             txt_gp=fpTxtGp(ticks=gpar(fontfamily="",fontsize=30),
                            legend = gpar(fontfamily="",fontsize=25),
                            label= gpar(fontfamily="",fontsize=25)),
             clip=seq(minLow,maxHigh), #clip defines the interval out of which an arrow is used to symbolize extended CI
             col=fpColors(box="blue",line="black"))
} #02/12/21 Forestplot does not work
#### effect size for levels of a moderator for an indicator ####
# effect size for levels of a moderator for an indicator
#19/01/21: added a print of table of nb of occurence for each level of moderator.
MA_moderator <- function(data,moderator,indicator,MPA="yes"){
  MAdata <- data[data$outMA=="in" & data$indicator==indicator,]
  #nb of observations per MPA and per study
  print(table(MAdata[[moderator]]))
  if (MPA=="yes"){
  MAdata <- MAdata[which(!is.na(MAdata[[moderator]])),]
  MAdata[[moderator]] <- as.factor(trimws(MAdata[[moderator]]))
  indic_info <- data.frame("moderator"=levels(MAdata[[moderator]]),
                           "studies"=c(0), "MPAs"=c(0), "datapoints"=c(0))
  for (i in levels(MAdata[[moderator]])){
    mechIN <- MAdata[MAdata[[moderator]]==i,]
    valueMPA <- nrow(mechIN[!duplicated(mechIN$MPAname),])
    indic_info[indic_info$moderator==i,"MPAs"] <- valueMPA
    valueStudies<- nrow(mechIN[!duplicated(mechIN$ID),])
    indic_info[indic_info$moderator==i,"studies"] <- valueStudies
    indic_info[indic_info$moderator==i,"datapoints"] <- nrow(mechIN)
  }
  print(indic_info)
  }
  MAInt=rma(yi=ei, vi=vei, data=MAdata, mods=~MAdata[[moderator]])
  MA=rma(yi=ei, vi=vei, data=MAdata, mods=~MAdata[[moderator]]-1)
  print(MAInt)
  print(MA)

  #Creating data table
  table <- data_frame(category=row.names(MA[["b"]]),
                      ei=as.vector(MA[["b"]]),
                      low=as.vector(MA[["ci.lb"]]),
                      high=as.vector(MA[["ci.ub"]]))
  minLow <- round(min(table$low,na.rm=TRUE),0)-0.5 # min x axis
  maxHigh <- round(max(table$high,na.rm=TRUE),0)+0.5 # max x axis

  #Forest plot
  base=6 # height of figure
  expand=2 # font size increase
  forestplot(table$category,
             mean=table$ei,lower=table$low,upper=table$high,
             boxsize=0.1,fn.ci_norm = fpDrawCircleCI,
             xticks = seq(minLow,maxHigh,0.5),
             txt_gp=fpTxtGp(ticks=gpar(fontfamily="",fontsize=base*expand),
                            legend = gpar(fontfamily="",fontsize=base*expand),
                            label= gpar(fontfamily="",fontsize=base*expand)),
             clip=seq(minLow,maxHigh), #clip defines the interval out of which an arrow is used to symbolize extended CI
             col=fpColors(box="blue",line="black"))
}

#### nb of data points per levels of moderators for all moderators ####
moderators <- function(data){
  data$ei = as.numeric(as.character(data$ei))
  data$vei = as.numeric(as.character(data$vei))
  MAdata <- data[data$outMA=="in",]
  print(table(as.character(MAdata$Age)))
  print(table(as.character(MAdata$Size)))
  print(table(as.character(MAdata$MPAprotection)))
}
bootstrapping <- function(data,n,Indicator){
  MAboot <- data[data$outMA=="in" & data$indicator==Indicator,]
  uniqueStudies <- nrow(MAboot[!duplicated(MAboot$ID),])
  datapoints <- nrow(MAboot)
  print(paste("nb of unique studies is", uniqueStudies))
  print(paste("nb of datapoints is", datapoints))
  MAboot$sample <- (MAboot$AfterMPA_samplesize+MAboot$AfterControl_samplesize) #sum of sample size
  ARei <- sum(MAboot$sample*MAboot$ei)/sum(MAboot$sample) #moyenne des ei pondérée par sample size
  bootEbars <- numeric(0) #initialisation
  for (i in 1:n){ # n weighted means
    set.seed(i)
    size <- nrow(MAboot)
    mySize <- sample(size,1)
    set.seed(size+1-i)
    newTab <- MAboot[(sample(1:size,mySize,replace=T)),] #échantillonnage de données
    bootEbar <- sum(newTab$sample*newTab$ei)/sum(newTab$sample) #moyenne à partir de l'échantillon
    bootEbars <- c(bootEbars,bootEbar)  #vecteur contenant les moyennes calculées
  }
  hist(bootEbars, main = paste("Effect size for", Indicator))

  abline(v=ARei,col="red") #représentation de la moyenne de l'échantillon initial
  CI=quantile(bootEbars,probs=c(2.5,97.5)/100)
  ARminCI=CI[1]
  abline(v=ARminCI,col="blue") #lower confidence interval
  ARmaxCI=CI[2]
  abline(v=ARmaxCI,col="blue") #upper confidence interval
  print(paste("Ebar = ",ARei," [",ARminCI,";",ARmaxCI,"]"))
  values <<- list(ARei,ARminCI,ARmaxCI)
} #28/12/21






