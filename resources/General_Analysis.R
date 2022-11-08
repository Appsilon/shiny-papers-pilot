#### Dependencies ####
library(metafor) # perform meta-analysis
library(forestplot) # forestplots
library(dplyr)
library(ggpubr)
library(data.table)
library(stringr)
library(readr)
library(RColorBrewer)
source("Function_Social.R")
studies <- read.csv("data/DataS2.csv",colClasses ="character") %>%
  rename("ID" = "ï..ID")
studies <- studies[studies$mechanism!="pH",]
MAdata <- studies[studies$outMA=="in",]
MAdata$ei <- as.numeric(MAdata$ei)
MAdata$vei <- as.numeric(MAdata$vei)
MAdata$MPAage <- as.numeric(MAdata$MPAage)
MAdata$MPAsize <- as.numeric(MAdata$MPAsize)
MAdata$AfterMPA_samplesize <- as.numeric(MAdata$AfterMPA_samplesize)
MAdata$AfterControl_samplesize <- as.numeric(MAdata$AfterControl_samplesize)

#### total nb of studies by mechanism ####
Total_studies <- nrow(studies[!duplicated(studies$ID2),])
table_nbstudies <- data.frame(mechanism = character(),
                  votes = integer(),MA = integer(),stringsAsFactors = FALSE)
i <- 1
# gives a table with unique # studies per mechanism
for (k in levels(as.factor(studies$mechanism))){
  VC_studies <- studies[studies$mechanism==k,]
  nb_votes <- nrow(VC_studies[!duplicated(VC_studies$ID2),])
  MA_studies <- VC_studies[VC_studies$outMA=="in",]
  nb_MA <- nrow(MA_studies[!duplicated(MA_studies$ID2),])
  table_nbstudies[i,] <- list(k,nb_votes,nb_MA)
  i <- i+1
}

#### total nb of studies by indicator ####
indicator_nbstudies <- data.frame(indicator = character(),
                              votes = integer(),MA = integer(),stringsAsFactors = FALSE)
i <- 1
# gives a table with unique # studies per mechanism
for (k in levels(as.factor(studies$indicator))){
  VC_studies <- studies[studies$indicator==k,]
  nb_votes <- nrow(VC_studies[!duplicated(VC_studies$ID2),])
  MA_studies <- VC_studies[VC_studies$outMA=="in",]
  nb_MA <- nrow(MA_studies[!duplicated(MA_studies$ID2),])
  indicator_nbstudies[i,] <- list(k,nb_votes,nb_MA)
  i <- i+1
}

#### Repartition of ecological studies among taxon ####
par(mfrow=c(2,1),mar=c(2,2,1,1))
#non "recovery" or "resistance" studies (almost no corals)
taxon <- studies[!is.na(studies$taxon),]
taxon <- taxon %>%
  dplyr::filter(
    mechanism %in% c("biodiversity", "reproduction", "body condition", "genetic", "stability")
  )

general_moderator(taxon,"taxon","no")
all_taxon <- graph_indicator
#"recovery" or "resistance" studies (mostly corals)
taxon_recov <- studies[!is.na(studies$taxon),]
taxon_recov <- taxon_recov[taxon_recov$mechanism=="recovery" | taxon_recov$mechanism=="resistance",]
general_moderator(taxon_recov,"taxon")
recov_taxon <- graph_indicator
#taxon in positive direction lines
taxon_pos <- studies[!is.na(studies$taxon) & studies$direction=="positive",]
taxon_pos <- taxon_pos[taxon_pos$mechanism=="biodiversity" | taxon_pos$mechanism=="reproduction"
               | taxon_pos$mechanism=="body condition" | taxon_pos$mechanism=="genetic"
               | taxon_pos$mechanism=="stability" | taxon_pos$mechanism=="body condition"
               |taxon_pos$mechanism=="recovery" | taxon_pos$mechanism=="resistance",]
general_moderator(taxon_pos,"taxon")
pos_taxon <- graph_indicator

taxon_neg <- studies[!is.na(studies$taxon) & (studies$direction=="negative"|studies$direction=="neutral"),]
taxon_neg <- taxon_neg[taxon_neg$mechanism=="biodiversity" | taxon_neg$mechanism=="reproduction"
                       | taxon_neg$mechanism=="body condition" | taxon_neg$mechanism=="genetic"
                       | taxon_neg$mechanism=="stability" | taxon_neg$mechanism=="body condition"
                       |taxon_neg$mechanism=="recovery" | taxon_neg$mechanism=="resistance",]
general_moderator(taxon_neg,"taxon")

figure_taxon <- ggarrange(all_taxon,recov_taxon,pos_taxon,
                               ncol=3, nrow=1)
figure_taxon

#### Spatial repartition of studies ####
studies_loc <- studies[studies$continent!="mix",] #across all studies
general_moderator(studies_loc,"continent","yes")

studies_loc <- studies[studies$socioeco=="ecological" & studies$continent!="mix",]
general_moderator(studies_loc,"continent","yes")
eco_loc <- graph_indicator

studies_loc <- studies[studies$socioeco=="social"& studies$continent!="mix",]
general_moderator(studies_loc,"continent","yes")
socio_loc <- graph_indicator

studies_loc <- studies[studies$socioeco=="mitigation" & studies$continent!="mix",]
general_moderator(studies_loc,"continent")
mitigation_loc <- graph_indicator

figure_loc <- ggarrange(eco_loc,socio_loc,mitigation_loc,
                          ncol=3, nrow=1)
figure_loc

#### Vote counting results for all mechanisms, all indicators ####
votes <- studies
votes$mechanism <- as.factor(trimws(votes$mechanism))
votes$direction <- as.factor(trimws(votes$direction))
votes$indicator <- as.factor(trimws(votes$indicator))
votes <- data.table(votes)
mechanisms <- as.data.frame(table(votes$mechanism))
colnames(mechanisms) <- c("mechanism","N")
print(mechanisms) #nb of data points per mechanism

nbStudies <- votes[!duplicated(votes[,c("ID2","mechanism")]),]
table_Studies <- as.data.frame(table(nbStudies$mechanism))
print(table_Studies) #nb of study per mechanism

directions <-  votes[,.(.N),by=.(mechanism,direction)]
directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
directions$mechanism <- as.factor(directions$mechanism)

for (k in levels(directions$mechanism)){
  print(k)
  directions$proportion[directions$mechanism==k] <- round(directions$N[directions$mechanism==k] /
                                                            mechanisms[mechanisms$mechanism==k,"N"]*100,0)
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions <- directions[order(mechanism,direction),]
}
print("directions of data points by mechanism")
print(directions)

graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=mechanism)) +
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
    theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_text(aes(label=N), position=position_stack(vjust=0.5))+
    labs(x="vote counting results per mechanism")+
  coord_flip()
print(graph_indicator)


#### Vote counting results for all indicators ####
votes <- studies
votes$indicator <- as.factor(trimws(votes$indicator))
votes$direction <- as.factor(trimws(votes$direction))
votes <- data.table(votes)

indicators <- table(votes$indicator)
print(indicators) #nb of data points per indicator

direction <-  votes[,.(.N),by=.(mechanism,indicator,direction)] #directions for each indicator
for (k in levels(direction$indicator)){
  direction$proportion[direction$indicator==k] <- round(direction$N[direction$indicator==k] /indicators[k]*100,0)
  }
direction <- direction[order(mechanism,indicator),]
direction$direction <- factor(direction$direction,levels=c("positive", "neutral", "ambiguous","negative"))
direction$indicator <- as.factor(direction$indicator)

studies_dt <- data.table(studies)
mechanism_indicator <- studies_dt[,.(.N), by=.(indicator,mechanism)]
direction$indicator <- factor(
  direction$indicator,
  levels=mechanism_indicator$indicator[order(
    mechanism_indicator$mechanism
  )]
)

graph_indicator <- ggplot(direction, aes(fill=direction, y=N, x=indicator)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=N), position=position_stack(vjust=0.5), size=2)+
  labs(x="directions by indicator")+
  coord_flip()
print(graph_indicator)


#### Vote counting results for all mechanisms, selecting only some indicators ####
# only keeps indicators user rights and participation for agency
votes_selected <- studies[studies$indicator!="consultation" & studies$indicator!="decision making" &
                   studies$indicator!="governance",]
# only keeps education and environmental awareness for information
votes_selected <- votes_selected[votes_selected$indicator!="MPA knowledge" &
                            votes_selected$indicator!="research programs",]
# only keeps cpue and local food security for food security
votes_selected <- votes_selected[votes_selected$indicator!="catch" ,]

# only keeps income and costs for assets
votes_selected <- votes_selected[votes_selected$indicator!="material assets" &
                                   votes_selected$indicator!="employment",]

# only keeps allelic richness for genetic diversity
votes_selected <- votes_selected[votes_selected$indicator!="expected heterozygosity" &
                                   votes_selected$indicator!="observed heterozygosity",]

# only keeps health degradation for body condition
votes_selected <- votes_selected[votes_selected$indicator!="resistance" &
                                   votes_selected$indicator!="recovery",]

votes_selected <- votes_selected[!is.na(votes_selected$mechanism),]
votes_selected$mechanism <- as.factor(trimws(votes_selected$mechanism))
votes_selected$direction <- as.factor(trimws(votes_selected$direction))
votes_selected <- data.table(votes_selected)

## bar plot of vote counting by mechanisms
mechanisms <- as.data.frame(table(votes_selected$mechanism))
colnames(mechanisms) <- c("mechanism","N")
print(mechanisms) #nb of data points per mechanism
directions <-  votes_selected[,.(.N),by=.(mechanism,direction)]
directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
directions$mechanism <- as.factor(directions$mechanism)

for (k in levels(directions$mechanism)){
  print(k)
  directions$proportion[directions$mechanism==k] <- round(directions$N[directions$mechanism==k] /
                                                            mechanisms[mechanisms$mechanism==k,"N"]*100,0)
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions <- directions[order(mechanism,direction),]}
print("directions of data points by mechanism")
print(directions)

mechanism_order=rev(c("pH", "Cseq","allelic richness",
                      "health", "species richness", "shannon","recruitment","reproductive potential",
                      "wave attenuation","accretion","stability","costs","income", "participation",
                      "user rights","alternative livelihoods","cpue","local food security","environmental awareness",
                      "education","cohesion","conflict"))
directions$indicator <- factor(directions$indicator,levels=mechanism_order)

graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=mechanism)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=proportion), position=position_stack(vjust=0.5))+
  labs(x="vote counting results per mechanism")+
  coord_flip()
print(graph_indicator)

#### Vote counting results for selected indicators ####
indicators <- as.data.frame(table(votes_selected$indicator))
indicators$Var1 <- as.character(indicators$Var1)
print(indicators) #nb of data points per indicator

directions <-  votes_selected[,.(.N),by=.(mechanism,indicator,direction)] #directions for each indicator
directions <- data.table(directions)
for (k in levels(as.factor(directions$indicator))){
  print(k)
  directions$proportion[directions$indicator==k] <- round(directions$N[directions$indicator==k] /
                                                            indicators[indicators$Var1==k,"Freq"]*100,0)
  directions$direction <- factor(directions$direction,levels=c("positive", "neutral", "ambiguous","negative"))
  directions <- directions[order(mechanism,indicator,direction),]}
print(directions)
indicator_order=rev(c("pH", "Cseq","allelic richness",
                      "health", "species richness", "shannon","recruitment","reproductive potential",
                      "wave attenuation","accretion","stability","costs","income", "participation",
                      "user rights","alternative livelihoods","cpue","local food security","environmental awareness",
                      "education","cohesion","conflict"))
directions$indicator <- factor(directions$indicator,levels=indicator_order)

graph_indicator <- ggplot(directions, aes(fill=direction, y=N, x=indicator)) +
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c("darkolivegreen2","cornsilk2","coral","brown3"))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(aes(label=proportion), position=position_stack(vjust=0.5))+
  labs(x="directions by indicator")+
  coord_flip()
print(graph_indicator)




#### Creating the data table with all effect size ####
## adding C sequestration values
# BC ecosystems
CseqBC <- MAdata[MAdata$mechanism=="Csequestration" & MAdata$ecosystem!="sediment",]
MA <- rma(yi=ei, vi=vei, data=CseqBC)
print(MA)
forest=forest(MA, slab = paste(CseqBC$ID, CseqBC$Authors),
               showweights = TRUE)
print(forest)
ei_table <- data.frame(indicator=c("CseqBC"),ei=c(MA[["b"]]),lower=c(MA[["ci.lb"]]),
                       upper=c(MA[["ci.ub"]]), sample=c(MA[["k"]]), stringsAsFactors = FALSE)

# sediments
Cseqsed <- MAdata[MAdata$mechanism=="Csequestration" & MAdata$ecosystem=="sediment",]
MA <- rma(yi=ei, vi=vei, data=Cseqsed)
print(MA)
ei_table <- rbind(ei_table,list("Cseqsed",MA[["b"]],MA[["ci.lb"]],
                                MA[["ci.ub"]],MA[["k"]]))
forest=forest(MA, slab = paste(Cseqsed$ID, Cseqsed$Authors),
              showweights = TRUE)
print(forest)
# fish
Cseqfish <- MAdata[MAdata$mechanism=="Csequestration" & MAdata$ecosystem=="fish",]
MA <- rma(yi=ei, sei=vei, data=Cseqfish) #sei for fish values
print(MA)
ei_table <- rbind(ei_table,list("Cseqfish",MA[["b"]],MA[["ci.lb"]],
                                MA[["ci.ub"]],MA[["k"]]))
forest=forest(MA, slab = paste(Cseqfish$ID, Cseqfish$Authors),
              showweights = TRUE)
print(forest)

## Adding other MA of indicators
for (k in c("species richness","shannon","cpue","income","recruitment",
            "reproductive potential","accretion")){
Indicator <- MAdata[MAdata$indicator==k,]
MA <- rma(yi=ei, vi=vei, data=Indicator)
print(MA)
ei_table <- rbind(ei_table,list(k,MA[["b"]],MA[["ci.lb"]],
                                MA[["ci.ub"]],MA[["k"]]))
forest=forest(MA, slab = paste(Indicator$ID, Indicator$Authors),
              showweights = TRUE)
print(forest)
}

## Adding boot strapping values
# allelic richness
AR <- MAdata[MAdata$indicator=="allelic richness",]
bootstrapping(AR,1000,"allelic richness")
ARlist <- list("allelic richness",values[[1]],values[[2]],values[[3]],nrow(AR))
ei_table <- rbind(ei_table,ARlist)

# stability
var <- MAdata[MAdata$mechanism=="stability",]
bootstrapping(var,1000,"stability")
varlist <- list("stability",values[[1]],values[[2]],values[[3]],nrow(var))
ei_table <- rbind(ei_table,varlist)

#### graph with ei ####
ei_graph <- ei_table[ei_table$indicator!="accretion",]
base=6 # height of figure
expand=2 # font size increase
minLow <- round(min(ei_graph$lower,na.rm=TRUE),0)-0.5 # min x axis
maxHigh <- round(max(ei_graph$upper,na.rm=TRUE),0)+0.5 # max x axis

forestplot(ei_graph$indicator,
           mean=ei_graph$ei,lower=ei_graph$lower,upper=ei_graph$upper,
           boxsize=0.1,fn.ci_norm = fpDrawCircleCI,
           xticks = seq(minLow,maxHigh,0.5),
           txt_gp=fpTxtGp(ticks=gpar(fontfamily="",fontsize=base*expand),
                          legend = gpar(fontfamily="",fontsize=base*expand),
                          label= gpar(fontfamily="",fontsize=base*expand)),
           clip=seq(minLow,maxHigh), #clip defines the interval out of which an arrow is used to symbolize extended CI
           col=fpColors(box="blue",line="black"))

#### ei for recovery and resistance ####
table_resilience <- data.frame(mechanism=character(),ei=numeric(),lower=numeric(),
                               upper=numeric(),stringsAsFactors = FALSE)
i <- 1
for (k in c("resistance","recovery")){
  Indicator <- MAdata[MAdata$indicator==k,]
  MA <- rma(yi=ei, vi=vei, data=Indicator)
  print(MA)
  table_resilience[i,] <- list(k, MA[["b"]],MA[["ci.lb"]], MA[["ci.ub"]])
  i <- i+1
}

base=6 # height of figure
expand=2 # font size increase
minLow <- round(min(table_resilience$lower,na.rm=TRUE),0)-0.5 # min x axis
maxHigh <- round(max(table_resilience$upper,na.rm=TRUE),0)+0.5 # max x axis

forestplot(table_resilience$mechanism,
           mean=table_resilience$ei,lower=table_resilience$lower,upper=table_resilience$upper,
           boxsize=0.1,fn.ci_norm = fpDrawCircleCI,
           xticks = seq(minLow,maxHigh,0.5),
           txt_gp=fpTxtGp(ticks=gpar(fontfamily="",fontsize=base*expand),
                          legend = gpar(fontfamily="",fontsize=base*expand),
                          label= gpar(fontfamily="",fontsize=base*expand)),
           clip=seq(minLow,maxHigh), #clip defines the interval out of which an arrow is used to symbolize extended CI
           col=fpColors(box="blue",line="black"))




#### synthetic flower plot V1####
## adding values of vote counting to ei_table

# RUN: all file, and then again "vote counting for all mechanisms, selecting only
# some indicators.
flower_table <- ei_table
flower_table$MA <- rep("yes",nrow(flower_table))
flower_table <- rbind(flower_table,list("acidity buffer",0,0,0,0,"no")) #acidity buffering
flower_table <- rbind(flower_table,list("phenotypic plasticity",0,0,0,0,"no")) #phenotypic plasticity
flower_table <- rbind(flower_table,list("connectivity",0,0,0,0,"no")) #connectivity

for (k in c("body condition","agency","flexibility","learning","social organization")){
proportion <- as.numeric(directions[directions$mechanism==k &
                           directions$direction=="positive","proportion"]$proportion[1])
sample <- as.numeric(table_nbstudies[table_nbstudies$mechanism==k,"votes"])
flower_table <- rbind(flower_table,list(k,proportion, proportion,proportion,sample,"no"))
}
# finalizing flower plot table
flower_final <- flower_table[,-3]
flower_final <- flower_final[,-3]
flower_final$magnitude <- flower_final$ei
flower_final[flower_final$MA=="yes" & flower_final$ei>1,]$magnitude <- 1
#flower_final[flower_final$MA=="yes" & flower_final$ei<0,]$magnitude <- 0
flower_final[flower_final$MA=="no",]$magnitude <-
  flower_final[flower_final$MA=="no",]$ei/100
flower_final$category <- c(rep("mitigation",3),rep("ecological",2),rep("social",2),rep("ecological",5), "mitigation",rep("ecological",3),rep("social",4))
flower_final <- flower_final[order(flower_final$category),]
flower_final$confidence <- c("low")
flower_final$confidence[flower_final$MA=="yes"] <- c("medium")
flower_final$confidence[flower_final$MA=="yes" & flower_final$sample >= 15] <- c("high")
flower_final$confidence[flower_final$indicator=="Cseqfish"] <- c("high")
flower_final$confidence[flower_final$MA=="no" & flower_final$sample >= 50 ] <- c("medium")
flower_final$confidence <- factor(flower_final$confidence,levels=c("high", "medium","low"))
flower_final$logmagnitude <- c(0)
flower_final$logmagnitude[flower_final$magnitude>0] <- flower_final$magnitude[flower_final$magnitude>0]**0.5

#_create columns for bar's width
#_in x, bars start at:
flower_final$right <- (1:nrow(flower_final) + 1*c(0:(nrow(flower_final)-1))/10) #add something here to create gaps between bars
#_in x, bars end at:
flower_final$left <- flower_final$right - 1
flower_final$id <- 1:nrow(flower_final)

# Get the name and the y position of each label
label_data =data_frame(category=flower_final$indicator)
number_of_bar <- nrow(flower_final) #nb of categories
angle <- 90 - 360 * (flower_final$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$tot=flower_final$magnitude
label_data$id=flower_final$id

p <- ggplot(flower_final) +
  #Add circle line to delimitate the border of the figure.
  geom_segment(aes(x = 11.1, y = 1.1, xend = 15,
                   yend =1.1), colour = "blue",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(aes(x = 0.1, y = 1.1, xend = 11,
                   yend =1.1), colour = "darkolivegreen4",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(aes(x = 15.1, y = 1.1, xend = 21.9,
                   yend =1.1), colour = "goldenrod1",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #Add a point at the end of each barplot
  geom_point(data=flower_final, aes(x =(right+left)/2, y = logmagnitude),size = 1,color = "gray12")+
  # Add the stacked bar
  geom_rect(aes(ymin = 0, xmin = left, xmax = right, ymax = logmagnitude,
                colour = category, fill=confidence))+
  scale_colour_manual(values=c("darkolivegreen4","blue","goldenrod1"))  +
  scale_fill_manual(values=c("darkorchid","lightskyblue1","gainsboro"))  +
  ylim(-0.1,max(1.1, na.rm=T)) + # adds a hollow center
  theme_minimal() + # sets the default grey background to white
  theme(axis.text = element_blank(), #hides axis values
        axis.title = element_blank(),#hides axis title
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  #numerical scale
  annotate( x = 20,  y = 1.2,  label = "1",  geom = "text", color = "gray12") +
  annotate( x = 20,  y = 2.2,  label = "30",  geom = "text", color = "gray12") +
  annotate( x = 20,  y = 3.2,  label = "352",  geom = "text", color = "gray12") +
  coord_polar()
p

#### synthetic flower plot V2 ####
# sample size of vote counting
flower_v2 <- table_nbstudies
flower_v2 <- flower_v2[flower_v2$mechanism!="resistance" & flower_v2$mechanism!="recovery",]
flower_v2$ei <-c(0,ei_table[ei_table$indicator=="income","ei"],ei_table[ei_table$indicator=="species richness","ei"],
                 0,1,ei_table[ei_table$indicator=="CseqBC","ei"],
                 0,ei_table[ei_table$indicator=="cpue","ei"],ei_table[ei_table$indicator=="allelic richness","ei"],
                 0,ei_table[ei_table$indicator=="reproductive potential","ei"],0, ei_table[ei_table$indicator=="stability","ei"])
flower_v2$direction <- c("positive")
flower_v2$direction[c(4,9,13)] <- c("neutral")
flower_v2$direction[12] <- c("negative")
flower_v2 <- rbind(flower_v2,list("connectivity",0,"no",0,"neutral"))
flower_v2 <- rbind(flower_v2,list("phenotypic plasticity",0,"no",0,"neutral"))
flower_v2 <- rbind(flower_v2,list("acidity buffering",0,"no",0,"neutral"))
flower_v2$conf <- flower_v2$votes
flower_v2$conf[flower_v2$votes>20] <- c(20)
flower_v2$category <- c(rep("social",2),rep("ecological",3),"mitigation",rep("social",2),
                        "ecological","social","ecological","social",rep("ecological",3),"mitigation")
flower_v2 <- flower_[order(flower_v2$category),]

#_create columns for bar's width
#_in x, bars start at:
flower_v2$right <- (1:nrow(flower_v2) + 0.9) #add something here to create gaps between bars
#_in x, bars end at:
flower_v2$left <- 1:nrow(flower_v2)
flower_v2$id <- 1:nrow(flower_v2)

# Get the name and the y position of each label
label_data =data_frame(category=flower_v2$mechanism)
number_of_bar <- nrow(flower_v2) #nb of categories
angle <- 90 - 360 * (flower_v2$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data$tot=flower_v2$magnitude
label_data$id=flower_v2$id

p <- ggplot(flower_v2) +
  #Add circle line to delimitate the border of the figure.
  geom_segment(aes(x = 1.1, y = 22, xend = 8.9,
                   yend =22), colour = "darkolivegreen4",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(aes(x = 9, y = 22, xend = 10.9,
                   yend =22), colour = "blue",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(aes(x = 11, y = 22, xend = 17,
                   yend =22), colour = "goldenrod1",
               alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #Add a point at the end of each barplot
  geom_point(data=flower_v2, aes(x =(right+left)/2, y = conf),size = 1,color = "gray12")+
  # Add the stacked bar
  geom_rect(aes(ymin = 0, xmin = left, xmax = right, ymax = conf,
                colour = direction, fill=ei))+
  scale_colour_manual(values=c("red","grey", "darkolivegreen4"))  +
  scale_fill_gradient(low="white",high = "purple")  +
  ylim(-2,max(22, na.rm=T)) + # adds a hollow center
  theme_minimal() + # sets the default grey background to white
  theme(axis.text = element_blank(), #hides axis values
        axis.title = element_blank(),#hides axis title
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  #numerical scale
  #annotate( x = 15,  y = 1.2,  label = "1",  geom = "text", color = "gray12") +
  #annotate( x = 15,  y = 2.2,  label = "30",  geom = "text", color = "gray12") +
  #annotate( x = 15,  y = 3.2,  label = "352",  geom = "text", color = "gray12") +
  coord_polar()
p
#### expected resilience ####
resilience <- ei_table[ei_table$indicator=="species richness" |
                         ei_table$indicator=="shannon" |
                         ei_table$indicator=="recruitment"|
                         ei_table$indicator=="reproductive potential" |
                         ei_table$indicator=="allelic richness" |
                         ei_table$indicator=="stability",]
resilience1 <- resilience[resilience$indicator!="shannon" &
                            resilience$indicator!="recruitment",]
list_resilience <- tibble(ei=mean(resilience1$ei), CI=sd(resilience1$ei)*2/1.96)

resilience2 <- resilience[resilience$indicator!="shannon" &
                            resilience$indicator!="reproductive potential",]
list_resilience <- rbind(list_resilience, list(mean(resilience2$ei), sd(resilience2$ei)*2/1.96))

resilience3 <- resilience[resilience$indicator!="species richness" &
                            resilience$indicator!="reproductive potential",]
list_resilience <- rbind(list_resilience,list(mean(resilience3$ei), sd(resilience3$ei)*2/1.96))

resilience4 <- resilience[resilience$indicator!="species richness" &
                            resilience$indicator!="recruitment",]
list_resilience <- rbind(list_resilience,list(mean(resilience4$ei), sd(resilience4$ei)*2/1.96))

ei <- mean(resilience$ei) #moyenne des ei pondérée par sample size
bootEbars <- numeric(0) #initialisation
for (i in 1:100){ # n weighted means
  set.seed(i)
  size <- 6
  mySize <- sample(size,1)
  set.seed(size+1-i)
  newTab <- resilience[(sample(1:size,mySize,replace=T)),] #échantillonnage de données
  bootEbar <- sum(newTab$sample*newTab$ei)/sum(newTab$sample) #moyenne à partir de l'échantillon
  bootEbars <- c(bootEbars,bootEbar)  #vecteur contenant les moyennes calculées
}
hist(bootEbars)

abline(v=ei,col="red") #représentation de la moyenne de l'échantillon initial
CI=quantile(bootEbars,probs=c(2.5,97.5)/100)
minCI=CI[1]
abline(v=minCI,col="blue") #lower confidence interval
maxCI=CI[2]
abline(v=maxCI,col="blue") #upper confidence interval
print(paste("Ebar = ",ei," [",minCI,";",maxCI,"]"))

table_resilience <- rbind(table_resilience, list("expected resilience",ei,minCI,maxCI))

forestplot(table_resilience$mechanism,
           mean=table_resilience$ei,lower=table_resilience$lower,upper=table_resilience$upper,
           boxsize=0.1,fn.ci_norm = fpDrawCircleCI,
           xticks = seq(-1,1,0.5),
           txt_gp=fpTxtGp(ticks=gpar(fontfamily="",fontsize=base*expand),
                          legend = gpar(fontfamily="",fontsize=base*expand),
                          label= gpar(fontfamily="",fontsize=base*expand)),
           clip=seq(minLow,maxHigh), #clip defines the interval out of which an arrow is used to symbolize extended CI
           col=fpColors(box="black",line="black"))
