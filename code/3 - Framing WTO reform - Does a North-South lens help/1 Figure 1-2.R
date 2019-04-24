library(gtalibrary)
library(tidyverse)
library(openxlsx)
library(data.table)

rm(list=ls())

# SETWD
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud")
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
# setwd('C:/Users/Kamran/Dropbox/GTA cloud')
# setwd('D:/Dropbox/Dropbox/GTA cloud')

# CHAPTER SETTINGS
output.path <- "0 report production/GTA 24/tables & figures/3 - Framing WTO reform - Does a North-South lens help"
data.path <- "0 report production/GTA 24/data/3 - Framing WTO reform - Does a North-South lens help"
chapter.number <- 3

# LOAD THINGS
gta_colour_palette()
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

# The purpose of this chapter is to examine the degree to which the exports of different 
# groups of developing countries are harmed by different groups of implementing jurisdictions. 
# 
# The former groups should be taken to be the LDCs, AU, ACP, BRICS, non-OECD countries in 
# each of regions of the world (ie Sub Saharan Africa, MENA, Eastern Europe, South Asia, 
# East Asia, Latin America and the Caribbean). 
# 
# The latter groups should include OECD members of the G20, non-OECD members of the G20, 
# BRICS, China, EU, USA, all OECD, and all non-OECD. 


# SET COUNTRY GROUPS
correspondence <- gtalibrary::country.correspondence
countries <- gtalibrary::country.names

# FORMER GROUPS
ldc <- correspondence$un_code[correspondence$name == "LDCs" & correspondence$un_code %in% unique(countries$un_code)]
au <- correspondence$un_code[correspondence$name == "African Union" & correspondence$un_code %in% unique(countries$un_code)]
brics <- correspondence$un_code[correspondence$name %in% c("Brazil","Russia","India","South Africa","China")]
acp <- acp.members


sub.s.africa.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "Sub-Saharan Africa" & correspondence$un_code %in% unique(countries$un_code)])
mena.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "Middle East and North Africa" & correspondence$un_code %in% unique(countries$un_code)])
easteu.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "Europe and Central Asia" & correspondence$un_code %in% unique(countries$un_code)])
easteu.noecd<-setdiff(easteu.noecd, 
                      correspondence$un_code[correspondence$name == "EU-28" & correspondence$un_code %in% unique(countries$un_code)])
soasia.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "South Asia" & correspondence$un_code %in% unique(countries$un_code)])
easia.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "East Asia Pacific" & correspondence$un_code %in% unique(countries$un_code)])
latcar.noecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$name == "Latin America and the Caribbean" & correspondence$un_code %in% unique(countries$un_code)])

former.group <- list(ldc, au, brics, acp, sub.s.africa.noecd, mena.noecd, easteu.noecd, soasia.noecd, easia.noecd, latcar.noecd)
former.group.names = c("ldc", "au", "brics", "acp", "sub.s.africa.noecd", "mena.noecd", "easteu.noecd", "soasia.noecd", "easia.noecd", "latcar.noecd")

# LATTER GROUPS
g20.oecd <- correspondence$un_code[correspondence$name == "G20" & correspondence$un_code %in% oecd.members & correspondence$un_code %in% unique(countries$un_code)]
g20.noecd <- correspondence$un_code[correspondence$name == "G20" & ! correspondence$un_code %in% oecd.members & correspondence$un_code %in% unique(countries$un_code)]
eu <- correspondence$un_code[correspondence$name == "EU-28" & correspondence$un_code %in% unique(countries$un_code)]
brics <- brics
china <- 156
usa <- 840
oecd <- oecd.members
nonoecd <- unique(correspondence$un_code[! correspondence$un_code %in% oecd.members & correspondence$un_code %in% unique(countries$un_code)])

latter.group <- list(g20.oecd, g20.noecd, eu, brics, china, usa, oecd, nonoecd)
latter.group.names <- c("g20.oecd", "g20.noecd", "eu", "brics", "china", "usa", "oecd", "nonoecd")


# FIGURE 1 ######

# 1.	First, using data on harmful measures in effect today, please prepare three tables with the 
# former groups as rows and the latter groups as columns. In each cell please report the percentage 
# of the affected group’s world exports harmed by the latter group’s policies. Please use colours to 
# turn the table into a heat map (but keep the %s in the table cells visible.) The difference between 
# the three tables are the included implementer roles which are (i) importer + 3rd country, (ii) 
# importer and (iii) 3rd country.



###### IMPLEMENTER = IMPORTER + 3RD COUNTRY ######

# PREPARE DATAFRAME FIG 1.1
fig1.1 <- data.frame(affected = character(),
                   implementer = character(),
                   share = numeric())


# CALCULATE COVERAGES FIG 1.1
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
      print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
      gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                         exporters = former.group[[f]],
                         keep.exporters = T,
                         implementers = latter.group[[l]],
                         keep.implementer = T,
                         implementer.role = c("Importer","3rd country"),
                         coverage.period = c(2019,2019),
                         in.force.today = "Yes")
    
    fig1.1.temp <- data.frame(affected = former.group.names[f],
                            implementer = latter.group.names[l],
                            share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig1.1 <- rbind(fig1.1, fig1.1.temp)
    rm(fig1.1.temp)
  }
}

fig1.1.xlsx <- spread(fig1.1, affected, share)

# SAVE TABLE FIG 3.1.1
names(fig1.1.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")

fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".1.1.xlsx")

write.xlsx(fig1.1.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)

rdata.path=paste0(data.path,"/Fig",chapter.number,"-1-1.Rdata")
save(fig1.1, file=rdata.path)
load(rdata.path)

fig1.1 <- gather(fig1.1, affected, share, 2:ncol(fig1.1.0))
fig1.1$affected.num <- as.numeric(fig1.1$affected)
fig1.1$implementer.num <- as.numeric(fig1.1$implementer)

tile.labels.affected = c("LDC",
                            "AU",
                            "BRICS",
                            "ACP",
                            "Sub-Saharan Africa\n(non-OECD)",
                            "Middle East and North\nAfrica (non-OECD)",
                            "Eastern Europe\n(non-OECD)",
                            "South Asia\n(non-OECD)",
                            "East Asia\n(non-OECD)",
                            "Latin America and the\nCaribbean (non-OECD)")
tile.labels.implementer = c("G20\n(OECD)",
                         "G20\n(non-OECD)",
                         "EU",
                         "BRICS",
                         "China",
                         "USA",
                         "OECD",
                         "Non-OECD")

plot1.1 = ggplot()+
  geom_tile(data=fig1.1, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig1.1, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig1.1$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig1.1$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig1.1$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot1.1

gta_plot_saver(plot = plot1.1,
               path = paste0(output.path),
               name = "Figure 3.1.1 - implementer roles importer & 3rd country")




###### IMPLEMENTER = IMPORTER ######

# PREPARE DATAFRAME FIG 1.2
fig1.2 <- data.frame(affected = character(),
                     implementer = character(),
                     share = numeric())


# CALCULATE COVERAGES FIG 1.2
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
    print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       exporters = former.group[[f]],
                       keep.exporters = T,
                       implementers = latter.group[[l]],
                       keep.implementer = T,
                       implementer.role = c("Importer"),
                       coverage.period = c(2019,2019),
                       in.force.today = "Yes")
    
    fig1.2.temp <- data.frame(affected = former.group.names[f],
                              implementer = latter.group.names[l],
                              share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig1.2 <- rbind(fig1.2, fig1.2.temp)
    rm(fig1.2.temp)
  }
}

fig1.2.xlsx <- spread(fig1.2, affected, share)

# SAVE TABLE FIG 1.2
names(fig1.2.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")
fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".1.2.xlsx")
write.xlsx(fig1.2.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)


rdata.path=paste0(data.path,"/Fig",chapter.number,"-1-2.Rdata")
save(fig1.2, file=rdata.path)
load(rdata.path)

fig1.2$affected.num <- as.numeric(fig1.2$affected)
fig1.2$implementer.num <- as.numeric(fig1.2$implementer)

plot1.2 = ggplot()+
  geom_tile(data=fig1.2, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig1.2, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig1.2$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig1.2$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig1.2$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot1.2

gta_plot_saver(plot = plot1.2,
               path = paste0(output.path),
               name = "Figure 3.1.2 - implementer roles importer only")



###### IMPLEMENTER = 3RD COUNTRY ######

# PREPARE DATAFRAME FIG 1.3
fig1.3 <- data.frame(affected = character(),
                     implementer = character(),
                     share = numeric())


# CALCULATE COVERAGES FIG 1.3
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
    print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       exporters = former.group[[f]],
                       keep.exporters = T,
                       implementers = latter.group[[l]],
                       keep.implementer = T,
                       implementer.role = c("3rd country"),
                       coverage.period = c(2019,2019),
                       in.force.today = "Yes")
    
    fig1.3.temp <- data.frame(affected = former.group.names[f],
                              implementer = latter.group.names[l],
                              share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig1.3 <- rbind(fig1.3, fig1.3.temp)
    rm(fig1.3.temp)
  }
}

fig1.3.xlsx <- spread(fig1.3, affected, share)

# SAVE TABLE FIG 1.3
fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".1.3.xlsx")
rdata.path=paste0(data.path,"/Fig",chapter.number,"-1-3.Rdata")

names(fig1.3.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")
write.xlsx(fig1.3.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)

save(fig1.3, file=rdata.path)
load(rdata.path)

fig1.3$affected.num <- as.numeric(fig1.3$affected)
fig1.3$implementer.num <- as.numeric(fig1.3$implementer)

plot1.3 = ggplot()+
  geom_tile(data=fig1.3, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig1.3, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig1.3$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig1.3$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig1.3$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot1.3

gta_plot_saver(plot = plot1.3,
               path = paste0(output.path),
               name = "Figure 3.1.3 - implementer roles 3rd country only")



###### FIGURE 2.1 - 2.3 ######

# 2.	Second, using data on harmful measures in effect today, please prepare 
# three tables with the former groups as rows and the latter groups as columns. 
# In each cell please report the percentage of the affect former group’s exports 
# that would remain harmed if the latter group’s policies were eliminated. Please 
# use colours to turn the table into a heat map (but keep the %s in the table cells 
# visible.) The difference between the three tables are the included implementer 
# roles which are (i) importer + 3rd country, (ii) importer and (iii) 3rd country.


###### IMPLEMENTER = IMPORTER + 3RD COUNTRY ######

# PREPARE DATAFRAME FIG 2.1
fig2.1 <- data.frame(affected = character(),
                     implementer = character(),
                     share = numeric())

# CALCULATE COVERAGES FIG 2.1
# f = 1
# l = 1
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
    print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       exporters = former.group[[f]],
                       keep.exporters = T,
                       implementers = latter.group[[l]],
                       keep.implementer = F,
                       implementer.role = c("Importer","3rd country"),
                       coverage.period = c(2019,2019),
                       in.force.today = "Yes")
    
    fig2.1.temp <- data.frame(affected = former.group.names[f],
                              implementer = latter.group.names[l],
                              share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig2.1 <- rbind(fig2.1, fig2.1.temp)
    rm(fig2.1.temp)
  }
}

fig2.1.xlsx <- spread(fig2.1, affected, share)

# SAVE TABLE FIG 2.1
fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".2.1.xlsx")
rdata.path=paste0(data.path,"/Fig",chapter.number,"-2-1.Rdata")

names(fig2.1.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")
write.xlsx(fig2.1.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)

save(fig2.1, file=rdata.path)
load(rdata.path)

fig2.1$affected.num <- as.numeric(fig2.1$affected)
fig2.1$implementer.num <- as.numeric(fig2.1$implementer)

plot2.1 = ggplot()+
  geom_tile(data=fig2.1, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig2.1, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig2.1$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig2.1$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig2.1$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot2.1

gta_plot_saver(plot = plot2.1,
               path = paste0(output.path),
               name = "Figure 3.2.1 - implementer roles importer & 3rd country")


###### IMPLEMENTER = IMPORTER ######

# PREPARE DATAFRAME FIG 2.2
fig2.2 <- data.frame(affected = character(),
                     implementer = character(),
                     share = numeric())

# CALCULATE COVERAGES FIG 2.2
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
    print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       exporters = former.group[[f]],
                       keep.exporters = T,
                       implementers = latter.group[[l]],
                       keep.implementer = F,
                       implementer.role = c("Importer"),
                       coverage.period = c(2019,2019),
                       in.force.today = "Yes")
    
    fig2.2.temp <- data.frame(affected = former.group.names[f],
                              implementer = latter.group.names[l],
                              share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig2.2 <- rbind(fig2.2, fig2.2.temp)
    rm(fig2.2.temp)
  }
}

fig2.2.xlsx <- spread(fig2.2, affected, share)

# SAVE TABLE FIG 2.2
fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".2.2.xlsx")
rdata.path=paste0(data.path,"/Fig",chapter.number,"-2-2.Rdata")

names(fig2.2.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")
write.xlsx(fig2.2.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)

save(fig2.2, file=rdata.path)
load(rdata.path)

fig2.2$affected.num <- as.numeric(fig2.2$affected)
fig2.2$implementer.num <- as.numeric(fig2.2$implementer)

plot2.2 = ggplot()+
  geom_tile(data=fig2.2, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig2.2, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig2.2$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig2.2$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig2.2$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot2.2

gta_plot_saver(plot = plot2.2,
               path = paste0(output.path),
               name = "Figure 3.2.2 - implementer roles importer only")



###### IMPLEMENTER = 3RD COUNTRY ######

# PREPARE DATAFRAME FIG 2.3
fig2.3 <- data.frame(affected = character(),
                     implementer = character(),
                     share = numeric())

# CALCULATE COVERAGES FIG 2.3
for(f in 1:length(former.group)) {
  for (l in 1:length(latter.group)) {
    print(paste(f,"/",length(former.group)," - ",l,"/",length(latter.group)))
    gta_trade_coverage(gta.evaluation = c("Red","Amber"),
                       exporters = former.group[[f]],
                       keep.exporters = T,
                       implementers = latter.group[[l]],
                       keep.implementer = F,
                       implementer.role = c("3rd country"),
                       coverage.period = c(2019,2019),
                       in.force.today = "Yes")
    
    fig2.3.temp <- data.frame(affected = former.group.names[f],
                              implementer = latter.group.names[l],
                              share = trade.coverage.estimates[1,ncol(trade.coverage.estimates)])
    fig2.3 <- rbind(fig2.3, fig2.3.temp)
    rm(fig2.3.temp)
  }
}

fig2.3.xlsx <- spread(fig2.3, affected, share)

# SAVE TABLE FIG 2.3
fig.path=paste0(output.path,"/Table for Figure ",chapter.number,".2.3.xlsx")
rdata.path=paste0(data.path,"/Fig",chapter.number,"-2-3.Rdata")

names(fig2.3.xlsx) <- c("Implementer","LDCs","AU","BRICS","ACP","Sub-Saharan Africa (non-OECD)","Mena (non-OECD)","East Europe (non-OECD)","South Asia (non-OECD)","East Asia (non-OECD)","Latin America and the Caribbean (non-OECD)")
write.xlsx(fig2.3.xlsx, file=fig.path, sheetName = "Shares", col.names = T, row.names = F)

save(fig2.3, file=rdata.path)
load(rdata.path)

fig2.3$affected.num <- as.numeric(fig2.3$affected)
fig2.3$implementer.num <- as.numeric(fig2.3$implementer)

plot2.3 = ggplot()+
  geom_tile(data=fig2.3, aes(x=implementer.num, y=affected.num, fill=share), color="#FFFFFF", size=0.2)+
  geom_text(data=fig2.3, aes(x=implementer.num, y=affected.num, label=sprintf("%1.1f%%", 100*share)), color="#FFFFFF", size=3.5)+
  scale_fill_gradient(low = gta_colour$red[4], high=gta_colour$red[1])+
  labs(x="Implementing region",y="Affected region")+
  scale_y_continuous(breaks=seq(1,length(unique(fig2.3$affected)),1), labels = tile.labels.affected, sec.axis = sec_axis(~., breaks=seq(1,length(unique(fig2.3$affected)),1), labels = tile.labels.affected, name = "Affected region"))+
  scale_x_continuous(breaks=seq(1,length(unique(fig2.3$implementer)),1), labels = tile.labels.implementer)+
  guides(fill=FALSE)+
  gta_theme(x.bottom.angle = 45)+
  theme(panel.background = element_blank(), 
        panel.border=element_rect(size=1, colour="grey",fill = "transparent"), 
        legend.position="bottom",
        axis.text.x.bottom = element_text(hjust = 1))


plot2.3

gta_plot_saver(plot = plot2.3,
               path = paste0(output.path),
               name = "Figure 3.2.3 - implementer roles 3rd country only")
