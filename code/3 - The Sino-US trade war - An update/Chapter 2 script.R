library("splitstackshape")
library("xlsx")
library("foreign")
library("ggplot2")
library("scales")
library("gtable")
library("grid")
library("extrafontdb")
library("extrafont")
library("Rttf2pt1")
library("zoo")
library("gtalibrary")
library("lubridate")
library("data.table")
library("tidyverse")
rm(list = ls())

# font_import()
loadfonts(device="postscript")
loadfonts(device="win")

# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("/Users/piotrlukaszuk/Dropbox/GTA 21")
setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/")
#setwd('D:/Dropbox/Dropbox/GTA cloud')
# setwd('C:/Users/Kamran/Dropbox/GTA cloud')



#Settings
chapter.number=2
chapter.name="The Sino-US trade war - An update"
output.path=paste(chapter.number, chapter.name, sep=" - ")
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

### THE GTA standard colour palette
gta_colour_palette()

# F1: Stacked bar chart for annual average for 2013-16, annual 2017 and annual 2018 of Chinese exports to the USA affected by new harmful US measures introduced each year that
## Label 2013-16 as Obama II (2013-6). Bottom two entries in each stack should reveal extent (upper bound) of bilateral targeting to China.

figure2.1=data.frame(implementer=character(),
                     affected=character(),
                     year=character(),
                     i=character(),
                     ii=character(),
                     iii=character(),
                     iv=character()
)

trade.coverage.estimates=data.frame()

for(yr in 2013:2019){
  # prep
  f21=data.frame(implementer=c("United States of America", "China"),
                 affected=c("China","United States of America"),
                 year=yr,
                 i=NA,
                 ii=NA,
                 iii=NA,
                 iv=NA,
                 v=NA
  )
  
  
  
  yr.start=paste(yr, "-01-01",sep="")
  yr.end=paste(yr, "-12-31",sep="")
  
  t.data=paste(yr-1)
  if(t.data>=2018){
    t.data="2017"
  }
  
  for(i in 1:2){
    
    # (i) were tariffs or trade defence that only affected China,
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "inward",
                       coverage.period = c(yr,yr),
                       implementation.period = c(yr.start, yr.end),
                       implementers = as.character(f21$implementer[i]),
                       keep.implementer = T,
                       exporters = as.character(f21$affected[i]),
                       keep.exporters = T,
                       nr.exporters = c(1,1),
                       intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                       keep.type = T,
                       trade.statistic = "value",
                       trade.data = t.data,
                       intra.year.duration = F)
    
    if(nrow(trade.coverage.estimates)>0){
      f21$i[i]=trade.coverage.estimates[,4]
    } else {
      f21$i[i]=0
    }
    trade.coverage.estimates=data.frame()
    
    
    # (ii) were other harmful US policies that only affected China,
    
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "inward",
                       coverage.period = c(yr,yr),
                       implementation.period = c(yr.start, yr.end),
                       implementers = as.character(f21$implementer[i]),
                       keep.implementer = T,
                       exporters = as.character(f21$affected[i]),
                       keep.exporters = T,
                       nr.exporters = c(1,1),
                       intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                       keep.type = F,
                       trade.statistic = "value",
                       trade.data = t.data,
                       intra.year.duration = F)
    
    if(nrow(trade.coverage.estimates)>0){
      f21$ii[i]=trade.coverage.estimates[,4]
    } else {
      f21$ii[i]=0
    }
    
    trade.coverage.estimates=data.frame()
    
    
    # (iii) were tariffs or trade defence that affected China and other US trading partners,
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "inward",
                       coverage.period = c(yr,yr),
                       implementation.period = c(yr.start, yr.end),
                       implementers = as.character(f21$implementer[i]),
                       keep.implementer = T,
                       exporters = as.character(f21$affected[i]),
                       keep.exporters = T,
                       nr.exporters = c(2,999),
                       intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                       keep.type = T,
                       trade.statistic = "value",
                       trade.data = t.data,
                       intra.year.duration = F)
    
    if(nrow(trade.coverage.estimates)>0){
      f21$iii[i]=trade.coverage.estimates[,4]
    } else {
      f21$iii[i]=0
    }
    trade.coverage.estimates=data.frame()
    
    
    # (iv) were other US policies that affected China and other US trading partners.
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "inward",
                       coverage.period = c(yr,yr),
                       implementation.period = c(yr.start, yr.end),
                       implementers = as.character(f21$implementer[i]),
                       keep.implementer = T,
                       exporters = as.character(f21$affected[i]),
                       keep.exporters = T,
                       nr.exporters = c(2,999),
                       intervention.types = unique(intervention.groups$intervention.type[intervention.groups$group.name=="Tariffs and trade defence"]),
                       keep.type = F,
                       trade.statistic = "value",
                       trade.data = t.data,
                       intra.year.duration = F)
    
    if(nrow(trade.coverage.estimates)>0){
      f21$iv[i]=trade.coverage.estimates[,4]
    } else {
      f21$iv[i]=0
    }
    trade.coverage.estimates=data.frame()
    
    # (v) all US policies harming China
    gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                       affected.flows = "inward",
                       coverage.period = c(yr,yr),
                       implementation.period = c(yr.start, yr.end),
                       implementers = as.character(f21$implementer[i]),
                       keep.implementer = T,
                       exporters = as.character(f21$affected[i]),
                       keep.exporters = T,
                       
                       trade.statistic = "value",
                       trade.data = t.data,
                       intra.year.duration = F)
    
    if(nrow(trade.coverage.estimates)>0){
      f21$v[i]=trade.coverage.estimates[,4]
    } else {
      f21$v[i]=0
    }
    trade.coverage.estimates=data.frame()
    
  }
  
  f21[is.na(f21)]=0
  figure2.1=rbind(figure2.1, f21)
  
  print(yr)
}



f1316=merge(merge(aggregate(i ~ implementer + affected, subset(figure2.1, year<=2016),mean),
                  aggregate(ii ~ implementer + affected, subset(figure2.1, year<=2016),mean), by=c("implementer", "affected"), all.x=T),
            merge(aggregate(iii ~ implementer + affected, subset(figure2.1, year<=2016),mean),
                  aggregate(iv ~ implementer + affected, subset(figure2.1, year<=2016),mean), by=c("implementer", "affected"), all.x=T),
            by=c("implementer", "affected"), all.x=T)

f1316=merge(f1316, aggregate(v ~ implementer + affected, subset(figure2.1, year<=2016),mean), 
            by=c("implementer", "affected"), all.x=T)

f1316$year="2013-16"

figure2.1=rbind(f1316, subset(figure2.1, year>2016))

figure2.1.xlsx=subset(figure2.1, implementer!="China")[,c(1,2,8,3:7)]

figure2.1.xlsx=figure2.1.xlsx[order(figure2.1.xlsx$implementer),]
names(figure2.1.xlsx)=c("Implementer","Affected country", "Period","Targeted tariffs or trade defence", "Targeted non-tariff/trade defence",
                        "Untargeted tariffs or trade defence", "Untargeted non-tariff/trade defence","All US policies harming Chinese exports")

xlsx::write.xlsx(figure2.1.xlsx, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data for Figure ",chapter.number,".1.xlsx", sep=""), row.names=F)


# plot here

figure2.1.plot <- gather(figure2.1.xlsx,type, value, c(4:8))

plot2.1 <- ggplot()+
  geom_bar(data=figure2.1.plot, aes(x=forcats::fct_inorder(type), y=value/1000000000, fill=Period), stat = "identity", position=position_dodge(0.8), width=0.7) +
  scale_fill_manual(labels=c("Obama II", "2017","2018","2019"),values=c(gta_colour$qualitative[c(1,3,5,7)])) +
  scale_y_continuous(breaks=seq(0,400,50), limits = c(0,401), sec.axis = sec_axis(~., name = "Billions of USD of Chinese exports affected",breaks=seq(0,400,50)))+
  scale_x_discrete(labels=c("Tariff increases \ntargeting \nChina",
                            "Other US\ndistortions targeting\nChina",
                            "Tariff increases\naffecting but\nnot targeting\nChina",
                            "Other US distortions\naffecting but\nnot targeting\nChina",
                            "All US policies\n harming \nChinese exports"))+
  xlab("Intervention type")+
  ylab("Billions of USD of Chinese exports affected")+
  guides(fill=guide_legend(title="Period", ncol=3))+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(hjust = 0.5))+
  theme(axis.text.x.bottom = element_text(size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10))

plot2.1

# plot2.1 <- ggplot()+
#   geom_bar(data=figure2.1.plot, aes(x=Period, y=value/1000000000, fill=forcats::fct_rev(type)), stat = "identity") +
#   scale_fill_manual(values=c(gta_colour$blue[2:1],gta_colour$red[1:2])) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Value of chinese exports to the \nUSA affected (in billions)"))+
#   scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
#   xlab("Year")+
#   ylab("Value of chinese exports to the \nUSA affected (in Billions)")+
#   guides(fill=guide_legend(title=NULL, ncol=2))+
#   gta_theme()

gta_plot_saver(plot=plot2.1,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".1", sep=""))



# F2: Like F1 but for US exports to China.
figure2.2.xlsx=subset(figure2.1, implementer=="China")[,c(1,2,8,3:7)]

figure2.2.xlsx=figure2.2.xlsx[order(figure2.2.xlsx$implementer),]
names(figure2.2.xlsx)=c("Implementer","Affected country", "Period","Targeted tariffs or trade defence", "Targeted non-tariff/trade defence",
                        "Untargeted tariffs or trade defence", "Untargeted non-tariff/trade defence","All Chinese policies harming US exports")

xlsx::write.xlsx(figure2.2.xlsx, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".2 - Data for Figure ",chapter.number,".2.xlsx", sep=""), row.names=F)

# plot here

figure2.2.plot <- gather(figure2.2.xlsx,type, value, 4:8)

plot2.2 <- ggplot()+
  geom_bar(data=figure2.2.plot, aes(x=forcats::fct_inorder(type), y=value/1000000000, fill=Period), stat = "identity", position=position_dodge(0.8), width=0.7) +
  scale_fill_manual(labels=c("Obama II", "2017","2018","2019"),values=c(gta_colour$qualitative[c(1,3,5,7)])) +
  scale_y_continuous(breaks=seq(0,400,25), sec.axis = sec_axis(~., name = "Billions of USD of US exports affected",breaks=seq(0,400,25)))+
  scale_x_discrete(labels=c("Tariff increases \ntargeting\nthe USA",
                            "Other Chinese\ndistortions targeting\nthe USA",
                            "Tariff increases\n affecting but\nnot targeting\nthe USA",
                            "Other Chinese\ndistortions affecting \nbut not targeting\nthe USA",
                            "All Chinese policies \nharming US exports"))+
  xlab("Intervention type")+
  ylab("Billions of USD of US exports affected")+
  guides(fill=guide_legend(title="Period", ncol=3))+
  gta_theme(x.bottom.angle = 0)+
  theme(axis.text.x.bottom = element_text(hjust = 0.5))+
  theme(axis.text.x.bottom = element_text(size=10),
        axis.title.y.left = element_text(size=10),
        axis.title.y.right = element_text(size=10))

plot2.2

# plot2.2 <- ggplot()+
#   geom_bar(data=figure2.2.plot, aes(x=Period, y=value/1000000000, fill=forcats::fct_rev(type)), stat = "identity") +
#   scale_fill_manual(values=c(gta_colour$blue[2:1],gta_colour$red[1:2])) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Value of US exports to \nChina affected (in billions)"))+
#   scale_x_discrete(labels = c("Obama II", "2017", "2018"))+
#   xlab("Year")+
#   ylab("Value of US exports to \nChina affected (in Billions)")+
#   guides(fill=guide_legend(title=NULL, ncol=2))+
#   gta_theme()

gta_plot_saver(plot=plot2.2,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".2", sep=""))




# F3: Bar chart with total amount of Chinese exports to the USA affected by US harmful policy interventions of any type in force at end of Obama administration (19 Jan 2017), at end of 31 December 2017, and on 31 October 2018.
## As percentage of total 2016 US imports from China

figure2.3=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year","Trump 3rd year"),
                     end.date=c("2017-01-19", "2017-12-31","2018-12-31", cutoff),
                     nr.interventions=NA,
                     trade.value=NA,
                     trade.value.1=NA,
                     trade.value.2=NA,
                     trade.value.3=NA,
                     trade.value.4=NA,
                     trade.value.5=NA,
                     trade.share=NA)

for(i in 1:nrow(figure2.3)){
  # (i) were tariffs or trade defence that only affected China,
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(figure2.3$end.date[i]),year(figure2.3$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(figure2.3$end.date[i])),
                     implementers = "United States of America",
                     keep.implementer = T,
                     exporters =  "China",
                     keep.exporters = T,
                     trade.statistic = "value",
                     trade.data = "2017",
                     intra.year.duration = F,
                     hit.brackets = c(1,1,2,2,3,3,4,4,5,99999999))
  
  master.sliced=subset(master.sliced, i.un==840)
  
  figure2.3$nr.interventions[i]=length(unique(master.sliced$intervention.id))
  figure2.3$trade.value[i]=sum(trade.coverage.estimates[,4])
  figure2.3$trade.value.1[i]=sum(trade.coverage.estimates[1,4])
  figure2.3$trade.value.2[i]=sum(trade.coverage.estimates[2,4])
  figure2.3$trade.value.3[i]=sum(trade.coverage.estimates[3,4])
  figure2.3$trade.value.4[i]=sum(trade.coverage.estimates[4,4])
  figure2.3$trade.value.5[i]=sum(trade.coverage.estimates[5,4])
  
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(figure2.3$end.date[i]),year(figure2.3$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(figure2.3$end.date[i])),
                     importers = "United States of America",
                     keep.importers = T,
                     implementers = "United States of America",
                     keep.implementer = T,
                     exporters =  "China",
                     keep.exporters = T,
                     trade.statistic = "share",
                     trade.data = "2017",
                     intra.year.duration = F)
  
  figure2.3$trade.share[i]=trade.coverage.estimates[,4]
  
  rm(master.sliced, trade.coverage.estimates)
}
figure2.3[is.na(figure2.3)]=0


figure2.3.xlsx=figure2.3[,c(1:4,10,5:9)]

names(figure2.3.xlsx)=c("US administration","Cut-off date","Number of harmful interventions imposed affecting China",
                        "Value of 2017 imports on affected tariff lines (any intensity)", 
                        "Share of affected in total Chinese 2017 exports to the USA",
                        "Value of 2017 imports affected by 1 intervention",
                        "Value of 2017 imports affected by 2 interventions",
                        "Value of 2017 imports affected by 3 interventions",
                        "Value of 2017 imports affected by 4 interventions",
                        "Value of 2017 imports affected by 5 or more interventions")


xlsx::write.xlsx(figure2.3.xlsx, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".3 - Data for Figure ",chapter.number,".3.xlsx", sep=""), row.names=F)

# plot here
# RESHAPE SET
figure2.3.plot <- gather(figure2.3, type, value, 4:ncol(figure2.3))

plot2.3 <- ggplot()+
  geom_bar(data=subset(figure2.3.plot, ! type %in% c("trade.share","trade.value")), aes(x=administration, y=value/1000000000, fill=type), stat = "identity", width=0.6) +
  geom_line(data=subset(figure2.3.plot, type == "trade.share"), aes(x=administration, y=value*500, group=1),colour=gta_colour$qualitative[6],size=1)+
  geom_text(data=subset(figure2.3.plot, type == "trade.share"), aes(x=administration, y=value*500, label=round(value, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,500,50), limits = c(0,500),sec.axis = sec_axis(~.*(1/500), name = "Share of Chinese exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
  scale_fill_manual(values = gta_colour$qualitative[c(5,4,3,2,1)], labels=c("1","2","3","4","5 or more"))+
  xlab("Period")+
  ylab("Chinese exports affected (billion US Dollars)")+
  guides(fill=guide_legend(title="Number of interventions \naffecting trade", ncol=3,title.position = "top"))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12)
        )

plot2.3

gta_plot_saver(plot=plot2.3,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".3", sep=""))






# F4: Like F1 but for US exports to China.
figure2.4=data.frame(administration=c("Obama II","Trump 1st year", "Trump 2nd year","Trump 3rd year"),
                     end.date=c("2017-01-19", "2017-12-31","2018-12-31", cutoff),
                     nr.interventions=NA,
                     trade.value=NA,
                     trade.value.1=NA,
                     trade.value.2=NA,
                     trade.value.3=NA,
                     trade.value.4=NA,
                     trade.value.5=NA,
                     trade.share=NA)

for(i in 1:nrow(figure2.4)){
  # (i) were tariffs or trade defence that only affected China,
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(figure2.4$end.date[i]),year(figure2.4$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(figure2.4$end.date[i])),
                     implementers = "China",
                     keep.implementer = T,
                     exporters =  "United States of America",
                     keep.exporters = T,
                     trade.statistic = "value",
                     trade.data = "2017",
                     intra.year.duration = F,
                     hit.brackets = c(1,1,2,2,3,3,4,4,5,99999999))
  
  master.sliced=subset(master.sliced, i.un==156)
  
  figure2.4$nr.interventions[i]=length(unique(master.sliced$intervention.id))
  figure2.4$trade.value[i]=sum(trade.coverage.estimates[,4])
  figure2.4$trade.value.1[i]=sum(trade.coverage.estimates[1,4])
  figure2.4$trade.value.2[i]=sum(trade.coverage.estimates[2,4])
  figure2.4$trade.value.3[i]=sum(trade.coverage.estimates[3,4])
  figure2.4$trade.value.4[i]=sum(trade.coverage.estimates[4,4])
  figure2.4$trade.value.5[i]=sum(trade.coverage.estimates[5,4])
  
  
  gta_trade_coverage(gta.evaluation = c("Red", "Amber"),
                     affected.flows = "inward",
                     coverage.period = c(year(figure2.4$end.date[i]),year(figure2.4$end.date[i])),
                     implementation.period = c("2008-11-01", as.character(figure2.4$end.date[i])),
                     importers = "China",
                     keep.importers = T,
                     implementers = "China",
                     keep.implementer = T,
                     exporters =  "United States of America",
                     keep.exporters = T,
                     trade.statistic = "share",
                     trade.data = "2017",
                     intra.year.duration = F)
  
  figure2.4$trade.share[i]=trade.coverage.estimates[,4]
  
  rm(master.sliced, trade.coverage.estimates)
}
figure2.4[is.na(figure2.4)]=0


figure2.4.xlsx=figure2.4[,c(1:4,10,5:9)]

names(figure2.4.xlsx)=c("US administration","Cut-off date","Number of harmful interventions imposed affecting USA",
                        "Value of 2017 imports on affected tariff lines (any intensity)", 
                        "Share of affected in total American 2017 exports to China",
                        "Value of 2017 imports affected by 1 intervention",
                        "Value of 2017 imports affected by 2 interventions",
                        "Value of 2017 imports affected by 3 interventions",
                        "Value of 2017 imports affected by 4 interventions",
                        "Value of 2017 imports affected by 5 or more interventions")


xlsx::write.xlsx(figure2.4.xlsx, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".4 - Data for Figure ",chapter.number,".4.xlsx", sep=""), row.names=F)

# plot here
# RESHAPE SET
figure2.4.plot <- gather(figure2.4, type, value, 4:ncol(figure2.4))

plot2.4 <- ggplot()+
  geom_bar(data=subset(figure2.4.plot, ! type %in% c("trade.share","trade.value")), aes(x=administration, y=value/1000000000, fill=type), stat = "identity", width=0.6) +
  geom_line(data=subset(figure2.4.plot, type == "trade.share"), aes(x=administration, y=value*150, group=1),colour=gta_colour$qualitative[6],size=1)+
  geom_text(data=subset(figure2.4.plot, type == "trade.share"), aes(x=administration, y=value*150, label=round(value, digits = 3)), nudge_y = -20, size=3.5, colour="#FFFFFF")+
  scale_y_continuous(breaks=seq(0,150,50), limits = c(0,150),sec.axis = sec_axis(~.*(1/150), name = "Share of US exports affected"))+
  scale_x_discrete(labels = c("Obama II", "2017", "2018", "2019"))+
  scale_fill_manual(values = gta_colour$qualitative[c(5,4,3,2,1)], labels=c("1","2","3","4","5 or more"))+
  xlab("Period")+
  ylab("US exports affected (billion US Dollars)")+
  guides(fill=guide_legend(title="Number of interventions \naffecting trade", ncol=3,title.position = "top"))+
  gta_theme()+
  theme(axis.text.x.bottom = element_text(size=12),
        axis.title.y.left = element_text(size=12),
        axis.title.y.right = element_text(size=12)
  )

plot2.4

gta_plot_saver(plot=plot2.4,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name=paste("Figure ",chapter.number,".4", sep=""))


## Value of Chinese retaliatory tariff waiver (intervention ID 69501)

gta_trade_coverage(intervention.id=69501, 
                   keep.intervention=T, 
                   coverage.period=c(2019,2019),
                   trade.data = "2017",
                   trade.statistic = "value",
                   intra.year.duration = F)


# TABLE

# Please produce a table of the US measures taken in 2019 that 
# affect Chinese interests (distinguishing between harmful and 
# liberalising) and visa versa. Please create columns for the number 
# of interventions taken by China and the USA this year (i) only affect 
# each other, (ii) affect the other as one among several additional 
# affected trading partners


implementer <- c("United States of America", "China")
affected <- c("China","United States of America")

## targeted
gta_data_slicer(gta.evaluation = c("Red", "Amber","Green"),
                affected.flows = "inward",
                implementation.period = c("2019-01-01", cutoff),
                implementing.country = implementer,
                keep.implementer = T,
                affected.country = affected,
                keep.affected = T,
                nr.affected = c(1,1),
                keep.implementation.na=F
)

master.sliced=subset(master.sliced, i.un %in% c(156,840) & a.un %in% c(156,840))
master.sliced$gta.evaluation[master.sliced$gta.evaluation != "Green"] <- "Harmful"
master.sliced$gta.evaluation[master.sliced$gta.evaluation == "Green"] <- "Liberalising"
master.sliced$implementer="USA"
master.sliced$implementer[master.sliced$i.un==156]="China"
master.sliced$affected="USA"
master.sliced$affected[master.sliced$a.un==156]="China"

master.xlsx <- aggregate(intervention.id~gta.evaluation + implementer + affected, master.sliced, function(x) length(unique(x)))

## reformat here.
names(master.xlsx) <- c("Evaluation","Implementer","Affected","Interventions")
xlsx::write.xlsx(master.xlsx, file=paste0("0 report production/GTA 24/tables & figures/",output.path,"/Table ",chapter.number,".1 - China vs USA interventions in 2019.xlsx"),sheetName = "Targeted", append=F, row.names = F)


## untargeted
gta_data_slicer(gta.evaluation = c("Red", "Amber","Green"),
                affected.flows = "inward",
                implementation.period = c("2019-01-01", cutoff),
                implementing.country = implementer,
                keep.implementer = T,
                affected.country = affected,
                keep.affected = T,
                nr.affected = c(2,999),
                keep.implementation.na=F
)

master.sliced=subset(master.sliced, i.un %in% c(156,840) & a.un %in% c(156,840))
master.sliced$gta.evaluation[master.sliced$gta.evaluation != "Green"] <- "Harmful"
master.sliced$gta.evaluation[master.sliced$gta.evaluation == "Green"] <- "Liberalising"
master.sliced$implementer="USA"
master.sliced$implementer[master.sliced$i.un==156]="China"
master.sliced$affected="USA"
master.sliced$affected[master.sliced$a.un==156]="China"

master.xlsx <- aggregate(intervention.id~gta.evaluation + implementer + affected, master.sliced, function(x) length(unique(x)))

## reformat here.
names(master.xlsx) <- c("Evaluation","Implementer","Affected","Interventions")
xlsx::write.xlsx(master.xlsx, file=paste0("0 report production/GTA 24/tables & figures/",output.path,"/Table ",chapter.number,".1 - China vs USA interventions in 2019.xlsx"),sheetName = "Untargeted", append=T, row.names = F)

