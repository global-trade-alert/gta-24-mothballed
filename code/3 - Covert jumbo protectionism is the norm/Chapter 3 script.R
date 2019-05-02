rm(list=ls())

library(gtalibrary)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(dplyr)

#setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.number = 3
chapter.title = 'Covert jumbo protectionism is the norm'

output.path = paste(chapter.number, chapter.title, sep=' - ')

load('data/master_plus.Rdata')
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")
load("data/support tables/Final goods support table.Rdata")

jumbo.threshold.1 = 10e9
jumbo.threshold.2 = 100e9


# coverage by intervention computations -----------------------------------

## importing trade data

trade=subset(final, Year %in% c(2005:2007))[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")

trade=aggregate(trade.value ~ i.un + a.un + affected.product, trade, sum)
trade$trade.value= trade$trade.value/3 


## preparing GTA data
gta_data_slicer(gta.evaluation = c("red","amber"),
                implementation.period = c("2008-11-01",cutoff),
                keep.implementation.na = F)

coverage.by.intervention=unique(master.sliced[,c("intervention.id","date.implemented","currently.in.force")])
coverage.by.intervention$year.implemented=year(coverage.by.intervention$date.implemented)
coverage.by.intervention$date.implemented=NULL
coverage.by.intervention$value.usd=NA
coverage.by.intervention$found.trade=T



master.temp=subset(master.sliced, intervention.id %in% coverage.by.intervention$intervention.id)

## Generating the base file 
gta_imp_exp_hs_tuples(master.path="master.temp",
                      master.data.frame = T)

master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))

coverage.by.intervention$value.usd=NULL

coverage.by.intervention=merge(coverage.by.intervention, aggregate(trade.value ~ intervention.id, master.tuple, sum),by="intervention.id", all.x=T)
coverage.by.intervention$found.trade[is.na(coverage.by.intervention$trade.value)]=F
coverage.by.intervention$trade.value[is.na(coverage.by.intervention$trade.value)]=0

trade.coverage.base = subset(coverage.by.intervention, found.trade==T)

## different subsets
ids.all=unique(master.sliced$intervention.id)
ids.conservative=unique(subset(master.sliced, implementation.level %in% c("national", "supranational") &
                                          eligible.firms %in% c("all", "sector-specific"))$intervention.id)

### XLSX specs
thresholds = c(0,1e7,1e8,1e9,1e10,1e11,1e12,max(trade.coverage.base$trade.value)+1)
trade.thresholds.by.year = data.frame(Lower.threshold = thresholds[-length(thresholds)], Upper.threshold = thresholds[-1])
year.range = 2008:2019

# PDF/CDF plots -----------------------------------------------------------------

gta_colour_palette()

for(approach in c("all", "conservative", "non-conservative")){
  
  if(approach=="all"){
    ids=ids.all
    
    cdf.file.name="All interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="All interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/All interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  if(approach=="conservative"){
    ids=ids.conservative
    
    cdf.file.name="Conservative interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="Conservative interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/Conservative interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  if(approach=="non-conservative"){
    ids=setdiff(ids.all,ids.conservative)
    
    cdf.file.name="Non-conservative interventions - CDF of harmful intervention trade coverage"
    pdf.file.name="Non-conservative interventions - PDF of harmful intervention trade coverage"
    table.path = paste0('0 report production/GTA 24/tables & figures/', output.path, '/Non-conservative interventions - Interventions by year and affected trade thresholds.xlsx')
    
  }
  
  ## loop data
  loop.data=subset(trade.coverage.base, intervention.id %in% ids)
  
  ## ecdf 
  log10.cdf=ggplot(loop.data, aes(x=log10(trade.value))) + 
    stat_ecdf(geom = "step", position = "identity") + xlab('Log10 scale - Trade value in USD') + ylab('Fraction of Data') + 
    ggtitle('CDF of the value of trade harmed by harmful interventions implemented 2008-2019') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 5:12,labels=paste0('10e',5:12))+ 
    gta_theme()
  
  log10.cdf

  gta_plot_saver(plot=log10.cdf,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name=cdf.file.name)
  
    
  ## PDF (final version)
  trade.values.war.us <- trade.coverage.base[trade.coverage.base$intervention.id %in% trade.war.us,]$trade.value
  trade.values.war.chn <- trade.coverage.base[trade.coverage.base$intervention.id %in% trade.war.chn,]$trade.value
  trade.values.war.us = log10(trade.values.war.us)
  trade.values.war.chn = log10(trade.values.war.chn)
  
  log10.pdf = ggplot() + 
    geom_density(data=loop.data,aes(x=log10(trade.value)), size=1) + xlab('Trade value in USD') + ylab('Probability Density') + 
    ggtitle('PDF of the value of trade harmed by harmful interventions implemented 2008-2019') + 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = 5:12,labels=paste0('10e',5:12), limits=c(5,12))+ 
    gta_theme() +
    theme(plot.title = element_text(size = 11)) +
    geom_vline(aes(xintercept = trade.values.war.us, color = gta_colour$qualitative[6]), size=0.7,linetype='twodash', show.legend = TRUE) +
    geom_vline(aes(xintercept = trade.values.war.chn, color = gta_colour$qualitative[3]), size=0.7,linetype='twodash', show.legend = TRUE) +
    scale_color_manual(name='',values=gta_colour$qualitative[c(7,2)],labels=c('China harmful interventions \nimplemented in 2018 trade war with the US','US harmful interventions \nimplemented in 2018 trade war with China')) 
  
  log10.pdf
  
  gta_plot_saver(plot=log10.pdf,
                 path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
                 name=pdf.file.name)
  
  
  ## XLSX
  # an XSLX with summary stats about how many interventions affected between x1 and x2 worth of trade for several brackets eg. less than 1bn, 1-2bn, 2-3bn or so
  # choose those brackets as they make sense
  
  for (i in 1:nrow(trade.thresholds.by.year)){
    for (year in 1:length(year.range)){
      
      trade.thresholds.by.year[i,year+2] = length(which(loop.data[loop.data$year.implemented==year.range[year],]$trade.value > trade.thresholds.by.year$Lower.threshold[i] & 
                                                          loop.data[loop.data$year.implemented==year.range[year],]$trade.value < trade.thresholds.by.year$Upper.threshold[i])) 
      
      names(trade.thresholds.by.year)[year+2] = paste(as.character(year.range[year]),'Number of interventions harming trade between threshold values' )
      
    }
  }
  
  colnames(trade.thresholds.by.year)[1:2] = c('Lower Threshold', 'Upper Threshold')
  
  class(trade.thresholds.by.year$`Lower Threshold`) <- "scientific"
  class(trade.thresholds.by.year$`Upper Threshold`) <- "scientific"
  
  loop.data=loop.data[order(loop.data$trade.value, decreasing=T),c('intervention.id','year.implemented','trade.value')]
  
  xlsx::write.xlsx(trade.thresholds.by.year, file=table.path, row.names = F, sheetName = "Harmed trade" )
  xlsx::write.xlsx(trade.thresholds.by.year, file=table.path, row.names = F, sheetName = "Underlying data", append=T)
  
}

#  Task 1 ---------------------------------------------------------------------
# SE request: For the discriminatory measures imposed over the past 10 years, a bar chart showing the frequency of measures harming 10 billion USD 
# or more of trade would be produced. The trade war interventions (enumerated in chapter 2, stored in the GTA 24’s definitions file) 
# would be highlighted and compared to other jumbo measures.

trade.war.state.ids = c(china.us.trade.war.act.ids, eu.sg.steel.act.ids)
trade.war.intervention.ids = c(new.actions.intervention.ids ,china.us.trade.war.intervention.ids)

gta_data_slicer()
trade.war.df = subset(master.sliced, (state.act.id %in% trade.war.state.ids)|(intervention.id %in% trade.war.intervention.ids),select=c(intervention.id))

# state.ids.df=master.sliced[(master.sliced$state.act.id %in% trade.war.state.ids),]
# int.ids.df = master.sliced[master.sliced$intervention.id %in% trade.war.intervention.ids,]
# state.ids.df = rbind(state.ids.df,int.ids.df)
# state.ids.df = state.ids.df[state.ids.df$gta.evaluation == 'Red',]
# trade.war.us= unique(state.ids.df[state.ids.df$implementing.jurisdiction == 'United States of America',]$intervention.id)
# trade.war.chn= unique(state.ids.df[state.ids.df$implementing.jurisdiction == 'China',]$intervention.id)
# trade.war.int.ids = unique(state.ids.df$intervention.id)

## plotting
## For the discriminatory measures imposed over the past 10 years, a bar chart showing the frequency of measures harming 10 billion USD or more of trade would be produced. 
## The trade war interventions (enumerated in chapter 2, stored in the GTA 24’s definitions file) would be highlighted and compared to other jumbo measures.

## please replace these with the relevant entry from the defintion.R file
trade.war.us <- c(56890, 56823, 63051, 57917, 62073)
trade.war.chn <- c(63064, 62226, 62411)
trade.war.int.ids <- c(56890, 56823, 63051, 63064, 57917, 62073, 62226, 62411, 61213, 71656, 71661, 71655, 71660)


threshold = jumbo.threshold.1

annual.jumbos=aggregate(intervention.id ~ year.implemented, subset(trade.coverage.base, trade.value>=threshold & !intervention.id %in% trade.war.int.ids), function(x) length(unique(x)))
annual.jumbos$intervention.status="2"
tw.jumbos=aggregate(intervention.id ~ year.implemented, subset(trade.coverage.base, trade.value>=threshold & intervention.id %in% trade.war.int.ids), function(x) length(unique(x)))
tw.jumbos$intervention.status="1"
annual.jumbos=rbind(tw.jumbos,annual.jumbos)

fig.1 =ggplot(annual.jumbos, aes(x=year.implemented,y=intervention.id,fill=intervention.status)) + geom_col() + 
  scale_x_continuous(breaks=2008:2019,labels=2008:2019) + xlab('Year of implementation of the harmful intervention') +
  ylab(paste('Number of interventions harming trade for over 10 bln USD')) +
  scale_fill_manual(name='',values = c(gta_colour$qualitative[2],gta_colour$qualitative[1]), labels=c('Trade war interventions','Non Trade war interventions')) +
  gta_theme()

fig.1

gta_plot_saver(plot=fig.1,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name= paste(chapter.number,".1 ","Number of interventions per year harming trade for over 10b USD",sep=''))

annual.jumbos$intervention.status[annual.jumbos$intervention.status=="1"]="Trade war"
annual.jumbos$intervention.status[annual.jumbos$intervention.status=="2"]="Non-trade war"
xlsx::write.xlsx(annual.jumbos, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".1 ","Number of interventions per year harming trade for over 10b USD.xlsx",sep=''))



# Task 2 ------------------------------------------------------------------
# SE request: Please prepare a pie chart of the types of jumbo protectionist measures by MAST category.
# SE never specified a threshold and keeps going back and forward so i just did it for both thresholds of 10b and 100b

## for 100e9 threshold

mast.trade.coverage.base=merge(subset(trade.coverage.base,trade.value >= jumbo.threshold.2),
                               unique(master.sliced[,c('intervention.id','mast.chapter')]), 
                               by="intervention.id", all.x=T)
fig.2.data=aggregate(intervention.id ~ mast.chapter, mast.trade.coverage.base, function(x) length(unique(x)))
fig.2.data$value=fig.2.data$intervention.id/sum(fig.2.data$intervention.id)*100
fig.2.data$col = gta_colour$qualitative[1:nrow(fig.2.data)]

#changing names to fit your ggplot code.
setnames(fig.2.data, "intervention.id","value")
setnames(fig.2.data, "mast.chapter","group")
fig.2.data$group = factor(fig.2.data$group,levels=fig.2.data$group)



# mast.trade.coverage.base = merge(trade.coverage.base, master.sliced[!duplicated(master.sliced[,c('intervention.id','mast.chapter')]),
#                                                                                    c('intervention.id','mast.chapter')], by ='intervention.id')
# mast.trade.coverage.base = mast.trade.coverage.base[mast.trade.coverage.base$trade.value > jumbo.threshold.2,]$mast.chapter
# 
# fig.2.data = data.frame(group=mast.trade.coverage.base)
# fig.2.data$count = 1
# fig.2.data = aggregate(count~group,fig.2.data,sum)
# fig.2.data$value = fig.2.data$count/sum(fig.2.data$count)*100
# fig.2.data = fig.2.data[order(fig.2.data$value,decreasing=T),]
# fig.2.data$col = gta_colour$qualitative[1:nrow(fig.2.data)]
# fig.2.data$group = factor(fig.2.data$group,levels=fig.2.data$group)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


fig.2  <- ggplot(fig.2.data, aes(x="", y=value, fill=group)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=4) + blank_theme + gta_theme() + 
  scale_fill_manual(name='',values=fig.2.data$col) + xlab('') + ylab('') +
  theme(axis.ticks=element_blank(),panel.border = element_blank(),panel.grid=element_blank()) +   theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.6,y = value,label = ifelse(value>0,scales::percent(value/100),'')), size=1.6,position = position_stack(vjust = 0.5))
  

fig.2

gta_plot_saver(plot=fig.2,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name= paste(chapter.number,".2 ",jumbo.threshold.2," jumbo threshold MAST Chapter distribution of interventions harming trade",sep=''))

fig.2.data = fig.2.data[,!(colnames(fig.2.data) %in% c('col'))]
names(fig.2.data) = c('Mast Chapter','Count','Percentage') 
xlsx::write.xlsx(fig.2.data, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".2 ",jumbo.threshold.2,"jumbo threshold MAST Chapter distribution of interventions harming trade.xlsx",sep=''))

# SE: please do make a pie chart for the values between 10e9 and 100e9 
## between 10e9 - 100e9 threshold


mast.trade.coverage.base = merge(trade.coverage.base, master[!duplicated(master[,c('intervention.id','mast.chapter')]),
                                                                                   c('intervention.id','mast.chapter')], by ='intervention.id')
mast.trade.coverage.base = mast.trade.coverage.base[(mast.trade.coverage.base$trade.value > jumbo.threshold.1) &
                                                                            (mast.trade.coverage.base$trade.value < jumbo.threshold.2) ,]$mast.chapter

fig.2.data = data.frame(group=mast.trade.coverage.base)
fig.2.data$count = 1
fig.2.data = aggregate(count~group,fig.2.data,sum)
fig.2.data$value = fig.2.data$count/sum(fig.2.data$count)*100
fig.2.data = fig.2.data[order(fig.2.data$value,decreasing=T),]
fig.2.data$col = colorRampPalette(gta_colour$qualitative)(nrow(fig.2.data))
fig.2.data$group = factor(fig.2.data$group,levels=fig.2.data$group)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


fig.2  <- ggplot(fig.2.data, aes(x="", y=value, fill=group)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=4) + blank_theme + gta_theme() + 
  scale_fill_manual(name='',values=fig.2.data$col) + xlab('') + ylab('') +
  theme(axis.ticks=element_blank(),panel.border = element_blank(),panel.grid=element_blank()) +   theme(axis.text.x=element_blank()) +
  geom_text(aes(x=1.6,y = value,label = ifelse(value>=1,scales::percent(value/100),'')), size=1.6,position = position_stack(vjust = 0.5))


fig.2

gta_plot_saver(plot=fig.2,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name= paste(chapter.number,".2 ","between 10b-100b threshold MAST Chapter distribution of interventions harming trade",sep=''))

fig.2.data = fig.2.data[,!(colnames(fig.2.data) %in% c('col'))]
names(fig.2.data) = c('Mast Chapter','Count','Percentage') 
xlsx::write.xlsx(fig.2.data, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".2 ","between",jumbo.threshold.1,' - ',jumbo.threshold.2,"threshold MAST Chapter distribution of interventions harming trade.xlsx",sep=''))


# Task 3 ------------------------------------------------------------------

# SE request: Please compute the number of jumbo protectionist measures that affect only one trading partner.

gta_data_slicer(gta.evaluation=c('Red','Amber'),keep.implementation.na=F,nr.affected=c(1,1))
master.sliced=master.sliced[!is.na(master.sliced$affected.jurisdiction),]
unique.affected.partner.interventions = unique(master.sliced$intervention.id)

unique.affected.partner.jumbo = trade.coverage.base[trade.coverage.base$intervention.id %in% unique.affected.partner.interventions,]

total.unique.affected.partner.jumbo = data.frame(thresholds = c('10b','100b'), 
                                                 'Number of remaining protectionist measures affecting one partner' = 
                                                   c(length(which(unique.affected.partner.jumbo$trade.value > jumbo.threshold.1)),
                                                     length(which(unique.affected.partner.jumbo$trade.value > jumbo.threshold.2))))  

  
xlsx::write.xlsx(total.unique.affected.partner.jumbo, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".3 Number of jumbo protectionist measures affecting one trading partner.xlsx",sep=''))


#  Task 4 -----------------------------------------------------------------
# Request: Please send SE the number of jumbo protectionist measures affecting 10 billion USD of trade. 
# Add the value of trade affected to that list. If the number is not too large (SE will decide) then please produce a table of all of the jumbo protectionist
# measures listed in descending order of trade coverage today, indicating the implementing jurisdiction, the name of the measure, MAST chapter,
# date of implementation, whether the measure is still in force, and whether the measure affects only 1 trading partner 
# (if so, identify which trading partner.)
##
# Modified his request to 100 billion USD

trade.coverage.base.100b.threshold = trade.coverage.base[trade.coverage.base$trade.value > 100e9,]

trade.coverage.base.100b.threshold = merge(trade.coverage.base.100b.threshold, master[!duplicated(master[,c('intervention.id','mast.chapter','implementing.jurisdiction','title','date.implemented')]),
                                                                                                          c('intervention.id','mast.chapter','implementing.jurisdiction','title','date.implemented')], by ='intervention.id')

trade.coverage.base.100b.threshold = trade.coverage.base.100b.threshold[,c('intervention.id','implementing.jurisdiction','title','mast.chapter','date.implemented','currently.in.force','trade.value')]
trade.coverage.base.100b.threshold$affects.one.partner = 'FALSE'
trade.coverage.base.100b.threshold[trade.coverage.base.100b.threshold$intervention.id %in% unique.affected.partner.interventions,]$affects.one.partner = 'TRUE'
trade.coverage.base.100b.threshold = trade.coverage.base.100b.threshold[order(trade.coverage.base.100b.threshold$trade.value, decreasing=T),]

add.unique.affected.partner = master[master$intervention.id %in% unique.affected.partner.interventions,c('intervention.id','affected.jurisdiction')]
add.unique.affected.partner = add.unique.affected.partner[!duplicated(add.unique.affected.partner),]

trade.coverage.base.100b.threshold = merge(trade.coverage.base.100b.threshold, add.unique.affected.partner, by='intervention.id', all.x=T)
names(trade.coverage.base.100b.threshold) = c('Intervention ID','Implementing Jurisdiction','Title','Mast Chapter','Implemented Date','Currently in Force','Trade Value','Affects unique partner','Unique affected partner')                                                     
xlsx::write.xlsx(trade.coverage.base.100b.threshold, row.names=F, file = paste("0 report production/GTA 24/tables & figures/",output.path,"/",chapter.number,".4 Table of (100b threshold) jumbo protectionist measures.xlsx",sep=''))

