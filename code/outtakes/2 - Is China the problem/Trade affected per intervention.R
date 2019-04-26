library(gtalibrary)

rm(list=ls())



setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
# setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

output.path = '2 - Is China the problem'
xlsx.path = paste0('0 report production/GTA 24/tables & figures/',output.path)


load('data/master_plus.Rdata')
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

coverage.by.intervention=unique(subset(master, gta.evaluation %in% c("Amber","Red") & is.na(date.implemented)==F & date.implemented<=cutoff)[,c("intervention.id","date.implemented","currently.in.force")])
coverage.by.intervention$year.implemented=year(coverage.by.intervention$date.implemented)
coverage.by.intervention$date.implemented=NULL


coverage.by.intervention$value.usd=NA
coverage.by.intervention$found.trade=T



## Route 1: Via gta_trade_coverage

for(i in 1:nrow(coverage.by.intervention)){
  
  tryCatch({
    gta_trade_coverage(intra.year.duration = F,
                       coverage.period = c(coverage.by.intervention$year.implemented[i],
                                           coverage.by.intervention$year.implemented[i]),
                       intervention.ids = coverage.by.intervention$intervention.id[i],
                       keep.interventions = T,
                       trade.statistic="value",
                       trade.data = "base"
    )
    
    
    coverage.by.intervention$value.usd[i]=trade.coverage.estimates[1,ncol(trade.coverage.estimates)]/3
    
    
  },
  error = function(e) {
    coverage.by.intervention$value.usd[i]=0
    coverage.by.intervention<<-coverage.by.intervention
    coverage.by.intervention$found.trade[i]=F
    coverage.by.intervention<<-coverage.by.intervention
  })
  
  print(i)
  
  
}



## Route 2 - slow lane: Via gta_imp_exp_hs_tuples
load("data/support tables/Final goods support table.Rdata")
trade=subset(final, Year %in% c(2005:2007))[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")

trade=aggregate(trade.value ~ i.un + a.un + affected.product, trade, sum)
trade$trade.value= trade$trade.value/3 ## using 'mean' in the aggregate above would exclude observations with zero observed trade.
  

for(i in 1:nrow(coverage.by.intervention)){
  
  tryCatch({
    
    master.temp=subset(master, intervention.id==coverage.by.intervention$intervention.id[i])
    
    gta_imp_exp_hs_tuples(master.path="master.temp",
                          master.data.frame = T)
    
    
    master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))
    
    if(nrow(master.tuple)>0){
      coverage.by.intervention$value.usd[i]=sum(master.tuple$trade.value)
    } else {
      coverage.by.intervention$value.usd[i]=0
      coverage.by.intervention$found.trade[i]=F
    }
    
    
    
  },
  error = function(e) {
    coverage.by.intervention$value.usd[i]=0
    coverage.by.intervention<<-coverage.by.intervention
    coverage.by.intervention$found.trade[i]=F
    coverage.by.intervention<<-coverage.by.intervention
  })
  
  print(i)
  
  
}



## Route 2 -  fast lane: Via gta_imp_exp_hs_tuples & all in one
load("data/support tables/Final goods support table.Rdata")
trade=subset(final, Year %in% c(2005:2007))[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)
names(trade)=c("i.un","a.un","year","affected.product","trade.value")

trade=aggregate(trade.value ~ i.un + a.un + affected.product, trade, sum)
trade$trade.value= trade$trade.value/3 ## using 'mean' in the aggregate above would exclude observations with zero observed trade.


master.temp=subset(master, intervention.id %in% coverage.by.intervention$intervention.id)

gta_imp_exp_hs_tuples(master.path="master.temp",
                      master.data.frame = T)


master.tuple=merge(master.tuple, trade, by=c("i.un","a.un","affected.product"))

coverage.by.intervention$value.usd=NULL

coverage.by.intervention=merge(coverage.by.intervention, aggregate(trade.value ~ intervention.id, master.tuple, sum),by="intervention.id", all.x=T)
coverage.by.intervention$found.trade[is.na(coverage.by.intervention$trade.value)]=F
coverage.by.intervention$trade.value[is.na(coverage.by.intervention$trade.value)]=0



