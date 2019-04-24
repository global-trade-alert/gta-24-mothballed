# rm(list=ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)
library(openxlsx)

#setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

chapter.number=2
output.path = paste(chapter.number, " - Is China the problem",sep="")
xlsx.path = paste0('0 report production/GTA 24/tables & figures/',output.path)


# gtalibrary::country.names
country.descriptions = read_csv2('R help files/country_iso_un.csv')

# gtalibrary::int.mast.types
mast = readxl::read_xlsx('R help files/GTA-MAST.xlsx')

# gtalibrary::cpc.names
cpc.codes = read_csv2('R help files/cpc names.csv')

# gtalibrary::country.correspondence
load("0 gtalibrary/data/country.correspondence.rda")

# gtalibrary::country.groups
load("0 gtalibrary/data/country.groups.rda")

# gtalibrary::country.names
load("0 gtalibrary/data/country.names.rda")

# why do you need the master file? Should be loaded/manipulated via gta_data_slicer() where possible
load('data/master_plus.Rdata')

# 1 -----------------------------------------------------------------------
# First, please compute the current share of world exports that face 
# discriminatory policies still in effect.

gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                   implementer.role = c('importer','3rd country'),
                   coverage.period = c(2019,2019),
                   in.force.today = T
)

share.exports.discrim.policies = trade.coverage.estimates$`Trade coverage estimate for 2019`[1]

table.path = paste0(xlsx.path,'/Table ',chapter.number,'.1 - World export share affected by discriminatory interventions currently in force.xlsx')

wb <- createWorkbook()
sheetname = paste(chapter.number,'.1',sep="") #please remove number and give a short name, I leave it in here so you can see my earlier comment about the chapter number flexibility.
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=c('World export share affected by discriminatory interventions currently in force',share.exports.discrim.policies))
saveWorkbook(wb,table.path,overwrite = T)

# 2 -----------------------------------------------------------------------
# Then calculate the share of world exports that face discriminatory policies if, in turn, 
# each G20 member eliminated all of their crisis-era red and amber measures still in effect. 
# In each case, please identify the absolute value of exports that would no longer face discrimination.

g20.countries = country.descriptions[country.descriptions$G20 ==1, colnames(country.descriptions) == 'gta.name']
g20.countries = g20.countries[!is.na(g20.countries), ]
g20.countries = g20.countries$gta.name
g20.countries = union(g20.countries, 'G20')

#export shares without g20
export.shares.without.g20 = data.frame()

for (country in g20.countries){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     implementers = country,
                     keep.implementer = F,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`Implementing country` = country
    export.shares.without.g20 = rbind(export.shares.without.g20,trade.coverage.estimates)
  }
  
}

if(length(setdiff(g20.countries,export.shares.without.g20$`Implementing country`))>0){
  stop(paste("You are missing:",setdiff(g20.countries,export.shares.without.g20$`Implementing country`)))
}


#absolute values that would no longer face discrimination

#absolute values all interventions
gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                   implementer.role = c('importer','3rd country'),
                   coverage.period = c(2019,2019),
                   in.force.today = T, 
                   trade.statistic = 'value'
)

absolute.exports.discrim.policies = trade.coverage.estimates$`Trade coverage estimate for 2019`

export.absolutes.without.g20 = data.frame()
#absolute values excluding specific interventions
for (country in g20.countries){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     implementers = country,
                     keep.implementer = F,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T, 
                     trade.statistic = 'value'
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`Implementing country` = country
    
    export.absolutes.without.g20 = rbind(export.absolutes.without.g20,trade.coverage.estimates)
  }
}

if(length(setdiff(g20.countries,export.absolutes.without.g20$`Implementing country`))>0){
  stop(paste("You are missing:",setdiff(g20.countries,export.absolutes.without.g20$`Implementing country`)))
}


export.absolutes.without.g20$`Absolute value no longer discriminated` = absolute.exports.discrim.policies - export.absolutes.without.g20$`Trade coverage estimate for 2019`
export.absolutes.without.g20$`Absolute value no longer discriminated` = export.absolutes.without.g20$`Absolute value no longer discriminated`/3

path = paste0('0 report production/GTA 24/data/',output.path,'/Task 2.Rdata') # please rename file to match chapter and/or figure number.
save(export.shares.without.g20, absolute.exports.discrim.policies, export.absolutes.without.g20, file = path)
load(path)

export.shares.without.g20 = export.shares.without.g20[,c(5,4)] # please use the names of these columns instead. Their order may change.
colnames(export.shares.without.g20) = c('Omitted implementing country', 'World export share remaining affected')
options(scipen=999)
export.absolutes.without.g20 = export.absolutes.without.g20[,5:6] # please use the names of these columns instead. Their order may change.
colnames(export.absolutes.without.g20) = c('Omitted implementing country', 'Absolute value no longer discriminated against')

path.4.2 = paste0(xlsx.path,'/Table 4.2 - World export shares with individual omission of G20 implementers.xlsx')

wb <- createWorkbook()
sheetname = 'Shares remaining'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.shares.without.g20)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'Value no longer discriminated'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.absolutes.without.g20)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
saveWorkbook(wb,path.4.2,overwrite = T)


# 3 -----------------------------------------------------------------------
# Then calculate the share of world exports that face discriminatory policies if, in turn, 
# each MAST category of crisis-era red and amber measures still in effect was eliminated. 
# In each case, please identify the absolute value of exports that would no longer face discrimination.

mast.chapters = unique(mast$`MAST chapter ID`)

#export shares without mast
export.shares.without.mast = data.frame()

for (chapter in mast.chapters){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     mast.chapters = chapter,
                     keep.mast = F, 
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`Chapter` = chapter
    
    export.shares.without.mast = rbind(export.shares.without.mast,trade.coverage.estimates)
  }
}

## add completeness check here.

export.absolutes.without.mast = data.frame()
#absolute values excluding specific mast.chapters
for (chapter in mast.chapters){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     mast.chapters = chapter,
                     keep.mast = F,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T, 
                     trade.statistic = 'value'
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`Chapter` = chapter
    
    export.absolutes.without.mast = rbind(export.absolutes.without.mast,trade.coverage.estimates)
  }
}

## add completeness check here.

export.absolutes.without.mast$`Absolute value no longer discriminated` = absolute.exports.discrim.policies - export.absolutes.without.mast$`Trade coverage estimate for 2019`
export.absolutes.without.mast$`Absolute value no longer discriminated` = export.absolutes.without.mast$`Absolute value no longer discriminated`/3

path = paste0('0 report production/GTA 24/data/',output.path,'/Task 3.Rdata') # please rename file to match chapter and/or figure number.
save(export.shares.without.mast, absolute.exports.discrim.policies, export.absolutes.without.mast, file = path)

load(path)

export.shares.without.mast = export.shares.without.mast[,c(5,4)] # please use the names of these columns instead. Their order may change.
export.shares.without.mast = export.shares.without.mast[order(export.shares.without.mast$Chapter),]
export.shares.without.mast$Chapter.description = plyr::mapvalues(export.shares.without.mast$Chapter, unique(mast$`MAST chapter ID`),
                                                                 unique(mast$`MAST chapter name`))
colnames(export.shares.without.mast) = c('Omitted MAST chapter', 'World export share remaining affected', 'Chapter description')
export.absolutes.without.mast = export.absolutes.without.mast[,5:6] # please use the names of these columns instead. Their order may change.
export.absolutes.without.mast = export.absolutes.without.mast[order(export.absolutes.without.mast$Chapter),]
export.absolutes.without.mast$Chapter.description = plyr::mapvalues(export.absolutes.without.mast$Chapter, unique(mast$`MAST chapter ID`),
                                                     unique(mast$`MAST chapter name`))
colnames(export.absolutes.without.mast) = c('Omitted MAST chapter', 'Absolute value no longer discriminated against', 'Chapter description')

path.4.3 = paste0(xlsx.path,'/Table 4.3 - World export shares with individual omission of MAST chapters.xlsx')

wb <- createWorkbook()
sheetname = 'Shares remaining'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.shares.without.mast)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'No longer discriminated'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.absolutes.without.mast)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
saveWorkbook(wb,path.4.3,overwrite = T)

# 4 -----------------------------------------------------------------------
# Third, repeat the above steps for the 10 2-digit CPC sectors that represent the 
# largest shares of world trade i.e. total share of sectoral exports covered by current barrier, 
# remove G20 individually, remove MAST individually.


cpc.codes.2digit = cpc.codes[cpc.codes$cpc.digit.level==2, ]
cpc.codes.2digit = cpc.codes.2digit$cpc
mast.chapters = unique(mast$`MAST chapter ID`)

## Identify top 10 2-digit CPC sectors that represent the largest shares of world trade
load("data/support tables/Final goods support table.Rdata")
trade=subset(final, Year==2017)[,c("Reporter.un","Partner.un","Year","Tariff.line","Value")]
rm(final)

cpc.hs=gtalibrary::cpc.to.hs
cpc.hs$cpc.2=substr(cpc.hs$cpc, 1,2)
cpc.hs$cpc.2[nchar(cpc.hs$cpc)==2]=substr(cpc.hs$cpc[nchar(cpc.hs$cpc)==2], 1,1)
data.table::setnames(cpc.hs, "hs","Tariff.line")
trade=merge(trade, cpc.hs[,c("cpc.2", "Tariff.line")], by="Tariff.line", all.x=T)

trade.by.sector=aggregate(Value ~ cpc.2, trade, sum)
trade.by.sector=trade.by.sector[order(-trade.by.sector$Value),]

if(length(setdiff(unique(cpc.hs$cpc.2), unique(trade.by.sector$cpc.2)))>0){
  stop(paste("You missed sectors:",setdiff(unique(cpc.hs$cpc.2), unique(trade.by.sector$cpc.2))))
}

cpc.codes.2digit=trade.by.sector$cpc.2[1:10,]

export.shares.with.2digitcpc = data.frame()
#determine which 2digit cpc have the largest shares 
for (code in cpc.codes.2digit){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'), 
                     cpc.sectors = gta_cpc_code_expand(as.numeric(code)),
                     keep.cpc = T,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T
    )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`2 digit cpc` = code
    
    export.shares.with.2digitcpc = rbind(export.shares.with.2digitcpc,trade.coverage.estimates)
  }
  
  ### Please work through all three requested statistics in the same loop:
  ## (i) total share of sectoral exports covered by current barriers, 
  ## (ii) remove G20 individually, 
  ## (iii) remove MAST individually.
  
}

## add completeness check here.

path = paste0('0 report production/GTA 24/data/',output.path,'/Task 4.Rdata') # please rename file to match chapter and/or figure number.
save(export.shares.with.2digitcpc, file = path)
load(path)

export.shares.with.2digitcpc = export.shares.with.2digitcpc[order(export.shares.with.2digitcpc$`Trade coverage estimate for 2019`, decreasing=T),]
export.shares.with.2digitcpc = export.shares.with.2digitcpc[1:10,]

top.10.2digitcpc = export.shares.with.2digitcpc$`2 digit cpc`

# repeat previous steps 
export.shares.without.top.10.2digitcpc = data.frame()

for (code in top.10.2digitcpc){
    gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                       cpc.sectors = gta_cpc_code_expand(as.numeric(code)),
                       keep.cpc = F,
                       implementer.role = c('importer','3rd country'),
                       coverage.period = c(2019,2019),
                       in.force.today = T
    )
    
    if (error.message == FALSE){
      trade.coverage.estimates$`Code removed` = code
      
      export.shares.without.top.10.2digitcpc = rbind(export.shares.without.top.10.2digitcpc,trade.coverage.estimates)
    
  }
}

## add completeness check here.

save(top.10.2digitcpc,absolute.exports.discrim.policies,export.shares.with.2digitcpc,export.shares.without.top.10.2digitcpc, file = path)
load(path)


# absolute values without certain sectors
export.absolutes.without.top.10.2digitcpc = data.frame()

for (code in top.10.2digitcpc){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     cpc.sectors = gta_cpc_code_expand(as.numeric(code)),
                     keep.cpc = F,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T,
                     trade.statistic = 'value'
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`Code removed` = code
    
    export.absolutes.without.top.10.2digitcpc = rbind(export.absolutes.without.top.10.2digitcpc,trade.coverage.estimates)
    
  }
}

## add completeness check here.

export.absolutes.without.top.10.2digitcpc$`Absolute value no longer discriminated` = absolute.exports.discrim.policies - export.absolutes.without.top.10.2digitcpc$`Trade coverage estimate for 2019`
export.absolutes.without.top.10.2digitcpc$`Absolute value no longer discriminated` = export.absolutes.without.top.10.2digitcpc$`Absolute value no longer discriminated`/3
save(top.10.2digitcpc,absolute.exports.discrim.policies, export.shares.with.2digitcpc,export.shares.without.top.10.2digitcpc,export.absolutes.without.top.10.2digitcpc, file = path)
load(path)

# g20
g20.countries = country.descriptions[country.descriptions$G20 ==1, colnames(country.descriptions) == 'gta.name']
g20.countries = g20.countries[!is.na(g20.countries), ]
g20.countries = g20.countries$gta.name
g20.countries = union(g20.countries, 'G20')

# remove g20 / mast individually
export.shares.top.10.2digitcpc = data.frame()

for (country in g20.countries){
  for (code in top.10.2digitcpc){
  gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                     implementers = country,
                     keep.implementer = F,
                     cpc.sectors = gta_cpc_code_expand(as.numeric(code)),
                     keep.cpc = T,
                     implementer.role = c('importer','3rd country'),
                     coverage.period = c(2019,2019),
                     in.force.today = T
  )
  
  if (error.message == FALSE){
    trade.coverage.estimates$`2 digit cpc code` = code
    trade.coverage.estimates$`Element removed` = country
    trade.coverage.estimates$`Element type` = 'G20 country'
    
    export.shares.top.10.2digitcpc = rbind(export.shares.top.10.2digitcpc,trade.coverage.estimates)
    }
  }
}

## add completeness check here.

save(top.10.2digitcpc,absolute.exports.discrim.policies, export.shares.with.2digitcpc,export.shares.without.top.10.2digitcpc,export.absolutes.without.top.10.2digitcpc,
     export.shares.top.10.2digitcpc,file = path)

for (code in top.10.2digitcpc){
  for (chapter in mast.chapters){
    gta_trade_coverage(gta.evaluation = c('Red','Amber'),
                       mast.chapters = chapter,
                       keep.mast = F,
                       cpc.sectors = gta_cpc_code_expand(as.numeric(code)),
                       keep.cpc = T,
                       implementer.role = c('importer','3rd country'),
                       coverage.period = c(2019,2019),
                       in.force.today = T
    )
    
    if (error.message == FALSE){
      trade.coverage.estimates$`2 digit cpc code` = code
      trade.coverage.estimates$`Element removed` = chapter
      trade.coverage.estimates$`Element type` = 'MAST chapter'
      
      export.shares.top.10.2digitcpc = rbind(export.shares.top.10.2digitcpc,trade.coverage.estimates)
    }
  }
}

## add completeness check here.

path = paste0('0 report production/GTA 24/data/',output.path,'/Task 4.Rdata') # please rename file to match chapter and/or figure number.
save(absolute.exports.discrim.policies, export.shares.with.2digitcpc,export.shares.without.top.10.2digitcpc,export.absolutes.without.top.10.2digitcpc,
     export.shares.top.10.2digitcpc,file = path)
load(path)

export.shares.without.top.10.2digitcpc = export.shares.without.top.10.2digitcpc[,c(5:4)] # please use the names of these columns instead. Their order may change.
export.shares.without.top.10.2digitcpc$new.col = plyr::mapvalues(export.shares.without.top.10.2digitcpc$`Code removed`,
                                                                 cpc.codes$cpc,
                                                                 cpc.codes$cpc.name)
export.shares.without.top.10.2digitcpc = export.shares.without.top.10.2digitcpc[order(as.numeric(export.shares.without.top.10.2digitcpc$`Code removed`)),]  
colnames(export.shares.without.top.10.2digitcpc) = c('Omitted 2 digit Cpc code', 'World export share remaining affected', 'Cpc code description') 

export.absolutes.without.top.10.2digitcpc = export.absolutes.without.top.10.2digitcpc[,5:6] # please use the names of these columns instead. Their order may change.
export.absolutes.without.top.10.2digitcpc$new.col = plyr::mapvalues(export.absolutes.without.top.10.2digitcpc$`Code removed`,
                                                                 cpc.codes$cpc,
                                                                 cpc.codes$cpc.name)
colnames(export.absolutes.without.top.10.2digitcpc) = c('Omitted 2 digit Cpc code', 'Absolute value no longer discriminated against', 'Cpc code description') 

mast.export.shares.top.10.2digitcpc = export.shares.top.10.2digitcpc[export.shares.top.10.2digitcpc$`Element type` == 'MAST chapter',]
g20.export.shares.top.10.2digitcpc = export.shares.top.10.2digitcpc[export.shares.top.10.2digitcpc$`Element type` == 'G20 country',]
mast.export.shares.top.10.2digitcpc = mast.export.shares.top.10.2digitcpc[ ,c(5:6,4)] # please use the names of these columns instead. Their order may change.
g20.export.shares.top.10.2digitcpc = g20.export.shares.top.10.2digitcpc[,c(5:6,4)] # please use the names of these columns instead. Their order may change.

g20.export.shares.top.10.2digitcpc = g20.export.shares.top.10.2digitcpc[order(as.numeric(g20.export.shares.top.10.2digitcpc$`2 digit cpc code`)),]
mast.export.shares.top.10.2digitcpc = mast.export.shares.top.10.2digitcpc[order(as.numeric(mast.export.shares.top.10.2digitcpc$`2 digit cpc code`)),]

g20.export.shares.top.10.2digitcpc$Cpc.description = plyr::mapvalues(g20.export.shares.top.10.2digitcpc$`2 digit cpc code`, cpc.codes$cpc,
                                                                     cpc.codes$cpc.name)
mast.export.shares.top.10.2digitcpc$Cpc.description = plyr::mapvalues(mast.export.shares.top.10.2digitcpc$`2 digit cpc code`, cpc.codes$cpc,
                                                                     cpc.codes$cpc.name)

mast.export.shares.top.10.2digitcpc$Chapter.description = plyr::mapvalues(mast.export.shares.top.10.2digitcpc$`Element removed`, unique(mast$`MAST chapter ID`),
                                                                    unique(mast$`MAST chapter name`))

colnames(mast.export.shares.top.10.2digitcpc) = c('2 digit cpc code', 'Chapter removed', 'Export Share remaining', 'Cpc description', 'MAST chapter description')
colnames(g20.export.shares.top.10.2digitcpc) = c('2 digit cpc code', 'Country removed', 'Export Share remaining', 'Cpc description')

path.4.4 = paste0(xlsx.path,'/Table 4.4 - World export shares with individual omission of top 10 2-digit cpc.xlsx')

wb <- createWorkbook()
sheetname = 'Shares remaining'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.shares.without.top.10.2digitcpc)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'Value no longer discriminated'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=export.absolutes.without.top.10.2digitcpc)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'Removal MAST chapters'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=mast.export.shares.top.10.2digitcpc)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'Removal G20 members'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=g20.export.shares.top.10.2digitcpc)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
saveWorkbook(wb,path.4.4,overwrite = T)

  

# 5 -----------------------------------------------------------------------
# Fourth, for thresholds of $10 billion, $100 billion, $250 billion, and $500 billion 
# calculate the total number of red or amber policy measures 
# (i) ever implemented that cover more than the threshold and the total number of such measures 
# (ii) still in force today.


load('data/master_plus.Rdata')
source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")

coverage.by.intervention=unique(subset(master, gta.evaluation %in% c("Amber","Red") & is.na(date.implemented)==F & date.implemented<=cutoff)[,c("intervention.id","date.implemented","currently.in.force")])
coverage.by.intervention$year.implemented=year(coverage.by.intervention$date.implemented)
coverage.by.intervention$date.implemented=NULL


coverage.by.intervention$value.usd=NA
coverage.by.intervention$found.trade=T


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

filter.implementer.coverage.by.intervention = function(un.code.list, country.group.name){
  
  master.tuple = master.tuple[master.tuple$i.un %in% un.code.list, ]
  coverage.by.intervention=merge(coverage.by.intervention, aggregate(trade.value ~ intervention.id, master.tuple, sum),by="intervention.id", all.x=T)
  coverage.by.intervention$found.trade[is.na(coverage.by.intervention$trade.value)]=F
  coverage.by.intervention$trade.value[is.na(coverage.by.intervention$trade.value)]=0
  
  all.coverage.thresholds = coverage.by.intervention
  in.force.coverage.thresholds = coverage.by.intervention[coverage.by.intervention$currently.in.force == 'Yes',] 
  
  df.all = data.frame(thresholds = c('10b','100b','250b','500b'),
                      in.force = rep('Interventions ever implemented', 4),
                      'Number of interventions remaining' = c(length(which(all.coverage.thresholds$trade.value > 10e09)),
                                                              length(which(all.coverage.thresholds$trade.value > 100e09)),
                                                              length(which(all.coverage.thresholds$trade.value > 250e09)),
                                                              length(which(all.coverage.thresholds$trade.value > 500e09))))
  
  df.in.force = data.frame(thresholds = c('10b','100b','250b','500b'),
                           in.force = rep('Interventions still in force', 4),
                           'Number of interventions remaining' = c(length(which(in.force.coverage.thresholds$trade.value > 10e09)),
                                                                   length(which(in.force.coverage.thresholds$trade.value > 100e09)),
                                                                   length(which(in.force.coverage.thresholds$trade.value > 250e09)),
                                                                   length(which(in.force.coverage.thresholds$trade.value > 500e09))))
  
  df = rbind(df.all,df.in.force)
  df$Implementing.country = country.group.name
  
  colnames(df) = c('Threshold', 'Enforcement status', 'Number of interventions passing threshold', 'Implementing country group')
  df=df[,c(4,1:3)] # please use the names of these columns instead. Their order may change.
  
    
  return(df)
  
}

all.interventions.un.code.list = unique(master.tuple$i.un)
all.interventions.thresholds = filter.implementer.coverage.by.intervention(all.interventions.un.code.list, 'All countries')

path.4.5 = paste0(xlsx.path,'/Table 4.5 - Interventions passing trade coverage thresholds - all implementers.xlsx')

wb <- createWorkbook()
sheetname = 'All implemented interventions'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=all.interventions.thresholds[1:4,])
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'In force interventions'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=all.interventions.thresholds[5:8,])
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
saveWorkbook(wb,path.4.5,overwrite = T)

# 6 -----------------------------------------------------------------------
# Disaggregate counts of
# (i) the total ever and 
# (ii) the total still in force 
# by implementing jurisdictions as follows: 
# United States, China, EU28, Japan, rest of G20, and rest of world (non-G20).


US.code = country.names[country.names$name=='United States of America',]$un_code
china.code = country.names[country.names$name=='China',]$un_code
japan.code = country.names[country.names$name=='Japan',]$un_code

eu28.codes = country.correspondence[country.correspondence$name == 'EU-28',]$un_code

g20.members.codes =c("32", "36", "76", "124", "156", "251", "276", "699", "360", "381",
              "392", "484", "410", "643", "682", "710", "792", "826", "840")

rest.g20.members.codes = setdiff(g20.members.codes,c(US.code,china.code,eu28.codes))
rest.g20.members.names = data.frame('Rest of G20 includes: ' = plyr::mapvalues(rest.g20.members.codes, as.character(country.names$un_code),
                                         as.character(country.names$name)))
colnames(rest.g20.members.names) = 'Rest of G20 includes: '
non.g20.members.codes = setdiff(all.interventions.un.code.list,g20.members.codes)

US.interventions.thresholds = filter.implementer.coverage.by.intervention(US.code, 'United States of America')
china.interventions.thresholds = filter.implementer.coverage.by.intervention(china.code, 'China')
japan.interventions.thresholds = filter.implementer.coverage.by.intervention(japan.code, 'Japan')
eu28.interventions.thresholds = filter.implementer.coverage.by.intervention(eu28.codes, 'EU28')
rest.g20.interventions.thresholds = filter.implementer.coverage.by.intervention(rest.g20.members.codes, 'Rest of G20')
non.g20.interventions.thresholds = filter.implementer.coverage.by.intervention(non.g20.members.codes, 'non G20')

path.4.6 = paste0(xlsx.path,'/Table 4.6 - Interventions passing trade coverage thresholds - specific implementers.xlsx')

wb <- createWorkbook()
sheetname = 'USA thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=US.interventions.thresholds)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'China thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=china.interventions.thresholds)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'Japan thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=japan.interventions.thresholds)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'EU28 thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=eu28.interventions.thresholds)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'rest G20 thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=rest.g20.interventions.thresholds)
writeData(wb, sheet=sheetname, x=rest.g20.members.names, startCol = 7)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
sheetname = 'non-G20 ROW thresholds'
addWorksheet(wb, sheetname)
writeData(wb, sheet=sheetname, x=non.g20.interventions.thresholds)
setColWidths(wb, sheet = sheetname, cols = 1:10, widths = "auto")
saveWorkbook(wb,path.4.6,overwrite = T)

