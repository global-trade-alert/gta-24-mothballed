library("xlsx")
library("gtalibrary")
rm(list = ls())


## setup
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
# setwd("/Users/patrickbuess/Dropbox/Collaborations/GTA cloud/0 report production/GTA 23")
# setwd('C:/Users/Kamran/Dropbox/GTA cloud')

source("0 report production/GTA 24/help files/GTA 24 cutoff and definitions.R")



path="0 report production/GTA 24/tables & figures/annex - p. 1 - title tables/"


chapters=c("D","E","F","G","I","L","M","P","TARIFF","X")

for(cty in g20.member.names){
  if(cty=="South Korea"){cty="Republic of Korea"}
    gta_trade_coverage(coverage.period = c(2009,2019),
                     gta.evaluation = c("red","amber"),
                     implementation.period = c("2008-11-01",cutoff),
                     exporters=cty,
                     keep.exporters = T,
                     implementer.role = c("importer","3rd country"),
                     mast.chapters = chapters,
                     keep.mast = T,
                     group.mast = F)
    
    trade.coverage.estimates=trade.coverage.estimates[,c(3,4,6:ncol(trade.coverage.estimates))]
    
    ## creating percentages
    for(i in 3:ncol(trade.coverage.estimates)){
      trade.coverage.estimates[,i]=round(trade.coverage.estimates[,i]*100,2)
    }
    names(trade.coverage.estimates)[1:2]=c("UN MAST chapter", "Foreign discriminatory policy instrument")
    names(trade.coverage.estimates)=gsub("Trade coverage estimate for ","",names(trade.coverage.estimates))
    
    
    xlsx::write.xlsx(trade.coverage.estimates, file=paste(path,cty,'.xlsx', sep=''), row.names = F)
    rm(trade.coverage.estimates)
  print(cty)
}


