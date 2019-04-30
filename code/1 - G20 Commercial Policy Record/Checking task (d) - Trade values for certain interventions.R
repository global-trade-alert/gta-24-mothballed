rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

#setwd("C:/Users/Johannes Fritz/Dropbox/GTA/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')


mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')

output.path = '1 - G20 Commercial Policy Record'

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
value.per.intervention=data.frame()
gta.evaluation=c("Red","Amber")

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_data_slicer(gta.evaluation = gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementer = T,
                  reporting.period=c(year.list[[year]]),
                  implementation.period = year.list[[year]],
                  keep.implementation.na = F
                  )
  
  check.int=unique(master.sliced$intervention.id)
  ms.base=master.sliced
  
  for(int in check.int){
    tryCatch(gta_trade_coverage(intervention.ids = int,
                                keep.interventions = T,
                                coverage.period = c(c.period, c.period),
                                trade.statistic = "value",
                                trade.data = r.year))
    
    if(nrow(trade.coverage.estimates)==0){
        
      tv=0
      
    } else{
      tv=trade.coverage.estimates[,4]
      
    }
    
    value.per.intervention=rbind(value.per.intervention,
                                 data.frame(period=paste(year.list[[year]], collapse=" - "),
                                            intervention.id=int,
                                            title=as.character(unique(subset(ms.base, intervention.id==int)$title)),
                                            trade.value=tv))
        
    trade.coverage.estimates=data.frame()
    
    print(int)
    
  }

  rm(check.int, master.sliced)
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(value.per.intervention, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""))
=======
rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

#setwd("C:/Users/Johannes Fritz/Dropbox/GTA/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')


mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')

output.path = '1 - G20 Commercial Policy Record'

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
value.per.intervention=data.frame()
gta.evaluation=c("Red","Amber")

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_data_slicer(gta.evaluation = gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementer = T,
                  reporting.period=c(year.list[[year]]),
                  implementation.period = year.list[[year]],
                  keep.implementation.na = F
                  )
  
  check.int=unique(master.sliced$intervention.id)
  ms.base=master.sliced
  
  for(int in check.int){
    tryCatch(gta_trade_coverage(intervention.ids = int,
                                keep.interventions = T,
                                coverage.period = c(c.period, c.period),
                                trade.statistic = "value",
                                trade.data = r.year))
    
    if(nrow(trade.coverage.estimates)==0){
        
      tv=0
      
    } else{
      tv=trade.coverage.estimates[,4]
      
    }
    
    value.per.intervention=rbind(value.per.intervention,
                                 data.frame(period=paste(year.list[[year]], collapse=" - "),
                                            intervention.id=int,
                                            title=as.character(unique(subset(ms.base, intervention.id==int)$title)),
                                            trade.value=tv))
        
    trade.coverage.estimates=data.frame()
    
    print(int)
    
  }

  rm(check.int, master.sliced)
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(value.per.intervention, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""))
>>>>>>> 1c7c6261717ba59f61b7793aa59eec505a4e39f6
=======
rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

#setwd("C:/Users/Johannes Fritz/Dropbox/GTA/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')


mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')

output.path = '1 - G20 Commercial Policy Record'

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
value.per.intervention=data.frame()
gta.evaluation=c("Red","Amber")

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_data_slicer(gta.evaluation = gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementer = T,
                  reporting.period=c(year.list[[year]]),
                  implementation.period = year.list[[year]],
                  keep.implementation.na = F
                  )
  
  check.int=unique(master.sliced$intervention.id)
  ms.base=master.sliced
  
  for(int in check.int){
    tryCatch(gta_trade_coverage(intervention.ids = int,
                                keep.interventions = T,
                                coverage.period = c(c.period, c.period),
                                trade.statistic = "value",
                                trade.data = r.year))
    
    if(nrow(trade.coverage.estimates)==0){
        
      tv=0
      
    } else{
      tv=trade.coverage.estimates[,4]
      
    }
    
    value.per.intervention=rbind(value.per.intervention,
                                 data.frame(period=paste(year.list[[year]], collapse=" - "),
                                            intervention.id=int,
                                            title=as.character(unique(subset(ms.base, intervention.id==int)$title)),
                                            trade.value=tv))
        
    trade.coverage.estimates=data.frame()
    
    print(int)
    
  }

  rm(check.int, master.sliced)
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(value.per.intervention, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""))
=======
rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

#setwd("C:/Users/Johannes Fritz/Dropbox/GTA/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
#setwd('C:/Users/Kamran/Dropbox/GTA cloud')
setwd('D:/Dropbox/Dropbox/GTA cloud')


mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')

output.path = '1 - G20 Commercial Policy Record'

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
value.per.intervention=data.frame()
gta.evaluation=c("Red","Amber")

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_data_slicer(gta.evaluation = gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementer = T,
                  reporting.period=c(year.list[[year]]),
                  implementation.period = year.list[[year]],
                  keep.implementation.na = F
                  )
  
  check.int=unique(master.sliced$intervention.id)
  ms.base=master.sliced
  
  for(int in check.int){
    tryCatch(gta_trade_coverage(intervention.ids = int,
                                keep.interventions = T,
                                coverage.period = c(c.period, c.period),
                                trade.statistic = "value",
                                trade.data = r.year))
    
    if(nrow(trade.coverage.estimates)==0){
        
      tv=0
      
    } else{
      tv=trade.coverage.estimates[,4]
      
    }
    
    value.per.intervention=rbind(value.per.intervention,
                                 data.frame(period=paste(year.list[[year]], collapse=" - "),
                                            intervention.id=int,
                                            title=as.character(unique(subset(ms.base, intervention.id==int)$title)),
                                            trade.value=tv))
        
    trade.coverage.estimates=data.frame()
    
    print(int)
    
  }

  rm(check.int, master.sliced)
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(value.per.intervention, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Trade value per intervention.xlsx", sep=""))

