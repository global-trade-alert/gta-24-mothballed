rm(list=ls())
library(tidyverse)
library(lubridate)
library(data.table)
library(gtalibrary)
library(ggplot2)

# setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")
#setwd("C:/Users/Piotr Lukaszuk/Dropbox/GTA cloud")
#setwd("/Users/piotrlukaszuk/Dropbox/GTA cloud")
setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

mast.descriptions = gtalibrary::int.mast.types

chapter.number = 1
chapter.title = 'G20 Commercial Policy Record'
output.path = paste(chapter.number,chapter.title,sep = ' - ')

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))

periods = c('01/12/14-15/04/15','01/12/15-15/04/16', '01/12/16-15/04/17', 
                  '01/12/17-15/04/18', '01/12/18-15/04/19')

period.labels = c('01/12/14-\n15/04/15','01/12/15-\n15/04/16', '01/12/16-\n15/04/17', 
                  '01/12/17-\n15/04/18', '01/12/18-\n15/04/19')


# 1 -----------------------------------------------------------------------
# a -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the (a) total number of G20 amber and red implemented measures

total.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                    implementing.country = 'G20',
                    keep.implementation.na = F,
                    implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])

  total.implemented.harmful.measures[year] = length(unique(master.sliced$intervention.id))

}

plotting.data = data.frame(periods = 1:5, periods = periods, total.implemented.harmful.measures)

table.fig.1 = plotting.data[,c('periods','total.implemented.harmful.measures')]
names(table.fig.1) = c('Period', 'Total Implemented Harmful measures')
xlsx::write.xlsx(table.fig.1, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data.xlsx", sep=""))

# b -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (b) the share of amber and red implemented measures by the G20 in all measures implemented by the G20

share.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  temp = master.sliced
  
  gta_data_slicer(gta.evaluation= c('Red', 'Amber', 'Green'),
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  share.implemented.harmful.measures[year] = length(unique(temp$intervention.id))/length(unique(master.sliced$intervention.id))

}

plotting.data$share.implemented.harmful.measures = share.implemented.harmful.measures

table.fig.2 = plotting.data[,c('periods','share.implemented.harmful.measures')]
names(table.fig.2) = c('Period', 'Share Implemented Harmful measures')
xlsx::write.xlsx(table.fig.2, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ", chapter.number,".2 - Data.xlsx", sep=""))


# c -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (c) as the top five harmful policy instruments used by the G20 (and the other)

g20.members=c(32, 36, 76, 124, 156, 251, 276, 699, 360, 381, 392, 484, 410, 643, 682, 710, 792, 826, 840)
oecd.members=c(36, 40, 56, 124, 152, 203, 208, 233, 246, 251, 276, 300, 348, 352, 372, 376, 381, 392, 410, 428, 440, 442, 484, 528, 554, 578, 616, 620, 703, 705, 724, 752, 756, 792, 826, 840)
country.groups=list('G20' = g20.members, 'OECD' = oecd.members)
# 
# harmful.instruments.per.year = data.frame()
# for (year in 1:length(year.list)){
#   for (c.group in 1:length(country.groups)){
#     if (names(country.groups[c.group]) == 'G20'){keep.implementer.boolean = T}
#     if (names(country.groups[c.group]) == 'OECD'){keep.implementer.boolean = F}
#     
#     gta_data_slicer(gta.evaluation= gta.evaluation,
#                     implementing.country = country.groups[[c.group]],
#                     keep.implementer = keep.implementer.boolean,
#                     keep.implementation.na = T,
#                     implementation.period = c(year.list[[year]]))
#     
#     master.sliced=subset(master.sliced, date.published<=year.list[[year]][2], select = c('mast.chapter','intervention.id'))
#     master.sliced=subset(master.sliced, !duplicated(master.sliced))
#     master.sliced=dplyr::count(master.sliced,mast.chapter)
#     
#     master.sliced$period=year
#     master.sliced$country.group=names(country.groups[c.group])
#     
#     harmful.instruments.per.year = rbind(harmful.instruments.per.year,master.sliced)
#   }
# }

# c -----------------------------------------------------------------------


g20.implemented.harmful.measures.policies = data.frame()

#get most frequent policy instruments over the 5 years
for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
  g20.policies = g20.policies[!duplicated(g20.policies),]
  g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) 
  g20.policies.chapters$period = year
  
  g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)

}

top5.frequent.policies = g20.implemented.harmful.measures.policies %>% group_by(mast.chapter) %>% summarise(most.frequent = sum(n)) %>% dplyr::top_n(5)
top5.frequent.policies = top5.frequent.policies$mast.chapter

g20.implemented.harmful.measures.policies = data.frame()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
  g20.policies = g20.policies[!duplicated(g20.policies),]
  g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) %>% filter(mast.chapter %in% top5.frequent.policies)
  g20.policies.others = g20.policies[!(g20.policies$mast.chapter %in% g20.policies.chapters$mast.chapter),]
  g20.policies.others = g20.policies.others[!duplicated(g20.policies.others),]
  g20.policies.chapters = rbind(g20.policies.chapters, data.frame(mast.chapter = 'Others', n = nrow(g20.policies.others)))
  g20.policies.chapters$period = year
  
  g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)
  
}



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

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

# 2 - Plotting ----------------------------------------------------------------

# a -----------------------------------------------------------------------
# Simon's request: A line chart for (a) and (b) should be prepared.

gta_colour_palette()

plot.6.2.a = ggplot(plotting.data,aes(x = periods, y = total.implemented.harmful.measures)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour= gta_colour$harmful[1], size=3) +
  ylab('Number of G20 implemented harmful interventions') +
  xlab('Period') + ylim(c(0, 300)) + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + gta_theme()

plot.6.2.a

# b -----------------------------------------------------------------------
# Simon's request: A line chart for (a) and (b) should be prepared.

plot.6.2.b = ggplot(plotting.data,aes(x=periods, y=share.implemented.harmful.measures*100)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour=gta_colour$harmful[1], size=3) +
  xlab('Period') +  ylab('Percentage of G20 implemented measures which are harmful') + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + 
  scale_y_continuous(breaks = seq(0,100,10),labels=paste0(seq(0,100,10),'%'), limits = (c(60,100))) + gta_theme()

plot.6.2.b

# c -----------------------------------------------------------------------
# Simon's request: A stacked bar chart should be prepared for (c).

MAST.chapter.descriptions = unique(mast.descriptions$`MAST chapter name`)

# MAST.chapter.descriptions = c(paste(unique(mast.descriptions$`MAST chapter ID`),unique(mast.descriptions$`MAST chapter name`), sep = ' - '))

g20.implemented.harmful.measures.policies$mast.chapter.names = plyr::mapvalues(g20.implemented.harmful.measures.policies$mast.chapter, unique(mast.descriptions$`MAST chapter ID`)
                                             , MAST.chapter.descriptions)

plot.6.2.c = ggplot(data = g20.implemented.harmful.measures.policies, aes(x=period, y = n, fill=mast.chapter.names)) + 
  geom_col(position='stack') + 
  scale_fill_manual(name='', values = gta_colour$qualitative, labels=g20.implemented.harmful.measures.policies$mast.chapter.names ) + 
  xlab('Period') + 
  gta_theme() +
  ylab('Number of harmful policy instruments implemented by G20') + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) 
plot.6.2.c
# d -----------------------------------------------------------------------
# Simon's request: A bar chart should be prepared for (d).
# 
# trade.coverage.estimates.df = trade.coverage.estimates[,9:14]
# trade.coverage.estimates.df = data.frame(periods = seq(2014,2019,1), trade.estimates = as.numeric(as.vector(trade.coverage.estimates.df[1,])))
# 
# #non-percentage share
# ggplot(data=trade.coverage.estimates.df) + geom_line(aes(x=periods,y=trade.estimates),colour=gta_colour$harmful[1], size=1.2) + 
#   ylim(c(0,1)) + xlab('Year') + ylab('Share of trade covered by G20 implemented harmful measures') +
#   gta_theme()
#   
# #percentage share
# plot.6.2.d = ggplot(data=trade.coverage.estimates.df) + geom_line(aes(x=periods,y=trade.estimates*100),colour=gta_colour$harmful[1], size=1.2) +
#   geom_point(aes(x=periods,y=trade.estimates*100),colour=gta_colour$harmful[1], size=3) +
#   xlab('Year') + ylab('Percentage of world trade covered by G20 implemented harmful measures') + 
#   scale_y_continuous(breaks = seq(0,80,10),labels=paste0(seq(0,80,10),'%'), limits = (c(0,85))) + gta_theme()
# 
# plot.6.2.d


# save plots --------------------------------------------------------------

gta_plot_saver(plot=plot.6.2.a,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.1 - Number of harmful G20 implemented measures")

gta_plot_saver(plot=plot.6.2.b,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.2 - Share of harmful G20 implemented measures")

gta_plot_saver(plot=plot.6.2.c,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.3 - Top 5 harmful policy instruments implemented by G20")

# gta_plot_saver(plot=plot.6.2.d,
#                path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
#                name="Figure 1.4 - Number of harmful G20 implemented measures")
# 

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
setwd('C:/Users/Kamran/Dropbox/GTA cloud')
#setwd('D:/Dropbox/Dropbox/GTA cloud')

mast.descriptions = gtalibrary::int.mast.types

chapter.number = 1
chapter.title = 'G20 Commercial Policy Record'
output.path = paste(chapter.number,chapter.title,sep = ' - ')

gta.evaluation = c('Red', 'Amber')

year.list = list(year1  <- c(ymd('2014-12-01'), ymd('2015-04-15')),
          year2  <- c(ymd('2015-12-01'), ymd('2016-04-15')),
          year3  <- c(ymd('2016-12-01'), ymd('2017-04-15')),
          year4  <- c(ymd('2017-12-01'), ymd('2018-04-15')),
          year5  <- c(ymd('2018-12-01'), ymd('2019-04-15')))

periods = c('01/12/14-15/04/15','01/12/15-15/04/16', '01/12/16-15/04/17', 
                  '01/12/17-15/04/18', '01/12/18-15/04/19')

period.labels = c('01/12/14-\n15/04/15','01/12/15-\n15/04/16', '01/12/16-\n15/04/17', 
                  '01/12/17-\n15/04/18', '01/12/18-\n15/04/19')


# 1 -----------------------------------------------------------------------
# a -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the (a) total number of G20 amber and red implemented measures

total.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                    implementing.country = 'G20',
                    keep.implementation.na = F,
                    implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])

  total.implemented.harmful.measures[year] = length(unique(master.sliced$intervention.id))

}

plotting.data = data.frame(periods = 1:5, periods = periods, total.implemented.harmful.measures)

table.fig.1 = plotting.data[,c('periods','total.implemented.harmful.measures')]
names(table.fig.1) = c('Period', 'Total Implemented Harmful measures')
xlsx::write.xlsx(table.fig.1, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ",chapter.number,".1 - Data.xlsx", sep=""))

# b -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (b) the share of amber and red implemented measures by the G20 in all measures implemented by the G20

share.implemented.harmful.measures = c()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  temp = master.sliced
  
  gta_data_slicer(gta.evaluation= c('Red', 'Amber', 'Green'),
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  share.implemented.harmful.measures[year] = length(unique(temp$intervention.id))/length(unique(master.sliced$intervention.id))

}

plotting.data$share.implemented.harmful.measures = share.implemented.harmful.measures

table.fig.2 = plotting.data[,c('periods','share.implemented.harmful.measures')]
names(table.fig.2) = c('Period', 'Share Implemented Harmful measures')
xlsx::write.xlsx(table.fig.2, row.names=FALSE, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure ", chapter.number,".2 - Data.xlsx", sep=""))


# c -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (c) as the top five harmful policy instruments used by the G20 (and the other)

g20.members=c(32, 36, 76, 124, 156, 251, 276, 699, 360, 381, 392, 484, 410, 643, 682, 710, 792, 826, 840)
oecd.members=c(36, 40, 56, 124, 152, 203, 208, 233, 246, 251, 276, 300, 348, 352, 372, 376, 381, 392, 410, 428, 440, 442, 484, 528, 554, 578, 616, 620, 703, 705, 724, 752, 756, 792, 826, 840)
country.groups=list('G20' = g20.members, 'OECD' = oecd.members)
# 
# harmful.instruments.per.year = data.frame()
# for (year in 1:length(year.list)){
#   for (c.group in 1:length(country.groups)){
#     if (names(country.groups[c.group]) == 'G20'){keep.implementer.boolean = T}
#     if (names(country.groups[c.group]) == 'OECD'){keep.implementer.boolean = F}
#     
#     gta_data_slicer(gta.evaluation= gta.evaluation,
#                     implementing.country = country.groups[[c.group]],
#                     keep.implementer = keep.implementer.boolean,
#                     keep.implementation.na = T,
#                     implementation.period = c(year.list[[year]]))
#     
#     master.sliced=subset(master.sliced, date.published<=year.list[[year]][2], select = c('mast.chapter','intervention.id'))
#     master.sliced=subset(master.sliced, !duplicated(master.sliced))
#     master.sliced=dplyr::count(master.sliced,mast.chapter)
#     
#     master.sliced$period=year
#     master.sliced$country.group=names(country.groups[c.group])
#     
#     harmful.instruments.per.year = rbind(harmful.instruments.per.year,master.sliced)
#   }
# }

# c -----------------------------------------------------------------------


g20.implemented.harmful.measures.policies = data.frame()

#get most frequent policy instruments over the 5 years
for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
  g20.policies = g20.policies[!duplicated(g20.policies),]
  g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) 
  g20.policies.chapters$period = year
  
  g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)

}

top5.frequent.policies = g20.implemented.harmful.measures.policies %>% group_by(mast.chapter) %>% summarise(most.frequent = sum(n)) %>% dplyr::top_n(5)
top5.frequent.policies = top5.frequent.policies$mast.chapter

g20.implemented.harmful.measures.policies = data.frame()

for (year in 1:length(year.list)){
  gta_data_slicer(gta.evaluation= gta.evaluation,
                  implementing.country = 'G20',
                  keep.implementation.na = F,
                  implementation.period = c(year.list[[year]]))
  
  ## adjusting for interventions reported by end of period
  master.sliced=subset(master.sliced, date.published<=year.list[[year]][2])
  
  g20.policies = master.sliced[,colnames(master.sliced) %in% c('mast.chapter','intervention.id')]
  g20.policies = g20.policies[!duplicated(g20.policies),]
  g20.policies.chapters = g20.policies %>% dplyr::count(mast.chapter) %>% filter(mast.chapter %in% top5.frequent.policies)
  g20.policies.others = g20.policies[!(g20.policies$mast.chapter %in% g20.policies.chapters$mast.chapter),]
  g20.policies.others = g20.policies.others[!duplicated(g20.policies.others),]
  g20.policies.chapters = rbind(g20.policies.chapters, data.frame(mast.chapter = 'Others', n = nrow(g20.policies.others)))
  g20.policies.chapters$period = year
  
  g20.implemented.harmful.measures.policies = rbind(g20.implemented.harmful.measures.policies, g20.policies.chapters)
  
}



# d -----------------------------------------------------------------------

# Simon's request: In each of these five years I am interested in the  
# (d) the total amount of trade covered by the G20 harmful measures.

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

# 2 - Plotting ----------------------------------------------------------------

# a -----------------------------------------------------------------------
# Simon's request: A line chart for (a) and (b) should be prepared.

gta_colour_palette()

plot.6.2.a = ggplot(plotting.data,aes(x = periods, y = total.implemented.harmful.measures)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour= gta_colour$harmful[1], size=3) +
  ylab('Number of G20 implemented harmful interventions') +
  xlab('Period') + ylim(c(0, 300)) + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + gta_theme()

plot.6.2.a

# b -----------------------------------------------------------------------
# Simon's request: A line chart for (a) and (b) should be prepared.

plot.6.2.b = ggplot(plotting.data,aes(x=periods, y=share.implemented.harmful.measures*100)) + 
  geom_line(colour=gta_colour$harmful[1], size=1.2) + 
  geom_point(colour=gta_colour$harmful[1], size=3) +
  xlab('Period') +  ylab('Percentage of G20 implemented measures which are harmful') + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) + 
  scale_y_continuous(breaks = seq(0,100,10),labels=paste0(seq(0,100,10),'%'), limits = (c(60,100))) + gta_theme()

plot.6.2.b

# c -----------------------------------------------------------------------
# Simon's request: A stacked bar chart should be prepared for (c).

MAST.chapter.descriptions = unique(mast.descriptions$`MAST chapter name`)

# MAST.chapter.descriptions = c(paste(unique(mast.descriptions$`MAST chapter ID`),unique(mast.descriptions$`MAST chapter name`), sep = ' - '))

g20.implemented.harmful.measures.policies$mast.chapter.names = plyr::mapvalues(g20.implemented.harmful.measures.policies$mast.chapter, unique(mast.descriptions$`MAST chapter ID`)
                                             , MAST.chapter.descriptions)

plot.6.2.c = ggplot(data = g20.implemented.harmful.measures.policies, aes(x=period, y = n, fill=mast.chapter.names)) + 
  geom_col(position='stack') + 
  scale_fill_manual(name='', values = gta_colour$qualitative, labels=g20.implemented.harmful.measures.policies$mast.chapter.names ) + 
  xlab('Period') + 
  gta_theme() +
  ylab('Number of harmful policy instruments implemented by G20') + 
  scale_x_continuous(breaks = plotting.data$periods,labels=period.labels) 
plot.6.2.c
# d -----------------------------------------------------------------------
# Simon's request: A bar chart should be prepared for (d).
# 
# trade.coverage.estimates.df = trade.coverage.estimates[,9:14]
# trade.coverage.estimates.df = data.frame(periods = seq(2014,2019,1), trade.estimates = as.numeric(as.vector(trade.coverage.estimates.df[1,])))
# 
# #non-percentage share
# ggplot(data=trade.coverage.estimates.df) + geom_line(aes(x=periods,y=trade.estimates),colour=gta_colour$harmful[1], size=1.2) + 
#   ylim(c(0,1)) + xlab('Year') + ylab('Share of trade covered by G20 implemented harmful measures') +
#   gta_theme()
#   
# #percentage share
# plot.6.2.d = ggplot(data=trade.coverage.estimates.df) + geom_line(aes(x=periods,y=trade.estimates*100),colour=gta_colour$harmful[1], size=1.2) +
#   geom_point(aes(x=periods,y=trade.estimates*100),colour=gta_colour$harmful[1], size=3) +
#   xlab('Year') + ylab('Percentage of world trade covered by G20 implemented harmful measures') + 
#   scale_y_continuous(breaks = seq(0,80,10),labels=paste0(seq(0,80,10),'%'), limits = (c(0,85))) + gta_theme()
# 
# plot.6.2.d


# save plots --------------------------------------------------------------

gta_plot_saver(plot=plot.6.2.a,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.1 - Number of harmful G20 implemented measures")

gta_plot_saver(plot=plot.6.2.b,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.2 - Share of harmful G20 implemented measures")

gta_plot_saver(plot=plot.6.2.c,
               path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
               name="Figure 1.3 - Top 5 harmful policy instruments implemented by G20")

# gta_plot_saver(plot=plot.6.2.d,
#                path=paste("0 report production/GTA 24/tables & figures/",output.path, sep=""),
#                name="Figure 1.4 - Number of harmful G20 implemented measures")
# 


