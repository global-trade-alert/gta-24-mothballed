<<<<<<< HEAD:code/1 - G20 Commercial Policy Record/Chapter 1 script.R
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


# mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')
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

plotting.data = data.frame(periods = 1:5, total.implemented.harmful.measures)

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

# c -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (c) as the top five harmful policy instruments used by the G20 (and the other)

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

#recycled code since time is short 
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

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
g20.trade.covered=data.frame()

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     implementers = 'G20',
                     implementation.period = year.list[[year]],
                     reporting.period=c(year.list[[year]]),
                     coverage.period = c(c.period, c.period),
                     trade.statistic = "value",
                     trade.data = r.year)
  
  g20.trade.covered=rbind(g20.trade.covered, 
                          data.frame(period.start=year.list[[year]][1],
                                     period.end=year.list[[year]][2],
                                     trade.value=trade.coverage.estimates[,4]))
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(g20.trade.covered, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Data.xlsx", sep=""))


# 2 - Plotting ----------------------------------------------------------------

period.labels = c('01/12/14-\n15/04/15','01/12/15-\n15/04/16', '01/12/16-\n15/04/17', 
                  '01/12/17-\n15/04/18', '01/12/18-\n15/04/19')


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


# mast.descriptions = readxl::read_xlsx('R help files/GTA-MAST.xlsx')
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

plotting.data = data.frame(periods = 1:5, total.implemented.harmful.measures)

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

# c -----------------------------------------------------------------------
# Simon's request: In each of these five years I am interested in the  (c) as the top five harmful policy instruments used by the G20 (and the other)

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

#recycled code since time is short 
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

# The more I think about it, you are right, this is what he wants, it actually wants makes sense
g20.trade.covered=data.frame()

for (year in 1:length(year.list)){
  
  r.year=as.character(min(year(year.list[[year]])))
  c.period=max(year(year.list[[year]]))
  if(r.year>2017){r.year="2017"}

  gta_trade_coverage(gta.evaluation = gta.evaluation,
                     implementers = 'G20',
                     implementation.period = year.list[[year]],
                     reporting.period=c(year.list[[year]]),
                     coverage.period = c(c.period, c.period),
                     trade.statistic = "value",
                     trade.data = r.year)
  
  g20.trade.covered=rbind(g20.trade.covered, 
                          data.frame(period.start=year.list[[year]][1],
                                     period.end=year.list[[year]][2],
                                     trade.value=trade.coverage.estimates[,4]))
  
  print(paste(year.list[[year]], collapse=" - "))
}

xlsx::write.xlsx(g20.trade.covered, file=paste("0 report production/GTA 24/tables & figures/",output.path,"/Figure 1.4 - Data.xlsx", sep=""))


# 2 - Plotting ----------------------------------------------------------------

period.labels = c('01/12/14-\n15/04/15','01/12/15-\n15/04/16', '01/12/16-\n15/04/17', 
                  '01/12/17-\n15/04/18', '01/12/18-\n15/04/19')


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
>>>>>>> 1c7c6261717ba59f61b7793aa59eec505a4e39f6:code/1 - G20 Commercial Policy Record/Chapter 1 script.R
