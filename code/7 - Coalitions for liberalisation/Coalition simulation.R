library(gtalibrary)
library(splitstackshape)
rm(list=ls())
setwd("C:/Users/jfrit/Desktop/Dropbox/GTA cloud")

chapter.number = 7
chapter.title = 'Coalitions for liberalisation'
output.path = paste(chapter.number,chapter.title,sep = ' - ')


## Choosing trade data
gta_trade_value_bilateral(trade.data="2017",df.name="trade")
setnames(trade, "hs6","affected.product")
total.imports=aggregate(trade.value ~ i.un + affected.product, trade, sum)
setnames(total.imports, "trade.value","total.imports")
total.exports=aggregate(trade.value ~ a.un + affected.product, trade, sum)
setnames(total.exports, "trade.value","total.exports")



## What tariff lines are part of the prize? i.e currently discriminated against
gta_data_slicer(gta.evaluation = c("Red","Amber"),
                in.force.today = T,
                mast.chapters = "D",
                keep.mast = F)

liberalisation.options=unique(subset(master.sliced, is.na(affected.product)==F)[,c("intervention.id","i.un","affected.product")])
liberalisation.options=cSplit(liberalisation.options, which(names(liberalisation.options)=="affected.product"), sep=",", direction="long")
liberalisation.options=aggregate(intervention.id ~., liberalisation.options, function(x) length(unique(x)))
barrier.count=unique(liberalisation.options[,c("i.un","affected.product","intervention.id")])
setnames(barrier.count, "intervention.id","nr.of.hits")

liberalisation.options=merge(merge(liberalisation.options, trade, by=c("i.un","affected.product")), total.imports, by=c("i.un","affected.product"))
liberalisation.options$market.share=liberalisation.options$trade.value/liberalisation.options$total.imports

prize.allocation=unique(liberalisation.options[,c("i.un","affected.product","a.un","market.share")])

## loop growth from above; remove areas where no nucleus; proceed with those that have
# growth.rates=c(seq(.5,.1,-.05),seq(.09,.01,-.01))
growth.rates=1

## Export/import weights
participation.threshold=0
# exporter.weight=1
# domestic.producer.weight=1
# consumer.weight=0
# relative.import.utility=(consumer.weight-domestic.producer.weight)/exporter.weight

import.weight=seq(-1.5,1.5,.25)

## define areas of cooperation
# areas.of.cooperation=data.frame(cpc=unique(cpc.to.hs$cpc[cpc.to.hs$hs %in% unique(liberalisation.options$affected.product)]),
                                level=3)
areas.of.cooperation=data.frame(cpc=unique(as.numeric(substr(sprintf(fmt = "%03i", areas.of.cooperation$cpc),1,2))),
                                      level=2)



## initialise records
coalition.members=data.frame()
coalition.stats=data.frame(coalition.id=numeric(),
                           sector.scope=character(),
                           sector.level=numeric(),
                           sector.name=character(),
                           import.utility.weight=numeric(),
                           member.size=numeric(),
                           members.liberalising=numeric(),
                           freerider.count=numeric(),
                           coalition.total.trade=numeric(),
                           coalition.liberalised.trade=numeric(),
                           intra.coalition.liberalised.trade=numeric(),
                           share.world.imports=numeric(),
                           share.world.imports.liberalised=numeric())


step=1
for(growth in growth.rates){
  for(i in 1:nrow(areas.of.cooperation)){
    
    if(areas.of.cooperation$level[i]==3){
      area=areas.of.cooperation$cpc[i]
      
      s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==3 & cpc.names$cpc %in% area])), collapse="")
      s.scope=paste(area, collapse = ";")
      
    } else {
      
      area=gta_cpc_code_expand(areas.of.cooperation$cpc[i])
      
      s.name=paste(unique(as.character(cpc.names$cpc.name[cpc.names$cpc.digit.level==2 & cpc.names$cpc %in% areas.of.cooperation$cpc[i]])), collapse="")
      s.scope=areas.of.cooperation$cpc[i]
    }
    
    
    print(paste("STARTING AREA",area))
    area.codes=intersect(subset(cpc.to.hs, cpc %in% area)$hs,unique(liberalisation.options$affected.product))
    exporters=unique(trade$a.un[trade$affected.product %in% area.codes])
    
    prize.allocation.area=subset(prize.allocation, affected.product %in% area.codes)
    area.world.imports=sum(subset(total.imports, affected.product %in% area.codes)$total.imports)
    
    for(i.weight in import.weights){
      
      print(paste("STARTING import weight",i.weight))
      
      ## initialise: only countries with an upside will join the coalition i.e. exporters
      benefactors=exporters
      coalition=exporters
      free.riders=c()
      net.income=data.frame(result=-1)
      
      relative.import.utility=i.weight
      
      while(length(coalition)>0 & nrow(subset(net.income, result<participation.threshold))>0){
        
        ## assuming only GTA-recorded barriers are open for liberalisation
        the.prize=merge(subset(barrier.count, i.un %in% coalition & 
                                 affected.product %in% area.codes), 
                        total.imports, 
                        by=c("i.un","affected.product"))
        
        the.prize=subset(the.prize, is.na(total.imports)==F)
        
        ## invoking post-liberalisation growth assumption
        the.prize$total.imports=the.prize$total.imports*growth
        
        
        ## extrapolating 2017 market shares for prize allocation
        p.a=subset(prize.allocation.area, a.un %in% benefactors)
        p.a=merge(p.a, aggregate(market.share ~ i.un + affected.product, p.a, sum), by=c("i.un","affected.product"), all.x=T)
        p.a$market.share=p.a$market.share.x/p.a$market.share.y
        p.a$market.share.x=NULL
        p.a$market.share.y=NULL
        
        prize.distribution=merge(p.a, the.prize, by=c("i.un","affected.product"))
        
        ## are the coalition partners also trading partners?
        if(nrow(prize.distribution)>0){
          prize.distribution$prize.earned=prize.distribution$market.share*prize.distribution$total.imports
          
          income.won=aggregate(prize.earned ~ a.un + affected.product, prize.distribution, sum)
          income.lost=aggregate(prize.earned ~ i.un + affected.product, prize.distribution, sum)
          
          setnames(income.won, "a.un","i.un")
          net.income.by.code=merge(income.won, income.lost, by=c("i.un", "affected.product"), all=T)
          net.income.by.code[is.na(net.income.by.code)]=0
          setnames(net.income.by.code, "prize.earned.x","new.exports")
          setnames(net.income.by.code, "prize.earned.y","new.imports")
          
          net.income=merge(aggregate(new.imports ~ i.un, net.income.by.code, sum),
                           aggregate(new.exports ~ i.un, net.income.by.code, sum),
                           by="i.un", all=T)
          net.income$result=net.income$new.exports+relative.import.utility*net.income$new.imports
          
          coalition=intersect(coalition, net.income$i.un[net.income$result>=participation.threshold])
          
          
        } else {
          coalition=c()
          net.income=data.frame(result=-1)
        }
        
        free.riders=setdiff(exporters, coalition)  
        
        # print(sum(income.won$prize.earned[income.won$i.un%in%coalition])/sum(income.won$prize.earned))
        # print(sum(income.lost$prize.earned))
        # print(paste(length(coalition), "coalition partners to",length(free.riders), "freeriders"))
        
      }
      
      ## storing result
      c.id=length(unique((coalition.stats$coalition.id)))+1
      
      if(length(coalition)>0){
        m.count=length(coalition)
        f.count=length(free.riders)
        lib.count=nrow(subset(net.income, result>=participation.threshold & new.imports!=0))
        
        
        
        c.t.trade=sum(subset(total.imports, i.un %in% coalition & affected.product %in% area.codes)$total.imports)
        c.l.trade=sum(the.prize$total.imports)/growth
        intra.c.l.trade=sum(subset(prize.distribution, a.un %in% coalition)$prize.earned)/growth
        
        imp.share=c.t.trade/area.world.imports
        imp.share.liberalised=c.l.trade/area.world.imports
        
        
        coalition.members=rbind(coalition.members, 
                                data.frame(coalition.id=c.id,
                                           i.un=coalition,
                                           free.rider=F))
        
        
        if(length(free.riders)>0){
          coalition.members=rbind(coalition.members, data.frame(coalition.id=c.id,
                                                                i.un=free.riders,
                                                                free.rider=T))
        }
        
      } else {
        m.count=0
        lib.count=0
        f.count=0
        
        c.t.trade=0
        c.l.trade=0
        intra.c.l.trade=0
        imp.share=0
        imp.share.liberalised=0
        
        
      }
      
      
      coalition.stats=rbind(coalition.stats,
                            data.frame(coalition.id=c.id,
                                       sector.scope=s.scope,
                                       sector.level=areas.of.cooperation$level[i],
                                       sector.name=s.name,
                                       import.utility.weight=i.weight,
                                       member.size=m.count,
                                       members.liberalising=lib.count,
                                       freerider.count=f.count,
                                       coalition.total.trade=c.t.trade,
                                       coalition.liberalised.trade=c.l.trade,
                                       intra.coalition.liberalised.trade=intra.c.l.trade,
                                       share.world.imports=imp.share,
                                       share.world.imports.liberalised=imp.share.liberalised))
                                       
      
      rm(m.count, f.count, c.t.trade, c.l.trade, intra.c.l.trade,imp.share,imp.share.liberalised)
      
      
      
      
    }
    
    print(paste("FINISHED AREA",area,"(",step,"out of",nrow(areas.of.cooperation),")."))
    step=step+1
    print(paste("FINISHED AREA",area))
    
  }
  
  
}

c.s.xlsx=coalition.stats

names(c.s.xlsx)=c("Coalition ID", "Sectoral scope (CPC 3-digit)","Sector name","Import utility weight", "Nr of coalition members", 
                  "Nr of members which liberalise", "Nr of freeriding exporters","Total imports by coalition",
                  "Intra-coalition imports","Share of coalition's imports in world trade")

xlsx::write.xlsx(c.s.xlsx, file="0 report production/GTA 24/tables & figures/7 - Coalitions for liberalisation/Coalition size by relative import weight.xlsx", row.names=F)
save(coalition.stats, coalition.members, file="0 report production/GTA 24/data/7 - Coalitions for liberalisation/Coalition results.Rdata")


