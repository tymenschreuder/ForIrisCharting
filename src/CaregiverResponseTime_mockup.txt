options(scipen=999); rm(list = ls()); graphics.off(); gc()

library("mongolite")
library("rmongodb")
library("ggplot2")
library("data.table")

## Get events from the db and clean up

# TODO - in the method, have a parameter for num_days_ago_start, default to 7.
# NTH: allow a date range to be passed in, takes precendence over days ago.

###########
# Set up the params
###########
p = list()
p$dir = "C:/Users/ghaith/Documents/irisdataprep/data/"
p$uri = "mongodb://130.211.163.207"

###########
# Mock up Staff response time
###########
events = readRDS( paste0(p$dir,"events.RDS") );
IB = readRDS(paste0(p$dir,"interventions_behaviors.RDS"));

dtList = list()
for ( name in unique(events$staffName ) ) {
  nSamples = runif(1) * 1000 + 50;
  tmp = data.table( residentId = sample( events$residentId, nSamples, replace = T),
            staffName = substring(digest(name),1,10),
            delay = rnorm( nSamples, mean=60*(runif(1)-.1), sd=20*runif(1) ) )

  tmp[sample(1:nSamples,floor(nSamples*runif(1)),replace=F),delay:=NA]
  dtList[[name]] = tmp;
}

dtAll = rbindlist(dtList);

###########
# Mock up Staff response time
###########

pdf( file = "IRIS_MOCK_StaffDelay.pdf" )

delay_by_staff = dtAll[,list(.N,meanDelay=mean(delay,na.rm=T)),by=list(staffName)]
delay_by_staff[,staffName:=factor(staffName,levels=delay_by_staff[order(meanDelay),staffName])]
p = ggplot(data=delay_by_staff,aes(x=staffName, y=meanDelay) ) +
  geom_bar(stat="identity", colour="black", width=1.0) +
  coord_flip() +
  ggtitle("Delay by Staff")
show(p)

missed_by_staff = dtAll[,list(count=.N,countNAs=sum(is.na(delay))),by=list(staffName)]
missed_by_staff[,percMissed:=countNAs/count]
missed_by_staff[,staffName:=factor(staffName,levels=missed_by_staff[order(percMissed),staffName])]
p = ggplot(data=missed_by_staff,aes(x=staffName, y=percMissed) ) +
  geom_bar(stat="identity", colour="black", width=1.0) +
  coord_flip() +
  ggtitle("Percentage of Missed Visits by Staff")
show(p)

graphics.off()
