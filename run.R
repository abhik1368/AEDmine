# Compare two query drugs
source("function.R")
source("riskratio.R")
library(xtable)
d1<-get.Names("Lipitor")
d2<-get.Names("Crestor")
d1se<-get.SideEffects(d1,"Lipitor")
d2se<-get.SideEffects(d2,"Crestor")
get.Vis(d1se,d2se,75,col=c("blue","red"))


# Comparing the Risk adverse effect for two types of drugs
# Get the reactions of Crestor which matches Lipitor
d<-d2se[which(d1se$reaction %in% d2se$reaction ),]
n<-d[complete.cases(d),]

# Aggregat all the data without considering the outcomes
crestor<-aggregate(count ~ reaction ,data=n, FUN=sum)
head(dt[order(dt$count, decreasing = T), ])

# Get the reactions of Lipitor which matches Crestor from the above dataframe
t<-d1se[which(d$reaction %in% d1se$reaction),]
lipitor<-aggregate(count ~ reaction ,data=t, FUN=sum)

# compare crestor against Lipitor 
r1<-riskratio(crestor,lipitor)
# Write top 10 reactions output as xtable html format
print(xtable(head(r1,10)) , type="html",file="compare_results.html")

# compare lipitor against crestor 
r2<-riskratio(lipitor,crestor)
# Write top 10 reactions output as xtable html format
print(xtable(head(r1,10)), type="html",file="compare_results.html")