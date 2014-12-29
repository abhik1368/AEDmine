# Compare two query drugs
source("function.R")
source("riskratio.R")
library(xtable)
d1<-get.Names("Lipitor")
d2<-get.Names("Crestor")
d1se<-get.SideEffects(d1,"Lipitor")
d2se<-get.SideEffects(d2,"Crestor")
# Without outcome
p1<-get.Vis(d1se,d2se,75,col=c("blue","red"))
p1$save("compare2.html",cdn=TRUE)
# With outcome hospitilization and other serious
p2<-get.Vis(d1se,d2se,75,col=c("blue","red"),out=c("OT","HO"))
p2$save("compare2out.html",cdn=TRUE)



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
r1.prr<-prr(crestor,lipitor)
r1.ror<-ror(crestor,lipitor)
# Write top 10 reactions output as xtable html format
print(xtable(head(r1.prr,10)) , type="html",file="compare_results_prr.html")
print(xtable(head(r1.ror,10)) , type="html",file="compare_results_ror.html")


# compare lipitor against crestor 
r2<-riskratio(lipitor,crestor)
# Write top 10 reactions output as xtable html format
print(xtable(head(r2,10)), type="html",file="compare_results2.html")
