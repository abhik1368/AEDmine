# Function to calculate Risk Ratio
# param d1: dataframe containing side effects for drug 1
# param d2: dataframe containing side effects for drug 2

riskratio <- function (d1,d2){
  rat<-data.frame()
  for (i in 1:dim(d2)[1]){
    r<-as.character(d2$reaction[i])
    c2 <-d2$count[i]
    c1<-d1[d1$reaction==r,]$count
    t1<-sum(d1$count)
    t2<-sum(d2$count)
    if ( length(c1) > 0){
      #ratio<-c1/c2
      ratio<-(c1/t1)/(c2/t2)
      advr<-cbind(r,ratio)
      #print (advr)
      rat<-rbind(rat,advr)
      rat$ratio<-as.numeric(rat$ratio)
      tab<-rat[order(rat$ratio, decreasing = T), ]
    }   
  }
  return (tab)
}
