# Function to calculate proportional risk ration
# param d1: dataframe containing side effects for drug 1
# param d2: dataframe containing side effects for drug 2

prr<- function (d1,d2){
  rat<-data.frame()
  for (i in 1:dim(d2)[1]){
    r<-as.character(d2$reaction[i])
    c2 <-d2$count[i]
    c1<-d1[d1$reaction==r,]$count
    t1<-sum(d1$count)
    t2<-sum(d2$count)
    if ( length(c1) > 0){
      #ratio<-c1/c2
      prr<-(c1/t1)/(c2/t2)
      advr<-cbind(r,prr)
      #print (advr)
      rat<-rbind(rat,advr)
      rat$prr<-as.numeric(rat$prr)
      tab<-rat[order(rat$prr, decreasing = T), ]
      colnames(tab)[1]<-"reaction"
    }   
  }
  return (tab)
}

# Function to calculate reporting odds ratio
# param d1: dataframe containing side effects for drug 1
# param d2: dataframe containing side effects for drug 2

ror <- function (d1,d2){
  rat<-data.frame()
  for (i in 1:dim(d2)[1]){
    r<-as.character(d2$reaction[i])
    c2 <-d2$count[i]
    c1<-d1[d1$reaction==r,]$count
    t1<-sum(d1$count)-c1
    t2<-sum(d2$count)-c2
    if ( length(c1) > 0){
      ror<-(c1/c2)/(t1/t2)
      advr<-cbind(r,ror)
      rat<-rbind(rat,advr)
      rat$ror<-as.numeric(rat$ror)
      tab<-rat[order(rat$ror, decreasing = T), ]
      colnames(tab)[1]<-"reaction"
    }   
  }
  return (tab)
}
