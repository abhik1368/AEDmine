library(RCurl)
library(jsonlite)

drugs<-read.csv("drug_names.csv",header=TRUE)
dn<-as.character(drugs$Name)
meshclass<-data.frame()
for (i in 1:length(dn)){
  url <- sprintf("http://rxnav.nlm.nih.gov/REST/rxclass/class/byDrugName.json?drugName=%s&relaSource=Mesh",dn[i]) 
  h <- getCurlHandle()
  d <- getURL(url, curl=h)
  data<-fromJSON(d)
  df<-data.frame(data$rxclassDrugInfoList$rxclassDrugInfo)
  if (dim(df)[1]==0) {next ;}
  else {
  df1<-cbind(data.frame(df[,1]$name),data.frame(df[,2]$classId),data.frame(df[,2]$className))
  colnames(df1)<-cbind("Name","classId","MeshClass")
  meshclass<-rbind(meshclass,df1)
  }
}

write.csv(meshclass,"MeshClass.csv")
d<-as.data.frame(table(as.character(meshclass$MeshClass)))
df<-d[order(d$Freq, decreasing = T),]
