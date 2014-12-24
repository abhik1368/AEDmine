library(foreach)
library(plyr)
library(doParallel)
c<-detectCores()
cl <- makeCluster(c)  
registerDoParallel(cl)

get.Names <- function(drug){
  dbclass<-read.csv("dbatc.csv",header=TRUE,stringsAsFactors=FALSE)
  dnames<-read.csv("brandnames.csv",header=TRUE,stringsAsFactors=FALSE)
  dn<-gsub("\n","",dnames$Name)
  dn<-gsub("^ ","",dn)
  dn<-gsub(" $","",dn)
  dname<-cbind(dnames[,1],as.data.frame(dn))
  colnames(dname)<-c("dbid","Name")
  bdname<-merge(dbclass,dname,by="dbid")
  bdname$Name<-as.character(bdname$Name)
  df1<- bdname[(grepl(drug, bdname$Name, ignore.case = T)),]
  count.dbid <- as.data.frame(table(as.character(df1$dbid)))
  dbid<-count.dbid[order(count.dbid$Freq, decreasing = T),]
  dbid.f<-as.character(dbid[1,1])
  # get all the brand names for a given query drug
  all.names<-as.character(bdname[bdname$dbid==dbid.f,]$Name)
  return(all.names)
}  

# all the atc codes
#atc<-unique(as.character(bdname[bdname$dbid==dbid.f,]$atc_code))

# You can find individual drugs and their reported adverse by specifying there names below

# There are commonly many names for a drug, see below where I put three grepl statements
# with text like, "put drug brand name here", you can replace this with drug brand names
# you can add or remote grepl statements depending on the number of brand names you want to look over.
# For all the names for a given query collect the data and bind to a dataframe

get.SideEffects <- function(all.names,drugname){
  d.data<-data.frame()
  patient <-read.table("faers_ascii_2012q4/ascii/demo12q4.txt",sep = "$", header = T, fill = T, quote = "")
  drug <- read.table("faers_ascii_2012q4/ascii/drug12q4.txt", sep = "$", header = T, fill = T, quote = "")
  reaction <- read.table("faers_ascii_2012q4/ascii/reac12q4.txt", sep = "$", header = T, fill = T, quote = "")
  outcomes <- read.table("faers_ascii_2012q4/ascii/outc12q4.txt", sep = "$", header = T, fill = T, quote = "")
  indication <-read.table("faers_ascii_2012q4/ascii/indi12q4.txt",sep = "$", header = T, fill = T, quote = "")
  response <- read.table("faers_ascii_2012q4/ascii/rpsr12q4.txt",sep = "$", header = T, fill = T, quote = "")
  ther <-read.table("faers_ascii_2012q4/ascii/ther12q4.txt",sep = "$", header = T, fill = T, quote = "")
  
  
  d.data <- foreach(i=1:length(all.names), .combine=rbind) %dopar% {
    drug[(grepl(all.names[i], drug$drugname, ignore.case = T)) & drug$drug_seq == 1, ] }
  
  df.d <- merge(d.data, reaction, by = "primaryid") # let's merge the drug file with reactions
  df.d <- merge(df.d, outcomes, by = "primaryid")# we'll bring in outcomes
  d.adr <- as.data.frame(table(df.d$pt, df.d$outc_code)) # count the instances of reactions and their outcomes
  names(d.adr) <- c("reaction", "outcome", "count")
  d.adr <- d.adr[order(d.adr$count, decreasing = T), ]
  n1.df<-d.adr[d.adr$count > 0,]
  n1.df$drug<-drugname
  return(n1.df)
}

pyramidPlot <- function(new.df,colors=NULL){
  library(rCharts)
  n1 <- nPlot(
    y = 'count',
    x = 'reaction',
    group = 'drug',
    type = 'multiBarHorizontalChart',
    data = new.df
    )
  n1$chart(stacked = TRUE)
  n1$chart(tooltipContent = "#! function(key, x, y){
   var format = d3.format('0,000');
   return '<h3>' + key + ', Adverse reaction : ' + x + '</h3>' +
   '<p>' + 'Counts : ' + y + '</p>'
   } !#")
  if (max(new.df$count >= 20)) {
    n1$yAxis(axisLabel = "Count",
             tickFormat = "#! function(d) {
            return d3.format(',.1f')(Math.abs(d) / 1)
             } !#")
    } else {
      n1$yAxis(axisLabel = "Count",
               tickFormat = "#! function(d) {
               return d3.format(',.0f')(Math.abs(d) / 1)
               } !#")
    }
   if (!is.null(colors)) {
     n1$chart(color = colors)
     }
   n1$chart(stacked = TRUE)
   n1$set(height=800,width=650)
   n1$chart(margin = list(left=200))
   n1
}


get.Vis <- function(d1,d2,top,col){
  d1$count<- -1 * d1$count
  new.df<-rbind(d1[1:top,],d2[1:top,])
  new.df<-new.df[order(new.df$count, decreasing = T), ]
  new.df$count<-as.numeric(new.df$count)
  new.df$drug<-as.factor(new.df$drug)
  print (head(new.df))
  p1<-pyramidPlot(new.df,colors=col)
  return(p1)
}