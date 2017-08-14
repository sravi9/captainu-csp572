monthlydata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/monthlydata.csv', header = TRUE)
library(stringr)
library(lubridate)
str(monthlydata)
names(monthlydata)[1]<-"athlete_id"
monthlydata[,4]<-as.POSIXct(monthlydata[,4])
monthlydata[,5]<-as.POSIXct(monthlydata[,5])
monthlydata<-monthlydata[,c(-9,-12,-15,-18,-21,-24,-27,-31)]
addchurn<-function(df){
  row<-nrow(df)
  result<-data.frame(df)
  result[,6]<-as.character(result[,6])
  result[,6]<-replace(result[,6],values = "NA")
  names(result)[6]<-"Timetag"
  
  for(n in 1:(row-1)){
    
    y<-year(result[n,4])
    m<-month(result[n,4])
    d<-day(result[n,4])
    if(d<16){
      timetag<-paste(y,"-",m,"-1")
      timetag<-str_replace_all(timetag,"[ ]","")
    }
    if(d>15){
      if(m==12){
        y<y+1
        m<-1
        timetag<-paste(y,"-",m,"-1")
        timetag<-str_replace_all(timetag,"[ ]","")
      }
      if(m<12){
        m<-m+1
        timetag<-paste(y,"-",m,"-1")
        timetag<-str_replace_all(timetag,"[ ]","")
      }
    }
    result[n,6]<-timetag
    
    if(df[n,1]!=df[n+1,1]){
      
      if(is.na(result[n,24])==TRUE){
        result[n,24]="TRUE"
      }
      if(is.na(result[n,25])==TRUE){
        result[n,25]="FALSE"
      }
    }
    if(is.na(result[n,24])==TRUE){
      result[n,24]="FALSE"
    }
    if(is.na(result[n,25])==TRUE){
      result[n,25]="FALSE"
    }
  }
  if(is.na(result[row,24])==TRUE){
    result[row,24]="TRUE"
  }
  if(is.na(result[row,25])==TRUE){
    result[row,25]="FALSE"
  }
  return(result)
}

testdata<-monthlydata[1:1000,]
testresult<-addchurn(testdata)
write.csv(testresult,"C:/Users/sshss-pc/Desktop/testresult.csv")

transdata<-addchurn(monthlydata)
write.csv(transdata,"C:/Users/sshss-pc/Desktop/transdata.csv")

transdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/transdata.csv', header = TRUE)
transdata<-transdata[,-1]

mmerge<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/mmergetest.csv', header = TRUE)
mmerge<-mmerge[,c(-1,-3,-4,-5,-7,-8,-10,-12,-17,-18,-19,-20,-21,-24,-26,-28,-29)]
analysisdata<-merge(transdata,mmerge,by = "athlete_id")
write.csv(analysisdata,"C:/Users/sshss-pc/Desktop/analysisdata.csv")

analysisdata<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/analysisdata.csv', header = TRUE)
analysisdata<-analysisdata[,-1]
churn<-analysisdata[which(analysisdata[,67]=="1"),]
write.csv(churn,"C:/Users/sshss-pc/Desktop/churn.csv")
levels(churn[,"Timetag"])
