prospects<-read.csv('C:/Users/sshss-pc/Desktop/project/CaptainU/prospects.csv', header = TRUE)
sumprospects<-function(df){
  row<-nrow(df)
  result<-data.frame(rep(NA,row),rep(NA,row),rep(NA,row))
  names(result)<-c("Athlete_id","Sum","update")
  result[1,1]<-df[1,1]
  result[1,2]<-1
  result[1,3]<-as.character(df[1,5])
  current<-1
  for(n in 2:row){
    if(df[n,1]==df[n-1,1]){
      result[current,2]<-result[current,2]+1
      x<-as.POSIXct(df[n,5])
      y<-as.POSIXct(df[n-1,5])
      if(x>y){
        result[current,3]<-as.character(df[n,5])
      }
    }
    if(df[n,1]!=df[n-1,1]){
      current<-current+1
      result[current,1]<-df[n,1]
      if(df[n,2]=="NULL"){
        result[current,2]<-0
        result[current,3]<-"Null"
      }
      if(df[n,2]!="NULL"){
        result[current,2]<-1
        result[current,3]<-as.character(df[n,5])
      }
    }
  }
  return(result)
}
prospectsum<-sumprospects(prospects)
part9<-prospects[4000000:4380000,]
part9sum<-sumprospects(part9)

part8<-prospects[3500000:3999999,]
part8sum<-sumprospects(part8)
write.csv(part8sum,"C:/Users/sshss-pc/Desktop/part8sum.csv")

part7<-prospects[3000000:3499999,]
part7sum<-sumprospects(part7)
write.csv(part7sum,"C:/Users/sshss-pc/Desktop/part7summ.csv")

part6<-prospects[2500000:2999999,]
part6sum<-sumprospects(part6)
write.csv(part6sum,"C:/Users/sshss-pc/Desktop/part6summ.csv")

part5<-prospects[2000000:2499999,]
part5sum<-sumprospects(part5)
write.csv(part5sum,"C:/Users/sshss-pc/Desktop/part5summ.csv")

part4<-prospects[1500000:1999999,]
part4sum<-sumprospects(part4)
write.csv(part4sum,"C:/Users/sshss-pc/Desktop/part4summ.csv")

part3<-prospects[1000000:1499999,]
part3sum<-sumprospects(part3)
write.csv(part3sum,"C:/Users/sshss-pc/Desktop/part3summ.csv")

part2<-prospects[500000:999999,]
part2sum<-sumprospects(part2)
write.csv(part2sum,"C:/Users/sshss-pc/Desktop/part2summ.csv")

part1<-prospects[0:499999,]
part1sum<-sumprospects(part1)
write.csv(part1sum,"C:/Users/sshss-pc/Desktop/part1summ.csv")

eventsum<-eventsum[1:332699,]
write.csv(eventsum,"C:/Users/sshss-pc/Desktop/eventsum.csv")
