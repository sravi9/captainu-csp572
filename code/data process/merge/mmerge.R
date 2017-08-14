mmerge<-read.csv('C:/Users/sshss-pc/Desktop/mmerge.csv', header = TRUE)
table(mmerge[,6])
1997/10/17 1997/11/13 1997/11/18  1997/11/9  1997/8/26  1997/8/27  1998/1/13  1998/1/19  1998/5/18   1998/8/1          f 
1          1          1          1          1          1          1          1          1          1     139280 
F          m          M       NULL 
9397     176080       7931          1 
mmerge[mmerge[,6]=="1997/10/17",]
mmerge[mmerge[,6]=="1997/11/13",]
mmerge[mmerge[,6]=="1997/11/18",]
mmerge[mmerge[,6]=="1997/11/9",]
mmerge[mmerge[,6]=="1997/8/26",]
mmerge[mmerge[,6]=="1997/8/27",]
mmerge[mmerge[,6]=="1998/1/13",]
mmerge[mmerge[,6]=="1998/1/19",]
mmerge[mmerge[,6]=="1998/5/18",]
mmerge[mmerge[,6]=="1998/8/1",]
mmerge[mmerge[,6]=="NULL",]

mmerge<-read.csv('C:/Users/sshss-pc/Desktop/mmergetest.csv', header = TRUE)
mmerge[mmerge[,6]=="M",6]<-"m"
mmerge[mmerge[,6]=="F",6]<-"f"

payingcustomer<-mmerge[mmerge[,22]<3,]
freeuser<-mmerge[mmerge[,22]>2,]
c0<-mmerge[mmerge[,22]==0,]
nrow(c0)
c1<-mmerge[mmerge[,22]==1,]
nrow(c1)
c2<-mmerge[mmerge[,22]==2,]
nrow(c2)

femcus<-payingcustomer[payingcustomer[,6]=="f",]
nrow(femcus)
malesuc<-payingcustomer[payingcustomer[,6]=="m",]
nrow(malesuc)
femuser<-mmerge[mmerge[,6]=="f",]
nrow(femuser)
maleuser<-mmerge[mmerge[,6]=="m",]
nrow(maleuser)

mean(payingcustomer[,"Vidsum"])
mean(freeuser[,"Vidsum"])
mean(payingcustomer[,"eventsum"])
mean(freeuser[,"eventsum"])

mmerge[,17]<-as.POSIXct(mmerge[,17])
mmerge[,18]<-as.POSIXct(mmerge[,18])

cus2012<-mmerge[mmerge[,17]<as.POSIXct("2013-1-1"),]
nrow(cus2012)
cus2013<-mmerge[mmerge[,17]<as.POSIXct("2014-1-1"),]
nrow(cus2013)
cus2014<-mmerge[mmerge[,17]<as.POSIXct("2015-1-1"),]
nrow(cus2014)
cus2015<-mmerge[mmerge[,17]<as.POSIXct("2016-1-1"),]
nrow(cus2015)
cus2016<-mmerge[mmerge[,17]<as.POSIXct("2016-5-1"),]
nrow(cus2016)


pay2012<-payingcustomer[payingcustomer[,17]<as.POSIXct("2013-1-1"),]
nrow(pay2012)
pay2013<-payingcustomer[payingcustomer[,17]<as.POSIXct("2014-1-1"),]
nrow(pay2013)
pay2014<-payingcustomer[payingcustomer[,17]<as.POSIXct("2015-1-1"),]
nrow(pay2014)
pay2015<-payingcustomer[payingcustomer[,17]<as.POSIXct("2016-1-1"),]
nrow(pay2015)
pay2016<-payingcustomer[payingcustomer[,17]<as.POSIXct("2016-5-1"),]
nrow(pay2016)

quit2012<-mmerge[mmerge[,18]<as.POSIXct("2013-1-1"),]
nrow(quit2012)
quit2013<-mmerge[mmerge[,18]<as.POSIXct("2014-1-1"),]
nrow(quit2013)
quit2014<-mmerge[mmerge[,18]<as.POSIXct("2015-1-1"),]
nrow(quit2014)
quit2015<-mmerge[mmerge[,18]<as.POSIXct("2016-1-1"),]
nrow(quit2015)
quit2016<-mmerge[mmerge[,18]<as.POSIXct("2016-5-1"),]
nrow(quit2016)

subscriptionsum<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/subscriptionsum.csv', header = TRUE)
subscriptionsum[,6]<-as.POSIXct(subscriptionsum[,6])
paysub<-subscriptionsum[subscriptionsum[,3]==1,]
unsub2012<-paysub[paysub[,6]<as.POSIXct("2013-1-1"),]
nrow(unsub2012)
unsub2013<-paysub[paysub[,6]<as.POSIXct("2014-1-1"),]
nrow(unsub2013)
unsub2014<-paysub[paysub[,6]<as.POSIXct("2015-1-1"),]
nrow(unsub2014)
unsub2015<-paysub[paysub[,6]<as.POSIXct("2016-1-1"),]
nrow(unsub2015)
unsub2016<-paysub[paysub[,6]<as.POSIXct("2016-5-1"),]
nrow(unsub2016)

table(mmerge[,"sport"])
table(payingcustomer[,"sport"])


mmergechurn<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/mmergechurn.csv', header = TRUE)
mmergetest<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/mmergetest.csv', header = TRUE)
prospects<-read.csv('C:/Users/sshss-pc/Desktop/project/processed/prospects.csv', header = TRUE)
memerge<-merge(mmergetest,prospects,by = "athlete_id")
memerge<-memerge[,-2]
write.csv(memerge,"C:/Users/sshss-pc/Desktop/mmergetestupdate.csv")

