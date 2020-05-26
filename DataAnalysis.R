library(jsonlite)
library(dplyr)
library(readr)
url104<-"http://ipgod.nchc.org.tw/dataset/b6f36b72-0c4a-4b60-9254-1904e180ddb1/resource/98d5094d-7481-44b5-876a-715a496f922c/download/a17000000j-020066-mah.csv"
url107<-"C:/Users/SAMUEL/Desktop/4133da254dbcdba28a2097de48d8d606_csv/job107.csv"
job104 <- read_csv(url104)
job107 <- read_csv(url107)

job104$大職業別<-gsub("部門","",job104$大職業別)
job107$大職業別<-gsub("_","、",job107$大職業別)
job104$大職業別<-gsub("營造業","營建工程",job104$大職業別)
job107$大職業別<-gsub("出版、影音製作、傳播及資通訊服務業","資訊及通訊傳播業",job107$大職業別)
job107$大職業別<-gsub("教育業","教育服務業",job107$大職業別)
job107$大職業別<-gsub("醫療保健業","醫療保健服務業",job107$大職業別)
job107$`大學-薪資`<-gsub("—","",job107$`大學-薪資`)
job104$`大學-薪資`<-gsub("—","",job104$`大學-薪資`)
job107$`大學-薪資`<-gsub("…","",job107$`大學-薪資`)
job104$`大學-女/男`<-gsub("—","",job104$`大學-女/男`)
job107$`大學-女/男`<-gsub("—","",job107$`大學-女/男`)
job104$`大學-女/男`<-gsub("…","",job104$`大學-女/男`)
job107$`大學-女/男`<-gsub("…","",job107$`大學-女/男`)
job107$`研究所-薪資`<-gsub("—","",job107$`研究所-薪資`)
job107$`研究所-薪資`<-gsub("…","",job107$`研究所-薪資`)
jobJoin<-full_join(job104,job107,c("大職業別"))
#跳

jobJoin$`大學-薪資.y`<-as.numeric(jobJoin$`大學-薪資.y`)
jobJoin$`大學-薪資.x`<-as.numeric(jobJoin$`大學-薪資.x`)
jobRaise<-filter(jobJoin,jobJoin$`大學-薪資.y`>jobJoin$`大學-薪資.x`)
jobRaise<-mutate(jobRaise,Ratio=jobRaise$`大學-薪資.y`/jobRaise$`大學-薪資.x`)
jobRaise<-arrange(jobRaise,desc(Ratio))
knitr::kable(head(jobRaise[order(jobRaise$Ratio,decreasing=T),
                           c("大職業別","Ratio")],10))

#


fivePercent <-filter(jobRaise,Ratio>1.05)
knitr::kable(fivePercent[,c("大職業別","Ratio")])
#
fivePercent$大職業別<-gsub("-+[\u4e00-\u9fa5]*","",fivePercent$大職業別)
table(fivePercent$大職業別)

#
job104$`大學-女/男`<-as.numeric(job104$`大學-女/男`) 
job107$`大學-女/男`<-as.numeric(job107$`大學-女/男`)
gender104<-filter(job104,`大學-女/男`>0)
gender107<-filter(job107,`大學-女/男`>0)

#104年男生薪資比女生薪資多的職業
knitr::kable(head(gender104[order(gender104$`大學-女/男`,decreasing=F),c("大職業別","大學-女/男")],10))
#104年女生生薪資比女男生薪資多的職業
gender104<-filter(gender104,`大學-女/男`>100)
knitr::kable(head(gender104[order(gender104$`大學-女/男`,decreasing=T),c("大職業別","大學-女/男")],10))
#107年男生薪資比女生薪資多的職業

knitr::kable(head(gender107[order(gender107$`大學-女/男`,decreasing=F),c("大職業別","大學-女/男")],10))
#107年女生薪資比男生薪資多的職業
gender107<-filter(job107,`大學-女/男`>100)
knitr::kable(head(gender107[order(gender107$`大學-女/男`,decreasing=T),c("大職業別","大學-女/男")],10))
#跳
job107$`大學-薪資`<-as.numeric(job107$`大學-薪資`)
job107$`研究所-薪資`<-as.numeric(job107$`研究所-薪資`)
job107<-mutate(job107,Institute=job107$`研究所-薪資`/job107$`大學-薪資`)
head(job107[order(job107$Institute,decreasing=T),c("大職業別","Institute")],10)
#
favorite<-job107[c("78","99","127"),c("大職業別","大學-薪資","研究所-薪資")]
knitr::kable(favorite)
#
favorite<-mutate(favorite,Raise=favorite$`研究所-薪資`-favorite$`大學-薪資`)












