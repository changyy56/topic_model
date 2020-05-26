#气泡图
#https://zhuanlan.zhihu.com/p/29328848

#============= read data ===============
#mydata <- read.csv("cita_ms_20180920.csv",header = T, sep = ";")
#citation data
mydata<-read.csv("bubbl_ms_cita.csv",header = T, sep = ";", stringsAsFactors = FALSE,check.names = FALSE)
#textual similarity data
mydata_sim<-read.csv("mandis.csv",header = T, sep = ";", stringsAsFactors = FALSE,check.names = FALSE)

#============ 透视表 ===================
#mydata$Class[c(1,6,7)]<-c("Housing,fuel\n&utilities","Restaurants\n& hotels","Clothing\n& footwear")
#names(mydata)[9:11]<-c("Saudi\nArabia","South\nKorea","United\nStates")
library("tidyr")
mydata1<-gather(mydata,topic_cited_by,num,-1) #数据透视表！常用！
mydata1$class<-factor(mydata1$class,levels=c("T40", "T39", "T38", "T37", "T36", "T35", "T34", "T33", "T32", "T31", "T30", "T29", "T28", "T27", "T26", "T25", "T24", "T23", "T22", "T21", "T20", "T19", "T18", "T17", "T16", "T15", "T14", "T13", "T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01"),ordered=T)
mydata1_sim<-gather(mydata_sim,topic,sim,-1) #数据透视表！常用！
mydata1_sim$class<-factor(mydata1_sim$class,levels=c("T40", "T39", "T38", "T37", "T36", "T35", "T34", "T33", "T32", "T31", "T30", "T29", "T28", "T27", "T26", "T25", "T24", "T23", "T22", "T21", "T20", "T19", "T18", "T17", "T16", "T15", "T14", "T13", "T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01"),ordered=T)

#=========== 分割区间 ==================
qa<-quantile(mydata1_sim$sim,c(0,.25,.5,.75,1)) #0 1 2 5 209 #sim:0 1.65 1.68 1.7 1.77
#mydata1$dis_fact<-cut(mydata1_sim$sim,breaks=qa,labels = c("lowest distance","below average","above average","highest distance"),include.lowest=TRUE,ordered=T)
mydata1$dis_fact<-cut(mydata1_sim$sim,breaks=qa,labels = c("highest similarity","above average","below average","lowest similarity"),include.lowest=TRUE,ordered=T)

#=========== 制作草图 ==================
library("ggplot2")
library("grid")
library("showtext")
library("Cairo")

CairoPNG(file="matirx_scatter.png",width=1250,height=1300)
showtext_begin()
ggplot(data=mydata1)+
  geom_hline(aes(x=topic_cited_by,y=class,yintercept = 1:nrow(mydata1)),size=20,colour="#E4EDF2",alpha=.5)+
 # geom_vline(aes(x=topic_cited_by,y=class,xintercept = 1:nrow(mydata1)),linetype="dashed")+
  geom_point(aes(x=topic_cited_by,y=class,size=num,fill=dis_fact),shape=21,colour="white")+
  scale_fill_manual(values=c("#41B0C3","#519F46","#F1B255","#F9DBD3"))+ #值越小颜色越深
  scale_size_area(max_size=23)+
  scale_x_discrete(position = "top")+
  guides(size=FALSE,fill=guide_legend(title="Topic similarity",direction="horizontal"))+
  labs(title="Citation and similarties between topics in journal Management Science")+ #,subtitle="Househlod spending*,of total,2013 or latest,includes taxes",caption="Source:Eurostat"
  theme_void(base_size=20) %+replace%
  theme(
    legend.position="top",
    panel.grid.major.x=element_line(linetype="dashed"),      #plot.margin=margin(5,5,5,5,unit="pt"),
    axis.text=element_text(size=15,hjust=0.5),
    plot.title=element_text(size=35,hjust=0,lineheight=1.2),
    plot.caption=element_text(hjust=0,lineheight=1.2)
  ) 
showtext.end()
dev.off()

