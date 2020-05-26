#气泡图
#https://zhuanlan.zhihu.com/p/29328848

#============= read data ===============
#mydata <- read.csv("cita_ms_20180920.csv",header = T, sep = ";")
#citation data
mydata<-read.csv("bubbl_1_biggerthan3.csv",header = T, sep = ";", stringsAsFactors = FALSE,check.names = FALSE)
#textual similarity data
mydata_sim<-read.csv("mandis_1.csv",header = T, sep = ";", stringsAsFactors = FALSE,check.names = FALSE)

#============ 透视表 ===================
#mydata$Class[c(1,6,7)]<-c("Housing,fuel\n&utilities","Restaurants\n& hotels","Clothing\n& footwear")
#names(mydata)[9:11]<-c("Saudi\nArabia","South\nKorea","United\nStates")
library("tidyr")
mydata1<-gather(mydata,topic_cited_by,num,-1) #数据透视表！常用！
#mydata1$class<-factor(mydata1$class,levels=c("MS40", "MS39", "MS38", "MS37", "MS36", "MS35", "MS34", "MS33", "MS32", "MS31", "MS30", "MS29", "MS28", "MS27", "MS26", "MS25", "MS24", "MS23", "MS22", "MS21", "MS20", "MS19", "MS18", "MS17", "MS16", "MS15", "MS14", "MS13", "MS12", "MS11", "MS10", "MS9", "MS8", "MS7", "MS6", "MS5", "MS4", "MS3", "MS2", "MS1", "OR30", "OR29", "OR28", "OR27", "OR26", "OR25", "OR24", "OR23", "OR22", "OR21", "OR20", "OR19", "OR18", "OR17", "OR16", "OR15", "OR14", "OR13", "OR12", "OR11", "OR10", "OR9", "OR8", "OR7", "OR6", "OR5", "OR4", "OR3", "OR2", "OR1"),ordered=T)
mydata1$class<-factor(mydata1$class,levels=c("OR30", "OR29", "OR28", "OR27", "OR26", "OR25", "OR24", "OR23", "OR22", "OR21", "OR20", "OR19", "OR18", "OR17", "OR16", "OR15", "OR14", "OR13", "OR12", "OR11", "OR10", "OR9", "OR8", "OR7", "OR6", "OR5", "OR4", "OR3", "OR2", "OR1"),ordered=T)
mydata1$topic_cited_by<-factor(mydata1$topic_cited_by,levels=c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6", "MS7", "MS8", "MS9", "MS10", "MS11", "MS12", "MS13", "MS14", "MS15", "MS16", "MS17", "MS18", "MS19", "MS20", "MS21", "MS22", "MS23", "MS24", "MS25", "MS26", "MS27", "MS28", "MS29", "MS30", "MS31", "MS32", "MS33", "MS34", "MS35", "MS36", "MS37", "MS38", "MS39", "MS40"),ordered=T)

mydata1_sim<-gather(mydata_sim,topic,sim,-1) #数据透视表！常用！
#mydata1_sim$class<-factor(mydata1_sim$class,levels=c("MS40", "MS39", "MS38", "MS37", "MS36", "MS35", "MS34", "MS33", "MS32", "MS31", "MS30", "MS29", "MS28", "MS27", "MS26", "MS25", "MS24", "MS23", "MS22", "MS21", "MS20", "MS19", "MS18", "MS17", "MS16", "MS15", "MS14", "MS13", "MS12", "MS11", "MS10", "MS9", "MS8", "MS7", "MS6", "MS5", "MS4", "MS3", "MS2", "MS1", "OR30", "OR29", "OR28", "OR27", "OR26", "OR25", "OR24", "OR23", "OR22", "OR21", "OR20", "OR19", "OR18", "OR17", "OR16", "OR15", "OR14", "OR13", "OR12", "OR11", "OR10", "OR9", "OR8", "OR7", "OR6", "OR5", "OR4", "OR3", "OR2", "OR1"),ordered=T)
mydata1_sim$class<-factor(mydata1_sim$class,levels=c("OR30", "OR29", "OR28", "OR27", "OR26", "OR25", "OR24", "OR23", "OR22", "OR21", "OR20", "OR19", "OR18", "OR17", "OR16", "OR15", "OR14", "OR13", "OR12", "OR11", "OR10", "OR9", "OR8", "OR7", "OR6", "OR5", "OR4", "OR3", "OR2", "OR1"),ordered=T)
mydata1_sim$topic<-factor(mydata1_sim$class,levels=c("MS1", "MS2", "MS3", "MS4", "MS5", "MS6", "MS7", "MS8", "MS9", "MS10", "MS11", "MS12", "MS13", "MS14", "MS15", "MS16", "MS17", "MS18", "MS19", "MS20", "MS21", "MS22", "MS23", "MS24", "MS25", "MS26", "MS27", "MS28", "MS29", "MS30", "MS31", "MS32", "MS33", "MS34", "MS35", "MS36", "MS37", "MS38", "MS39", "MS40"),ordered=T)

#=========== 分割区间 ==================
qa<-quantile(mydata1_sim$sim,c(0,.25,.5,.75,1)) #0 1 2 5 209 #sim:0 1.65 1.68 1.7 1.77
#mydata1$dis_fact<-cut(mydata1_sim$sim,breaks=qa,labels = c("lowest distance","below average","above average","highest distance"),include.lowest=TRUE,ordered=T)
mydata1$o<-cut(mydata1_sim$sim,breaks=qa,labels = c("highest similarity","above average","below average","lowest similarity"),include.lowest=TRUE,ordered=T)

#=========== 制作草图 ==================
library("ggplot2")
library("grid")
library("showtext")
library("Cairo")

CairoPNG(file="matirx_scatter.png",width=1800,height=800)
showtext_begin()
ggplot(data=mydata1)+
  geom_hline(aes(x=topic_cited_by,y=class,yintercept = 1:nrow(mydata1)),size=20,colour="#E4EDF2",alpha=.5)+
  # geom_vline(aes(x=topic_cited_by,y=class,xintercept = 1:nrow(mydata1)),linetype="dashed")+
  geom_point(aes(x=topic_cited_by,y=class,size=num,fill=o),shape=21,colour="white")+
  scale_fill_manual(values=c("#41B0C3","#519F46","#F1B255","#F9DBD3"))+ #值越小颜色越深
  scale_size_area(max_size=28)+
  scale_x_discrete(position = "top")+
  guides(size=FALSE,fill=guide_legend(title="Topic similarity",direction="horizontal"))+
  labs(title="Citation and similarties between topics: MS citing OR")+ #,subtitle="Househlod spending*,of total,2013 or latest,includes taxes",caption="Source:Eurostat"
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

