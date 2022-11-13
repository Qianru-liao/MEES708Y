
#Qianru Liao

#This function plots...
library(dplyr)
library(reshape2)
library(tidyr)
library(plotly)
library(ggpp)
library(xts)
library(zoo)
library(lubridate)
myplot <- function(x,  
                   plotmean = TRUE, 
                   color="black",
                   Average_color =color, 
                   Median_color="grey",
                   Q1_Q3_color="light blue",
                   title="input title here"){
  if((dim(x%>%data.frame)[[2]]<2)|| (class(x)=="numeric")){
    return (print("Error! Input data is not qualified.Please change input data type!"))
  }
  if((dim(x%>%data.frame)[[2]]>1)&&(class(x)=="data.frame")){
    Median = apply(x%>%na.omit(), 1, median, na.rm=T)
    Average=apply(x%>%na.omit(), 1, mean, na.rm=T)
    Q1=apply(x%>%na.omit(),1,function(a) quantile(a, 0.25))
    Q3=apply(x%>%na.omit(),1,function(a) quantile(a, 0.75))
    Time =  rownames(x%>%na.omit())%>%as.numeric()%>%date_decimal()
    data <- data.frame(
      Time=as.Date(Time),
      Median,
      Average,
      Q1,
      Q3)
    if(plotmean){
      plot_final=ggplot(data, aes(x=Time)) +
        geom_ribbon(aes(ymin = Q1, ymax = Q3, fill="Q1&Q3"))+
        geom_point(aes(y=Average,color="Average"))+
        geom_line(aes(y=Average,color="Average"))+ #,size=0.1 #Average_color
        #geom_point(aes(y=Average,shape=1,size=0.5,color="Average"))+ #add points, according to the template plot the homework gave
        geom_line(aes(y=Median,color="Median")) + 
        xlab("Year") +
        ylab("Value") +
        #theme_ipsum() +
        #theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_fill_manual(name = "Fill",values = c("Q1&Q3"= Q1_Q3_color))+
        scale_color_manual(name = "Line",values = c(Average_color, Median_color))+
        theme_classic()+
        ggtitle(title)
    }
    else{
      plot_final=ggplot(data, aes(x=Time)) +
        geom_ribbon(aes(ymin = Q1, ymax = Q3, fill="Q1&Q3"))+
        #geom_line(aes(y=Average,color="Average"))+ #,size=0.1 #Average_color
        #geom_point(aes(y=Average,shape=1,size=0.5,color="Average"))+ #add points, according to the template plot the homework gave
        geom_line(aes(y=Median,color="Median")) + 
        xlab("Year") +
        ylab("Value") +
        #theme_ipsum() +
        #theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_fill_manual(name = "Fill",values=c("Q1&Q3"= Q1_Q3_color))+
        scale_color_manual(name = "Line",values=c(Average_color))+
        theme_classic()+
        ggtitle(title)
    }
  }
  else{
    df_1 = data.frame(date=time(x),Y=as.matrix(x))%>%na.omit()
    df= df_1[,-1]
    Median = apply(df, 1, median, na.rm=T)
    Average=apply(df, 1, mean, na.rm=T)
    Q1=apply(df,1,function(a) quantile(a, 0.25))
    Q3=apply(df,1,function(a) quantile(a, 0.75))
    Time =  df_1$date%>%as.numeric()%>%date_decimal()
    data <- data.frame(
      Time=as.Date(Time),
      Median,
      Average,
      Q1,
      Q3)
    if(plotmean){
      plot_final=ggplot(data, aes(x=Time)) +
        geom_ribbon(aes(ymin = Q1, ymax = Q3, fill="Q1&Q3"))+
        geom_point(aes(y=Average,color="Average"))+
        geom_line(aes(y=Average,color="Average"))+ #,size=0.1 #Average_color
        #geom_point(aes(y=Average,shape=1,size=0.5,color="Average"))+ #add points, according to the template plot the homework gave
        geom_line(aes(y=Median,color="Median")) + 
        xlab("Year") +
        ylab("Value") +
        #theme_ipsum() +
        #theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_fill_manual(name = "Fill",values = c("Q1&Q3"= Q1_Q3_color))+
        scale_color_manual(name = "Line",values = c(Average_color, Median_color,'black'))+
        theme_classic()+
        ggtitle(title)
    }
    else{
      plot_final=ggplot(data, aes(x=Time)) +
        geom_ribbon(aes(ymin = Q1, ymax = Q3, fill="Q1&Q3"))+
        #geom_line(aes(y=Average,color="Average"))+ #,size=0.1 #Average_color
        #geom_point(aes(y=Average,shape=1,size=0.5,color="Average"))+ #add points, according to the template plot the homework gave
        geom_line(aes(y=Median,color="Median")) + 
        xlab("Year") +
        ylab("Value") +
        #theme_ipsum() +
        #theme(axis.text.x=element_text(angle=60, hjust=1))+
        scale_fill_manual(name = "Fill",values=c("Q1&Q3"= Q1_Q3_color))+
        scale_color_manual(name = "Line",values=c(Average_color))+
        theme_classic()+
        ggtitle(title)
    }
  }
  return (plot_final)
}


# the last line of the file