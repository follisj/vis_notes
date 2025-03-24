library(tidyverse)
library(socviz)
library(ggrepel)
library(viridis)

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  geom_point()+
  geom_label_repel(aes(label=winner_label,col=win_party))

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional"
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p <- ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))
p + geom_hline(yintercept=0.5,size=1.4,color="gray80") +
  geom_vline(xintercept=0.5,size=1.4,color="gray80")+
  geom_point()+
  geom_text_repel(aes(col=two_term))+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::percent)+
  labs(x=x_label,
       y=y_label,
       title=p_title,
       subtitle=p_subtitle,
       caption=p_caption)+
  theme_bw()

p + geom_hline(yintercept=0.5,size=1.4,color="gray80") +
  geom_vline(xintercept=0.5,size=1.4,color="gray80")+
  geom_point()+
  geom_text_repel(aes(col=two_term))+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::percent)+
  labs(x=x_label,
       y=y_label,
       title=p_title,
       subtitle=p_subtitle,
       caption=p_caption)+
  facet_grid(win_party~.)+
  theme_bw()


###
ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  coord_flip()

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  labs(title="Election Stuff",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="yellow2"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))+
  coord_flip()



elections_historic$year2 <- cut(elections_historic$year,breaks=c(1800,1850,1900,1950,1999,2050))
levels(elections_historic$year2) <- c("1800-1850","1851-1900","1900-1950","1951-1999","2000-2020")

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  geom_point(aes(col=win_party))+
  geom_label_repel()+
  facet_grid(year2~.)

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point()+
  geom_vline(xintercept=.5)+
  geom_hline(yintercept=.5)+
  geom_label_repel(aes(fill=win_party))+
  #scale_color_manual(values=c("yellow","blue","black","red"))+
  facet_grid(.~year2)


ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5)+
  #geom_hline(yintercept=.5)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_color_viridis()+
  #scale_color_manual(values=c("yellow","blue","black","red"))+
  facet_grid(.~year2,scales="free_y")

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  #geom_hline(yintercept=1929)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="cividis")+
  theme_bw()+
  facet_wrap(.~year2,scales="free_y",nrow=1)



ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  labs(title="Election Stuff",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="yellow2"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))


data.crash=data.frame(popular_pct=c(.3,.3),
                      year=c(1929,1943),
                      year2=c("1900-1950","1900-1950"),
                      winner_label=c(NA,NA),
                      event=c("Stock Market Crash 1929",
                              "World War II"))
ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_text_repel(data=data.crash,aes(popular_pct,year,label=event),col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="cividis")+
  theme_bw()+
  facet_wrap(.~year2,scales="free_y",nrow=1)

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="cividis")+
  theme_bw()+
  labs(title="Election Stuff",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="azure3"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="gray"))+
  facet_wrap(.~year2,scales="free_y",nrow=1)



ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="cividis")+
  theme_bw()+
  labs(title="Election Stuff",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="azure3"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))+
  facet_wrap(.~year2,scales="free_y",nrow=1)





data.crash=data.frame(popular_pct=c(.3,.3),
                      year=c(1929,1941.5,1914.5,1863,1871,1935,2008,1972.5),
                      year2=c("1900-1950","1900-1950","1900-1950","1851-1900","1851-1900","1900-1950","2000-2020","1951-1999"),
                      winner_label=c(NA,NA,NA,NA,NA,NA,NA,NA),
                      event=c("Stock Market Crash",
                              "World\nWar II",
                              "World\nWar I",
                              "Civil War",
                              "Reconstruction",
                              "Great\nDepression",
                              "Great\nRecession",
                              "Watergate"))
ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  #geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1914,ymax=1918,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1861,ymax=1865,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1865,ymax=1877,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1929,ymax=1941,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=2008,ymax=2009,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1972,ymax=1974,fill="gray",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  labs(title="Election Stuff",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="yellow2"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))+
  facet_wrap(.~year2,scales="free_y",nrow=1)


ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  #geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1914,ymax=1918,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1861,ymax=1865,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1865,ymax=1877,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1929,ymax=1941,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=2008,ymax=2009,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1972,ymax=1974,fill="gray",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  labs(title="Election Stuff",
       subtitle="test.text - annotation",
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="yellow2"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))+
  facet_wrap(.~year2,scales="free_y",nrow=1)





library(ggtext)
library(scales)
show_col(viridis_pal()(20))


test.text="There does not appear to be an overall relationship between electoral college percentage and popular vote.<br>
We see  <span style='color:#FDE725FF'>high</span> electoral college percentages (Roosevelt, Nixon, Reagan) 
and <span style='color:#440154FF'>low</span> electoral college percentages (Adams)<br>
associated with high and low popular vote percentages respectively.<br>
However, with electoral college percentages between <span style='color:#3F4788FF'>around 40%</span> 
to <span style='color:#3CBC75FF'>around 70%</span> it is not as clear.<br>
<span style='color:#FF0000'>(probably because my labels are covering some of the points)</span>"


ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  #geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1914,ymax=1918,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1861,ymax=1865,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1865,ymax=1877,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1929,ymax=1941,fill="gray46",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=2008,ymax=2009,fill="gray",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1972,ymax=1974,fill="gray",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="red",size=4)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="viridis")+
  scale_x_continuous(labels=scales::percent)+
  theme_bw()+
  labs(title="Election Stuff",
       subtitle=test.text,
       x="Popular Vote Percentage",
       y="Election Year",
       fill="Percent of\nElectoral\nVotes")+
  theme(plot.title=element_text(hjust=.5,size=25),
        plot.subtitle=element_markdown(),
        axis.text.y=element_text(face="bold",size=10),
        strip.background=element_rect(fill="yellow2"),
        strip.text.x=element_text(color="red",face="bold",size=12),
        legend.position = "top",
        legend.background=element_rect(fill="cornsilk1"),
        plot.background = element_rect(fill="cornsilk3"))+
  facet_wrap(.~year2,scales="free_y",nrow=1)
