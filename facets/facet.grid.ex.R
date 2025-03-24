library(tidyverse)
library(socviz)

#facets

names(gss_sm)
summary(gss_sm)

gss_sm %>%
  group_by(race,degree) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  #mutate(percent=n/sum(n)) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_wrap(.~race)
gss_sm %>%
  group_by(race,degree) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  #mutate(percent=n/sum(n)) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_wrap(race~.,ncol=2)

gss_sm %>%
  group_by(race,degree) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_grid(.~race)
gss_sm %>%
  group_by(race,degree) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_grid(race~.)

gss_sm %>%
  group_by(race,degree,sex) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_wrap(race~sex,ncol=2)
gss_sm %>%
  group_by(race,degree,sex) %>%
  drop_na() %>%
  summarize(n=n()) %>%
  ggplot(aes(degree,n,fill=degree)) +
  geom_col(position="dodge") +
  facet_grid(race~sex)


names(oecd_le)

ggplot(oecd_le,aes(year,lifeexp,col=country))+
  geom_line()

ggplot(oecd_le,aes(year,lifeexp))+
  geom_line(aes(col=country))+
  facet_grid(country~.)+
  theme_bw()+
  theme(legend.position = "none")

ggplot(oecd_le,aes(year,lifeexp))+
  geom_line(aes(col=country))+
  facet_wrap(.~country,nrow=7)+
  theme_bw()+
  theme(legend.position = "none")
ggplot(oecd_le,aes(year,lifeexp))+
  geom_line(aes(col=country))+
  facet_wrap(.~country,nrow=7)+
  theme_bw()+
  theme(legend.position = "none")
ggplot(oecd_le,aes(year,lifeexp))+
  geom_line(aes(col=country))+
  facet_wrap(~country)+
  labs(title="oecd",
       subtitle="stuff",
       y="life xp",
       x="year")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=90))


library(grid)
library(gridExtra)
library(cowplot)
names(mpg)
(g1 <- ggplot(mpg,aes(displ,hwy))+
  geom_point())
(g2 <- ggplot(mpg,aes(cyl))+
    geom_bar())
(g3 <- ggplot(mpg,aes(class,cty))+
    geom_boxplot(aes(col=class)))

grid.arrange(g1,g2,g3)
grid.arrange(g1,g2,g3,nrow=1)
grid.arrange(g1,arrangeGrob(g2,g3),nrow=2)
grid.arrange(g1,arrangeGrob(g2,g3,g3),ncol=2)
grid.arrange(g1,g1,g2,g3,ncol=2,
             layout_matrix=cbind(c(1,1,4),c(2,3,3),c(2,3,3)))
grid.arrange(g1,g1,g2,g3,nrow=3,
             layout_matrix=cbind(c(1,NA,2,4),c(1,3,3,4)))
grid.arrange(g1,g2,g3,
             layout_matrix=cbind(c(1,NA,NA),c(NA,2,NA),c(NA,NA,3)))


plot_grid(g1,g2,g3,labels=c("one","2","C"))
plot_grid(g1,g2,g3,labels=c("one","2","C"),ncol=1)
