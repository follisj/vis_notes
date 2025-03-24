library(tidyverse)
library(patchwork)
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
g1/g2/g3

grid.arrange(g1,g2,g3,nrow=1)
g1 | g2 | g3

grid.arrange(g1,arrangeGrob(g2,g3),nrow=2)
g1/(g2/g3)

grid.arrange(g1,arrangeGrob(g2,g3,g3),ncol=2)
g1 | (g2/g3/g3)

grid.arrange(g1,g1,g2,g3,ncol=2,
             layout_matrix=cbind(c(1,1,4),c(2,3,3),c(2,3,3)))
((g1/g3+plot_layout(heights=c(2,1))) | 
  (g1/g2+plot_layout(heights=c(1,2)))) + 
  plot_layout(widths=c(1,2))

layout <- "
ABB
ACC
DCC
"
g1+g1+g2+g3+plot_layout(design=layout)

grid.arrange(g1,g1,g2,g3,nrow=3,
             layout_matrix=cbind(c(1,NA,2,4),c(1,3,3,4)))
g1/((plot_spacer()/g1) | g2)/g3

grid.arrange(g1,g2,g3,
             layout_matrix=cbind(c(1,NA,NA),c(NA,2,NA),c(NA,NA,3)))
layout <- "
A##
#B#
##C
"
g1+g2+g3+plot_layout(design = layout)+
  plot_annotation(title="Diagonals only",
                  subtitle="just because",
                  tag_levels=c('A','1'),
                  tag_sep=".",
                  tag_prefix="Fig. "
                  )
