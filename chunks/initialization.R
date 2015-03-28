## ----initialization,echo=FALSE,include=FALSE-----------------------------

library(knitr)
opts_chunk$set(echo=FALSE, include=TRUE, fig.lp="fig:")
opts_chunk$set(fig.width=9.5, fig.height=7, fig.align="center",
               digits=4, fig.show="asis", fig.path="Figures/WR-")
thisFileName <- "Review"
require(Ranadu, quietly = TRUE, warn.conflicts=FALSE)
require(maps, quietly=TRUE)
require(mapdata, quietly=TRUE)
require(mapproj, quietly=TRUE)
require(ggplot2)
require(grid)
require(ggthemes)
Project <- sub (".*/", "", getwd())
setwd (sprintf ("~/RStudio/%s", Project))
run.args <- commandArgs (TRUE)


