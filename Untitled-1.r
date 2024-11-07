pacman::p_load(shiny, readxl, dplyr, tidyr, ggplot2, lubridate, miceadds,
               DT, plotly, viridis, corrplot)
source.all("anc/")
valcodes <- read.csv("lib/validatiecodesSAM.csv", sep = ";")
addResourcePath("www", "www")