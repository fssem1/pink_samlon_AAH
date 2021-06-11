# helper file for stikine inseason management

# load ----
library(tidyverse)
library(readxl)
library(broom)
library(scales)
library(extrafont)
library(stats)
library(lubridate)
library(pwr)
library(Matching)
library(reshape)
library(reshape2)
library(grid)
library(gridExtra)
library(dplyr)
library(reshape2)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times New Roman') +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()))

#unique(dat$Fishery.Type.Escapement)
#  glimpse(recoveries_dat)
#str(dat)
#glimpse(recoveries)
#sapply(recoveries, unique)$End.Day
