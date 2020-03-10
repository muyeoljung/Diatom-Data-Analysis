setwd("J:/Rex")

library(tidyverse)
library(readxl)

Gov.Diatom <- read_xls("Gov_Diatom_20091-20182_Species List.xls")
Gov.Diatom <- Gov.Diatom[-1,-1] 
Gov.Diatom <- Gov.Diatom %>% select(-6)

Table_of_Species <-table(Gov.Diatom$Species) #Look at the number of categories in column


#Subset: 1(April) and 2(September)
Gov.Diatom.1 <- Gov.Diatom %>% filter(Half.Of.Year==1)
Gov.Diatom.2 <- Gov.Diatom %>% filter(Half.Of.Year==2)

#I will use Gov.Diatom 2 only
#

#BioCode -> replace it with my name in study

#extract three sites for comparison with my 15 data set



