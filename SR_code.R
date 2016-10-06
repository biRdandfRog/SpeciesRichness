##species richness analyses in R
#by Colleen Nell

###load libraries
library(vegan)


#read in data in site x species community matrix, with grouping variable as first column if comparing mutliple groups
df<-read.csv("FILENAME.HERE.csv")
BCI<-data(BCI)##or use BCI dataset provided in 'vegan'


#look at dimensions
str(BCI) 

bci.rar<-specaccum(BCI,method="rarefaction") #run rarefaction

#get diversity estimates


bci.rar##extract values for plotting

##using iNEXT

