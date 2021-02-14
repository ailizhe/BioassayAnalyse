library(foreign);
library(BioassayAnalyse);
data1<-read.csv("example_bliss.csv");
bliss(data1$AT,data1$sdose,data1$sm,data1$sr,data1$tdose,data1$tm,data1$tr);
