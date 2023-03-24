#PCoA of Mariobezziinae specimens 
#Input data
setwd("/Users/cassbarker/Desktop/Data - excel etc")

library(vegan)
library(tidyverse)
library(ggplot2)
library(readxl)
library(glue)
#________________________________________________________ Callynthrophora 
# female cally
cally_female <- read_xlsx("Callynthrophora female binary matrix.xlsx")
cal_fem_data <- t(cally_female)
colnames(cal_fem_data) <- cal_fem_data[1,]
cal_fem_data <- cal_fem_data[-1,]
cal_fem_data2 <- as.matrix(cal_fem_data)
cfd <- cal_fem_data2[,2:ncol(cal_fem_data2)]

cal_fem_dist <- dist(cfd, method = "binary")
cal_fem_dist2 <- as.matrix(cal_fem_dist)

pcoa_cf <- cmdscale(cal_fem_dist2, eig=T, add=T) 
positions <- pcoa_cf$points
head(positions)
colnames(positions) <-c("pcoa1", "pcoa2")


percent_explained <- 100 * pcoa_cf$eig /sum(pcoa_cf$eig)
pretty_pe <- round(percent_explained[1:2], digits = 1)
pretty_pe
labs <- c(glue("PCo 1 ({pretty_pe[1]}%)"),
          glue("PCo 2 ({pretty_pe[2]}%)"))

cal_fem_data2 = as.data.frame(cal_fem_data2)
head(positions)
positions %>%
  as_tibble (rownames="samples") %>%
  ggplot(aes(x=pcoa1, y=pcoa2, color = "samples")) + 
  geom_point() +
  labs(x=labs[1], y=labs[2])


















