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


#ANOSIM and SIMPER
ano <- anosim(cal_fem_dist2,cal_fem_data2$Morphospecies)
ano

sim<-simper(cal_fem_dist2, cal_fem_data2$Morphospecies, permutations = 100)
summary(sim)

#Callynthrophora males
cally_male <- read_xlsx("Callynthrophora male binary matrix .xlsx")
cal_m_data <- t(cally_male)
colnames(cal_m_data) <- cal_m_data[1,]
cal_m_data <- cal_m_data[-1,]
cal_m_data2 <- as.matrix(cal_m_data)
cmd <- cal_m_data2[,2:ncol(cal_m_data2)]

cal_m_dist <- dist(cmd, method = "binary")

cal_m_dist2 <- as.matrix(cal_m_dist) 

pcoa_cm <- cmdscale(cal_m_dist2, eig=T, add=T) 
positions <- pcoa_cm$points
head(positions)
colnames(positions) <-c("pcoa1", "pcoa2")
pcoa_cm

percent_explained <- 100 * pcoa_cm$eig /sum(pcoa_cm$eig)
pretty_pe <- round(percent_explained[1:2], digits = 1)
pretty_pe
labs <- c(glue("PCo 1 ({pretty_pe[1]}%)"),
          glue("PCo 2 ({pretty_pe[2]}%)"))

positions %>%
  as_tibble (rownames="samples") %>%
  ggplot(aes(x=pcoa1, y=pcoa2, label = samples)) + 
  geom_point(aes(color = )) + 
  labs(x=labs[1], y=labs[2])

cal_m_data2 = as.data.frame(cal_m_data2)

ano <- anosim(cal_m_dist2,cal_m_data2$Morphospecies)
ano

sim<-simper(cal_m_dist2, cal_m_data2$Morphospecies, permutations = 100)
summary(sim)

#__________________________________________________________Corsomyza

#Corsomyza males
corso_male <- read_xlsx("Corsomyza male binary matrix.xlsx")
cor_m_data <- t(corso_male)
colnames(cor_m_data) <- cor_m_data[1,]
cor_m_data <- cor_m_data[-1,]
cor_m_data2 <- as.matrix(cor_m_data)
cormd <- cor_m_data2[,3:ncol(cor_m_data2)]

cor_m_dist <- dist(cormd, method = "binary")
cor_m_dist2 <- as.matrix(cor_m_dist)

pcoa_corm <- cmdscale(cor_m_dist2, eig=T, add=T) 
positions <- pcoa_corm$points
head(positions)
colnames(positions) <-c("pcoa1", "pcoa2")
pcoa_corm

percent_explained_corm <- 100 * pcoa_corm$eig /sum(pcoa_corm$eig)
pretty_pe_corm <- round(percent_explained_corm[1:2], digits = 1)
pretty_pe_corm
labs <- c(glue("PCo 1 ({pretty_pe_corm[1]}%)"),
          glue("PCo 2 ({pretty_pe_corm[2]}%)"))

positions %>%
  as_tibble (rownames="samples") %>%
  ggplot(aes(x=pcoa1, y=pcoa2, label = samples)) + 
  geom_point() + 
  labs(x=labs[1], y=labs[2])

cor_m_data2 = as.data.frame(cor_m_data2)
head(cor_m_data2)
str(cor_m_data2)
ano <- anosim(cor_m_dist2,cor_m_data2$clade)
ano
ano <- anosim(cor_m_dist2,cor_m_data2$species)
ano
sim_corm<-simper(cor_m_dist2, cor_m_data2$species, permutations = 999)
summary(sim_corm)
















