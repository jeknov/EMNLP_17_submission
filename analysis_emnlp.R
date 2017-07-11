##### Load Data #####
df <- read.csv("emnlp_data_medians.csv")
df_all <- read.csv("emnlp_data_individual_hum_scores.csv")

##### Import Packages #####
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(psych)
library(multcomp)
library(corrgram)
library(corrplot)
library(qdap)

library(irr)
library(tidyr)
require(gridExtra)

##### Inter-rater Agreement on Human Ratings (Table 7) #####

    ######## Subset data by judge: ########
j1 = df_all %>%
  filter(df_all$judge == 1)

j2 = df_all %>%
  filter(df_all$judge == 2)

j3 = df_all %>%
  filter(df_all$judge == 3)

    ######## Subset data by dataset (ds) and system (sys): ########
ds <- c(as.character(j1$dataset), as.character(j2$dataset), as.character(j3$dataset))
sys <- c(as.character(j1$system), as.character(j2$system), as.character(j3$system))
j1_all <- c(j1$informativeness, j1$naturalness, j1$quality)
j2_all <- c(j2$informativeness, j2$naturalness, j2$quality)
j3_all <- c(j3$informativeness, j3$naturalness, j3$quality)
j_all <- cbind.data.frame(ds, j1_all, j2_all, j3_all, sys)

    ######## Calculate intra-class correlation coefficient: ########
icc(j_all[,2:4], unit = "a", model = "t") # 0.45, p<0.01

icc(subset(j_all, ds == "BAGEL")[,2:4], unit = "a") # 0.31, p<0.01
icc(subset(j_all, ds == "BAGEL" & sys=="LOLS")[,2:4], unit = "a") # 0.24, p<0.01
icc(subset(j_all, ds == "BAGEL" & sys=="Dusek")[,2:4], unit = "a") # 0.31, p<0.01

icc(subset(j_all, ds == "SFHOT")[,2:4], unit = "a") # 0.50, p<0.01
icc(subset(j_all, ds == "SFHOT" & sys =="WEN")[,2:4], unit = "a") # 0.52, p<0.01
icc(subset(j_all, ds == "SFHOT" & sys =="LOLS")[,2:4], unit = "a") # 0.50, p<0.01

icc(subset(j_all, ds == "SFRES")[,2:4], unit = "a") # 0.35, p<0.01
icc(subset(j_all, ds == "SFRES" & sys =="WEN")[,2:4], unit = "a") # 0.25, p<0.01
icc(subset(j_all, ds == "SFRES" & sys =="LOLS")[,2:4], unit = "a") # 0.38, p<0.01

# agreement on informativeness:
j1_inf <- c(j1$informativeness)
j2_inf <- c(j2$informativeness)
j3_inf <- c(j3$informativeness)
sys.out <- as.character(j1$sys_ref)
sys <- j1$system
j_inf <- cbind.data.frame(ds, j1_inf, j2_inf, j3_inf,sys)
icc(j_inf[,2:4], unit = "a", model = "t") # 0.39, p<0.01
icc(subset(j_inf, ds == "SFHOT")[,2:4], unit = "a") # 0.41, p<0.01
icc(subset(j_inf, ds == "SFHOT" & sys == "WEN")[,2:4], unit = "a") # 0.38, p<0.01
icc(subset(j_inf, ds == "SFHOT" & sys == "LOLS")[,2:4], unit = "a") # 0.38, p<0.01
icc(subset(j_inf, ds == "SFRES")[,2:4], unit = "a") # 0.35, p<0.01
icc(subset(j_inf, ds == "SFRES" & sys == "WEN")[,2:4], unit = "a") # 0.23, p<0.01
icc(subset(j_inf, ds == "SFRES" & sys == "LOLS")[,2:4], unit = "a") # 0.38, p<0.01
icc(subset(j_inf, ds == "BAGEL")[,2:4], unit = "a") # 0.16, p<0.01
icc(subset(j_inf, ds == "BAGEL" & sys == "Dusek")[,2:4], unit = "a") # 0.30, p<0.01
icc(subset(j_inf, ds == "BAGEL" & sys == "LOLS")[,2:4], unit = "a") # 0.16, p<0.01

# agreement on naturalness:
j1_nat <- c(j1$naturalness)
j2_nat <- c(j2$naturalness)
j3_nat <- c(j3$naturalness)
j_nat <- cbind.data.frame(ds, j1_nat, j2_nat, j3_nat, sys)
icc(j_nat[,2:4], unit = "a") # 0.42, p<0.01
icc(subset(j_nat, ds == "BAGEL")[,2:4], unit = "a") # 0.36, p<0.01
icc(subset(j_nat, ds == "BAGEL" & sys == "Dusek")[,2:4], unit = "a") # 0.46, p<0.01
icc(subset(j_nat, ds == "BAGEL" & sys == "LOLS")[,2:4], unit = "a") # 0.3, p<0.01
icc(subset(j_nat, ds == "SFHOT")[,2:4], unit = "a") # 0.47, p<0.01
icc(subset(j_nat, ds == "SFHOT" & sys == "WEN")[,2:4], unit = "a") # 0.40, p<0.01
icc(subset(j_nat, ds == "SFHOT" & sys == "LOLS")[,2:4], unit = "a") # 0.49, p<0.01
icc(subset(j_nat, ds == "SFRES")[,2:4], unit = "a") # 0.29, p<0.01
icc(subset(j_nat, ds == "SFRES" & sys == "WEN")[,2:4], unit = "a") # 0.13, p<0.01
icc(subset(j_nat, ds == "SFRES" & sys == "LOLS")[,2:4], unit = "a") # 0.29, p<0.01

# agreement on quality:
j1_qual <- c(j1$quality)
j2_qual <- c(j2$quality)
j3_qual <- c(j3$quality)
j_qual <- cbind.data.frame(ds, j1_qual, j2_qual, j3_qual,sys)
icc(j_qual[,2:4], unit = "a") # 0.46, p<0.01
icc(subset(j_qual, ds == "SFHOT")[,2:4], unit = "a") # 0.52, p<0.01
icc(subset(j_qual, ds == "SFHOT" & sys == "WEN")[,2:4], unit = "a") # 0.59, p<0.01
icc(subset(j_qual, ds == "SFHOT" & sys == "LOLS")[,2:4], unit = "a") # 0.40, p<0.01

icc(subset(j_qual, ds == "SFRES")[,2:4], unit = "a") # 0.35, p<0.01
icc(subset(j_qual, ds == "SFRES" & sys == "WEN")[,2:4], unit = "a") # 0.28, p<0.01
icc(subset(j_qual, ds == "SFRES" & sys == "LOLS")[,2:4], unit = "a") # 0.38, p<0.01

icc(subset(j_qual, ds == "BAGEL")[,2:4], unit = "a") # 0.38, p<0.01
icc(subset(j_qual, ds == "BAGEL" & sys == "Dusek")[,2:4], unit = "a") # 0.46, p<0.01
icc(subset(j_qual, ds == "BAGEL" & sys == "LOLS")[,2:4], unit = "a") # 0.31, p<0.01


##### Correlations by Dataset and System (Table 9) #####

      ######## For Bagel / TGen: ########
dus <- subset(df, system=="Dusek")
dusek.cor <- as.data.frame(cbind(cor(dus[, c(6:31)], 
                                     method = "spearman")[1:23,-c(1:23)],
                                 rcorr(as.matrix(dus[, c(6:31)]), 
                                       type = "spearman")$P[1:23,-c(1:23)]))
names(dusek.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")

      ######## For Bagel / LOLS: ########
lols.bag <- subset(df, system=="LOLS" & dataset=="BAGEL")
lols.bag.cor <- as.data.frame(cbind(cor(lols.bag[, c(6:31)], 
                                        method = "spearman")[1:23,-c(1:23)],
                                    rcorr(as.matrix(lols.bag[, c(6:31)]), 
                                          type = "spearman")$P[1:23,-c(1:23)]))
names(lols.bag.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")

      ######## For SFHotel / RNNLG: ########
wen.sfh <- subset(df,system=="WEN" & dataset=="SFHOT")
wen.sfh.cor <- as.data.frame(cbind(cor(wen.sfh[, c(6:31)], 
                                       method = "spearman")[1:23,-c(1:23)],
                                   rcorr(as.matrix(wen.sfh[, c(6:31)]), 
                                         type = "spearman")$P[1:23,-c(1:23)]))
names(wen.sfh.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")

      ######## For SFHotel / LOLS: ########
lols.sfh <- subset(df, system=="LOLS" & dataset=="SFHOT")
lol.sfh.cor <- as.data.frame(cbind(cor(lols.sfh[, c(6:31)], 
                                       method = "spearman")[1:23,-c(1:23)],
                                   rcorr(as.matrix(lols.sfh[, c(6:31)]), 
                                         type = "spearman")$P[1:23,-c(1:23)]))
names(lol.sfh.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")

      ######## For SFRest / RNNLG: ########
wen.sfr <- subset(df,system=="WEN" & dataset=="SFRES")
wen.sfr.cor <- as.data.frame(cbind(cor(wen.sfr[, c(6:31)], 
                                       method = "spearman")[1:23,-c(1:23)],
                                   rcorr(as.matrix(wen.sfr[, c(6:31)]), 
                                         type = "spearman")$P[1:23,-c(1:23)]))
names(wen.sfr.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")

      ######## For SFRest / LOLS: ########
lols.sfr <- subset(df, system=="LOLS" & dataset=="SFRES")
lol.sfr.cor <- as.data.frame(cbind(cor(lols.sfr[, c(6:31)], 
                                       method = "spearman")[1:23,-c(1:23)],
                                   rcorr(as.matrix(lols.sfr[, c(6:31)]), 
                                         type = "spearman")$P[1:23,-c(1:23)]))
names(lol.sfr.cor) <- c("inf","nat","qual","inf.p","nat.p","qual.p")




