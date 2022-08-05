#this script takes the temperature column and then adjusted mortality column based on the thresholds set
#values range 16-23 c

library(tidyverse)
library(data.table)

test <- read.csv("../../data/patchvars_test.csv")

#goal is to break up and bring temp column back together
test$GrowthTemperatureBack <- as.character(test$GrowthTemperatureBack)
tmp <- data.frame(strsplit(test$GrowthTemperatureBack, "\\|"))

#this has column names that are the original 3 values and the rows are each value
#need to transpose so that the three columns are the three temp values
tmp2 <- t(tmp)
rownames(tmp2) <- NULL

#add new column with desired mortality for each genotype
#convert to data frame and make numeric
tmp3 <- as.data.frame(tmp2)
tmp3[] <-lapply(tmp3,as.numeric)

#next is to set up the values for the linear model for the temperature values/mortality function

#advantage is the reduced mortality, based on a line from Big Jacks AB (from chen 2018) to CTMax
ctmax <- read.csv("../../data/CTmax_GEM3_cohort1.csv")

#calculate mean ctmax for desert
ctmax_mean <- ctmax %>%
  group_by(ecotype) %>%
  summarize(mean_ctmax = mean(Ctmax, na.rm = TRUE))

#use 10 step intervals

#genotype 

#all advantage
#AABB 20.7 to 28.75
all_adv <- data.frame(temps = c(seq(20.7, 28.75, length = 10)),
mort = seq(10, 100, length = 10),
adv = c("Full Adv"))

#3/4 advantage 20.425 to 28.6925
#AaBB
#AABb
threefour_adv <- data.frame(temps = c(seq(20.425, 28.6925, length = 10)),
mort = seq(10, 100, length = 10),
adv = c("3/4 Adv"))

#half advantage 20.15 to 28.635
#AaBb
#AAbb
#aaBB
half_adv <- data.frame(temps = c(seq(20.15, 28.635, length = 10)),
mort = seq(10, 100, length = 10),
adv = c("1/2 Adv"))

#1/4 advantage 19.875 to 28.5775
#Aabb
#aaBb
onefour_adv <- data.frame(temps = c(seq(19.875, 28.5775, length = 10)),
mort = seq(10, 100, length = 10),
adv = c("1/4 Adv"))

#no advantage
#aabb 19.6 to 28.52
no_adv <- data.frame(temps = c(seq(19.6, 28.52, length = 10)),
                     mort = seq(10, 100, length = 10),
                     adv = c("No Adv"))

#combine together for plot (not that individual pieces are easier to reference for placing in patch file)
all_mort <- rbind(no_adv, onefour_adv, half_adv, threefour_adv, all_adv)

ggplot(all_mort, aes(x = temps, y = mort, color = adv)) +
  geom_line(size = 1.5) +
  scale_color_manual(breaks=c('Full Adv', '3/4 Adv', '1/2 Adv', '1/4 Adv', 'No Adv'), values=c('black', 'firebrick4', 'firebrick3', 'firebrick2', 'firebrick1')) +
  ylab("Additional Mortality") +
  xlab("Mean August Temperature (C)")+
  labs(colour = "")+
  scale_x_continuous(limits = c(18, 30))+
  theme_classic(base_size=16)+
  theme(legend.position = c(.85,.25)) +
  annotate(geom = "text", x = 19, y = 24, label = "Arrhenius Breakpoint \n temperature") +
  annotate(geom = "text", x = 27, y = 100, label = "CTmax")


#going to do each temp independently then bring together the full one
#Big Jack
Fitness_AABB_V1 <- mutate(tmp3,
                          Fitness_AABB = case_when(
                            #threshold 1, ab, up to 10, CTmax, plug along the 10 values in sequence
                            V1 <= all_adv$temps[1] ~ paste(all_adv$mort[1],"|", sep =""),
                            V1 <= all_adv$temps[2] ~ paste(all_adv$mort[2],"|", sep =""),
                            V1 <= all_adv$temps[3] ~ paste(all_adv$mort[3],"|", sep =""),
                            V1 <= all_adv$temps[4] ~ paste(all_adv$mort[4],"|", sep =""),
                            V1 <= all_adv$temps[5] ~ paste(all_adv$mort[5],"|", sep =""),
                            V1 <= all_adv$temps[6] ~ paste(all_adv$mort[6],"|", sep =""),
                            V1 <= all_adv$temps[7] ~ paste(all_adv$mort[7],"|", sep =""),
                            V1 <= all_adv$temps[8] ~ paste(all_adv$mort[8],"|", sep =""),
                            V1 <= all_adv$temps[9] ~ paste(all_adv$mort[9],"|", sep =""),
                            V1 > all_adv$temps[10] ~ paste(all_adv$mort[10],"|", sep ="")))
Fitness_AABB_V2 <- mutate(tmp3,
                          Fitness_AABB = case_when(
                            V2 <= all_adv$temps[1] ~ paste(all_adv$mort[1],"|", sep =""),
                            V2 <= all_adv$temps[2] ~ paste(all_adv$mort[2],"|", sep =""),
                            V2 <= all_adv$temps[3] ~ paste(all_adv$mort[3],"|", sep =""),
                            V2 <= all_adv$temps[4] ~ paste(all_adv$mort[4],"|", sep =""),
                            V2 <= all_adv$temps[5] ~ paste(all_adv$mort[5],"|", sep =""),
                            V2 <= all_adv$temps[6] ~ paste(all_adv$mort[6],"|", sep =""),
                            V2 <= all_adv$temps[7] ~ paste(all_adv$mort[7],"|", sep =""),
                            V2 <= all_adv$temps[8] ~ paste(all_adv$mort[8],"|", sep =""),
                            V2 <= all_adv$temps[9] ~ paste(all_adv$mort[9],"|", sep =""),
                            V2 > all_adv$temps[10] ~ paste(all_adv$mort[10],"|", sep ="")))
Fitness_AABB_V3 <- mutate(tmp3,
                          Fitness_AABB = case_when(
                            V3 <= all_adv$temps[1] ~ paste(all_adv$mort[1], sep =""),
                            V3 <= all_adv$temps[2] ~ paste(all_adv$mort[2], sep =""),
                            V3 <= all_adv$temps[3] ~ paste(all_adv$mort[3], sep =""),
                            V3 <= all_adv$temps[4] ~ paste(all_adv$mort[4], sep =""),
                            V3 <= all_adv$temps[5] ~ paste(all_adv$mort[5], sep =""),
                            V3 <= all_adv$temps[6] ~ paste(all_adv$mort[6], sep =""),
                            V3 <= all_adv$temps[7] ~ paste(all_adv$mort[7], sep =""),
                            V3 <= all_adv$temps[8] ~ paste(all_adv$mort[8], sep =""),
                            V3 <= all_adv$temps[9] ~ paste(all_adv$mort[9], sep =""),
                            V3 > all_adv$temps[10] ~ paste(all_adv$mort[10], sep ="")))
Fitness_AABB <- paste(Fitness_AABB_V1$Fitness_AABB, Fitness_AABB_V2$Fitness_AABB, Fitness_AABB_V3$Fitness_AABB, sep="")
tmp3$Fitness_AABB <- Fitness_AABB

#next up is no advantage
Fitness_aabb_V1 <- mutate(tmp3,
                          Fitness_aabb = case_when(
                            #threshold 1, ab, up to 10, CTmax, plug along the 10 values in sequence
                            V1 <= no_adv$temps[1] ~ paste(no_adv$mort[1],"|", sep =""),
                            V1 <= no_adv$temps[2] ~ paste(no_adv$mort[2],"|", sep =""),
                            V1 <= no_adv$temps[3] ~ paste(no_adv$mort[3],"|", sep =""),
                            V1 <= no_adv$temps[4] ~ paste(no_adv$mort[4],"|", sep =""),
                            V1 <= no_adv$temps[5] ~ paste(no_adv$mort[5],"|", sep =""),
                            V1 <= no_adv$temps[6] ~ paste(no_adv$mort[6],"|", sep =""),
                            V1 <= no_adv$temps[7] ~ paste(no_adv$mort[7],"|", sep =""),
                            V1 <= no_adv$temps[8] ~ paste(no_adv$mort[8],"|", sep =""),
                            V1 <= no_adv$temps[9] ~ paste(no_adv$mort[9],"|", sep =""),
                            V1 > no_adv$temps[10] ~ paste(no_adv$mort[10],"|", sep ="")))
Fitness_aabb_V2 <- mutate(tmp3,
                          Fitness_aabb = case_when(
                            V2 <= no_adv$temps[1] ~ paste(no_adv$mort[1],"|", sep =""),
                            V2 <= no_adv$temps[2] ~ paste(no_adv$mort[2],"|", sep =""),
                            V2 <= no_adv$temps[3] ~ paste(no_adv$mort[3],"|", sep =""),
                            V2 <= no_adv$temps[4] ~ paste(no_adv$mort[4],"|", sep =""),
                            V2 <= no_adv$temps[5] ~ paste(no_adv$mort[5],"|", sep =""),
                            V2 <= no_adv$temps[6] ~ paste(no_adv$mort[6],"|", sep =""),
                            V2 <= no_adv$temps[7] ~ paste(no_adv$mort[7],"|", sep =""),
                            V2 <= no_adv$temps[8] ~ paste(no_adv$mort[8],"|", sep =""),
                            V2 <= no_adv$temps[9] ~ paste(no_adv$mort[9],"|", sep =""),
                            V2 > no_adv$temps[10] ~ paste(no_adv$mort[10],"|", sep ="")))
Fitness_aabb_V3 <- mutate(tmp3,
                          Fitness_aabb = case_when(
                            V3 <= no_adv$temps[1] ~ paste(no_adv$mort[1], sep =""),
                            V3 <= no_adv$temps[2] ~ paste(no_adv$mort[2], sep =""),
                            V3 <= no_adv$temps[3] ~ paste(no_adv$mort[3], sep =""),
                            V3 <= no_adv$temps[4] ~ paste(no_adv$mort[4], sep =""),
                            V3 <= no_adv$temps[5] ~ paste(no_adv$mort[5], sep =""),
                            V3 <= no_adv$temps[6] ~ paste(no_adv$mort[6], sep =""),
                            V3 <= no_adv$temps[7] ~ paste(no_adv$mort[7], sep =""),
                            V3 <= no_adv$temps[8] ~ paste(no_adv$mort[8], sep =""),
                            V3 <= no_adv$temps[9] ~ paste(no_adv$mort[9], sep =""),
                            V3 > no_adv$temps[10] ~ paste(no_adv$mort[10], sep ="")))
Fitness_aabb <- paste(Fitness_aabb_V1$Fitness_aabb, Fitness_aabb_V2$Fitness_aabb, Fitness_aabb_V3$Fitness_aabb, sep="")
tmp3$Fitness_aabb <- Fitness_aabb

#for the other three, we will be doing the same as above for one genotype, but then adding the column in multiple times under the different genotypes (to keep track of things) before then moving that column into test/the patch file

#threefourth
Fitness_AABb_V1 <- mutate(tmp3,
                          Fitness_AABb = case_when(
                            #threshold 1, ab, up to 10, CTmax, plug along the 10 values in sequence
                            V1 <= threefour_adv$temps[1] ~ paste(threefour_adv$mort[1],"|", sep =""),
                            V1 <= threefour_adv$temps[2] ~ paste(threefour_adv$mort[2],"|", sep =""),
                            V1 <= threefour_adv$temps[3] ~ paste(threefour_adv$mort[3],"|", sep =""),
                            V1 <= threefour_adv$temps[4] ~ paste(threefour_adv$mort[4],"|", sep =""),
                            V1 <= threefour_adv$temps[5] ~ paste(threefour_adv$mort[5],"|", sep =""),
                            V1 <= threefour_adv$temps[6] ~ paste(threefour_adv$mort[6],"|", sep =""),
                            V1 <= threefour_adv$temps[7] ~ paste(threefour_adv$mort[7],"|", sep =""),
                            V1 <= threefour_adv$temps[8] ~ paste(threefour_adv$mort[8],"|", sep =""),
                            V1 <= threefour_adv$temps[9] ~ paste(threefour_adv$mort[9],"|", sep =""),
                            V1 > threefour_adv$temps[10] ~ paste(threefour_adv$mort[10],"|", sep ="")))
Fitness_AABb_V2 <- mutate(tmp3,
                          Fitness_AABb = case_when(
                            V2 <= threefour_adv$temps[1] ~ paste(threefour_adv$mort[1],"|", sep =""),
                            V2 <= threefour_adv$temps[2] ~ paste(threefour_adv$mort[2],"|", sep =""),
                            V2 <= threefour_adv$temps[3] ~ paste(threefour_adv$mort[3],"|", sep =""),
                            V2 <= threefour_adv$temps[4] ~ paste(threefour_adv$mort[4],"|", sep =""),
                            V2 <= threefour_adv$temps[5] ~ paste(threefour_adv$mort[5],"|", sep =""),
                            V2 <= threefour_adv$temps[6] ~ paste(threefour_adv$mort[6],"|", sep =""),
                            V2 <= threefour_adv$temps[7] ~ paste(threefour_adv$mort[7],"|", sep =""),
                            V2 <= threefour_adv$temps[8] ~ paste(threefour_adv$mort[8],"|", sep =""),
                            V2 <= threefour_adv$temps[9] ~ paste(threefour_adv$mort[9],"|", sep =""),
                            V2 > threefour_adv$temps[10] ~ paste(threefour_adv$mort[10],"|", sep ="")))
Fitness_AABb_V3 <- mutate(tmp3,
                          Fitness_AABb = case_when(
                            V3 <= threefour_adv$temps[1] ~ paste(threefour_adv$mort[1], sep =""),
                            V3 <= threefour_adv$temps[2] ~ paste(threefour_adv$mort[2], sep =""),
                            V3 <= threefour_adv$temps[3] ~ paste(threefour_adv$mort[3], sep =""),
                            V3 <= threefour_adv$temps[4] ~ paste(threefour_adv$mort[4], sep =""),
                            V3 <= threefour_adv$temps[5] ~ paste(threefour_adv$mort[5], sep =""),
                            V3 <= threefour_adv$temps[6] ~ paste(threefour_adv$mort[6], sep =""),
                            V3 <= threefour_adv$temps[7] ~ paste(threefour_adv$mort[7], sep =""),
                            V3 <= threefour_adv$temps[8] ~ paste(threefour_adv$mort[8], sep =""),
                            V3 <= threefour_adv$temps[9] ~ paste(threefour_adv$mort[9], sep =""),
                            V3 > threefour_adv$temps[10] ~ paste(threefour_adv$mort[10], sep ="")))
Fitness_AABb <- paste(Fitness_AABb_V1$Fitness_AABb, Fitness_AABb_V2$Fitness_AABb, Fitness_AABb_V3$Fitness_AABb, sep="")
tmp3$Fitness_AABb <- Fitness_AABb
tmp3$Fitness_AaBB <- Fitness_AABb

#onehalf
Fitness_AaBb_V1 <- mutate(tmp3,
                          Fitness_AaBb = case_when(
                            #threshold 1, ab, up to 10, CTmax, plug along the 10 values in sequence
                            V1 <= half_adv$temps[1] ~ paste(half_adv$mort[1],"|", sep =""),
                            V1 <= half_adv$temps[2] ~ paste(half_adv$mort[2],"|", sep =""),
                            V1 <= half_adv$temps[3] ~ paste(half_adv$mort[3],"|", sep =""),
                            V1 <= half_adv$temps[4] ~ paste(half_adv$mort[4],"|", sep =""),
                            V1 <= half_adv$temps[5] ~ paste(half_adv$mort[5],"|", sep =""),
                            V1 <= half_adv$temps[6] ~ paste(half_adv$mort[6],"|", sep =""),
                            V1 <= half_adv$temps[7] ~ paste(half_adv$mort[7],"|", sep =""),
                            V1 <= half_adv$temps[8] ~ paste(half_adv$mort[8],"|", sep =""),
                            V1 <= half_adv$temps[9] ~ paste(half_adv$mort[9],"|", sep =""),
                            V1 > half_adv$temps[10] ~ paste(half_adv$mort[10],"|", sep ="")))
Fitness_AaBb_V2 <- mutate(tmp3,
                          Fitness_AaBb = case_when(
                            V2 <= half_adv$temps[1] ~ paste(half_adv$mort[1],"|", sep =""),
                            V2 <= half_adv$temps[2] ~ paste(half_adv$mort[2],"|", sep =""),
                            V2 <= half_adv$temps[3] ~ paste(half_adv$mort[3],"|", sep =""),
                            V2 <= half_adv$temps[4] ~ paste(half_adv$mort[4],"|", sep =""),
                            V2 <= half_adv$temps[5] ~ paste(half_adv$mort[5],"|", sep =""),
                            V2 <= half_adv$temps[6] ~ paste(half_adv$mort[6],"|", sep =""),
                            V2 <= half_adv$temps[7] ~ paste(half_adv$mort[7],"|", sep =""),
                            V2 <= half_adv$temps[8] ~ paste(half_adv$mort[8],"|", sep =""),
                            V2 <= half_adv$temps[9] ~ paste(half_adv$mort[9],"|", sep =""),
                            V2 > half_adv$temps[10] ~ paste(half_adv$mort[10],"|", sep ="")))
Fitness_AaBb_V3 <- mutate(tmp3,
                          Fitness_AaBb = case_when(
                            V3 <= half_adv$temps[1] ~ paste(half_adv$mort[1], sep =""),
                            V3 <= half_adv$temps[2] ~ paste(half_adv$mort[2], sep =""),
                            V3 <= half_adv$temps[3] ~ paste(half_adv$mort[3], sep =""),
                            V3 <= half_adv$temps[4] ~ paste(half_adv$mort[4], sep =""),
                            V3 <= half_adv$temps[5] ~ paste(half_adv$mort[5], sep =""),
                            V3 <= half_adv$temps[6] ~ paste(half_adv$mort[6], sep =""),
                            V3 <= half_adv$temps[7] ~ paste(half_adv$mort[7], sep =""),
                            V3 <= half_adv$temps[8] ~ paste(half_adv$mort[8], sep =""),
                            V3 <= half_adv$temps[9] ~ paste(half_adv$mort[9], sep =""),
                            V3 > half_adv$temps[10] ~ paste(half_adv$mort[10], sep ="")))
Fitness_AaBb <- paste(Fitness_AaBb_V1$Fitness_AaBb, Fitness_AaBb_V2$Fitness_AaBb, Fitness_AaBb_V3$Fitness_AaBb, sep="")
tmp3$Fitness_AaBb <- Fitness_AaBb
tmp3$Fitness_AAbb <- Fitness_AaBb
tmp3$Fitness_aaBB <- Fitness_AaBb

#onefourth
Fitness_Aabb_V1 <- mutate(tmp3,
                          Fitness_Aabb = case_when(
                            #threshold 1, ab, up to 10, CTmax, plug along the 10 values in sequence
                            V1 <= onefour_adv$temps[1] ~ paste(onefour_adv$mort[1],"|", sep =""),
                            V1 <= onefour_adv$temps[2] ~ paste(onefour_adv$mort[2],"|", sep =""),
                            V1 <= onefour_adv$temps[3] ~ paste(onefour_adv$mort[3],"|", sep =""),
                            V1 <= onefour_adv$temps[4] ~ paste(onefour_adv$mort[4],"|", sep =""),
                            V1 <= onefour_adv$temps[5] ~ paste(onefour_adv$mort[5],"|", sep =""),
                            V1 <= onefour_adv$temps[6] ~ paste(onefour_adv$mort[6],"|", sep =""),
                            V1 <= onefour_adv$temps[7] ~ paste(onefour_adv$mort[7],"|", sep =""),
                            V1 <= onefour_adv$temps[8] ~ paste(onefour_adv$mort[8],"|", sep =""),
                            V1 <= onefour_adv$temps[9] ~ paste(onefour_adv$mort[9],"|", sep =""),
                            V1 > onefour_adv$temps[10] ~ paste(onefour_adv$mort[10],"|", sep ="")))
Fitness_Aabb_V2 <- mutate(tmp3,
                          Fitness_Aabb = case_when(
                            V2 <= onefour_adv$temps[1] ~ paste(onefour_adv$mort[1],"|", sep =""),
                            V2 <= onefour_adv$temps[2] ~ paste(onefour_adv$mort[2],"|", sep =""),
                            V2 <= onefour_adv$temps[3] ~ paste(onefour_adv$mort[3],"|", sep =""),
                            V2 <= onefour_adv$temps[4] ~ paste(onefour_adv$mort[4],"|", sep =""),
                            V2 <= onefour_adv$temps[5] ~ paste(onefour_adv$mort[5],"|", sep =""),
                            V2 <= onefour_adv$temps[6] ~ paste(onefour_adv$mort[6],"|", sep =""),
                            V2 <= onefour_adv$temps[7] ~ paste(onefour_adv$mort[7],"|", sep =""),
                            V2 <= onefour_adv$temps[8] ~ paste(onefour_adv$mort[8],"|", sep =""),
                            V2 <= onefour_adv$temps[9] ~ paste(onefour_adv$mort[9],"|", sep =""),
                            V2 > onefour_adv$temps[10] ~ paste(onefour_adv$mort[10],"|", sep ="")))
Fitness_Aabb_V3 <- mutate(tmp3,
                          Fitness_Aabb = case_when(
                            V3 <= onefour_adv$temps[1] ~ paste(onefour_adv$mort[1], sep =""),
                            V3 <= onefour_adv$temps[2] ~ paste(onefour_adv$mort[2], sep =""),
                            V3 <= onefour_adv$temps[3] ~ paste(onefour_adv$mort[3], sep =""),
                            V3 <= onefour_adv$temps[4] ~ paste(onefour_adv$mort[4], sep =""),
                            V3 <= onefour_adv$temps[5] ~ paste(onefour_adv$mort[5], sep =""),
                            V3 <= onefour_adv$temps[6] ~ paste(onefour_adv$mort[6], sep =""),
                            V3 <= onefour_adv$temps[7] ~ paste(onefour_adv$mort[7], sep =""),
                            V3 <= onefour_adv$temps[8] ~ paste(onefour_adv$mort[8], sep =""),
                            V3 <= onefour_adv$temps[9] ~ paste(onefour_adv$mort[9], sep =""),
                            V3 > onefour_adv$temps[10] ~ paste(onefour_adv$mort[10], sep ="")))
Fitness_Aabb <- paste(Fitness_Aabb_V1$Fitness_Aabb, Fitness_Aabb_V2$Fitness_Aabb, Fitness_Aabb_V3$Fitness_Aabb, sep="")
tmp3$Fitness_aaBb <- Fitness_Aabb
tmp3$Fitness_Aabb <- Fitness_Aabb

#go into test and replace the old values to 1
test$Fitness_AA <- c(1)
test$Fitness_Aa <- c(1)
test$Fitness_aa <- c(1)

#add in the new values to the original patch file
test$Fitness_AABB <- tmp3$Fitness_AABB
test$Fitness_AaBB <- tmp3$Fitness_AaBB
test$Fitness_aaBB <- tmp3$Fitness_aaBB
test$Fitness_AABb <- tmp3$Fitness_AABb
test$Fitness_AaBb <- tmp3$Fitness_AaBb
test$Fitness_aaBb <- tmp3$Fitness_aaBb
test$Fitness_AAbb <- tmp3$Fitness_AAbb
test$Fitness_Aabb <- tmp3$Fitness_Aabb
test$Fitness_aabb <- tmp3$Fitness_aabb
