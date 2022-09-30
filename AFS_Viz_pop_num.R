#script for figures for the presentation
#do after Pop_occupancy_3_site.R

library(tidyverse)
library(cowplot)
library(ggpubr)

#pop object comes from pop_occupancy_3_site file (which needs to be made from data_df on the cluster)
#LOAD summary df
setwd('../outputs/3_site')
data_df <- readRDS('data_df.Rds')

#LOAD pop df if needed
pop <- readRDS('pop1.Rds')

pop$year <- as.numeric(pop$year)

#add columns for sd
pop$ymin = pop$m - pop$sdev
pop$ymax = pop$m + pop$sdev

ggplot(pop, aes(x=year, y=m)) +
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .3, colour = NA) +
  labs(x = "Time (years)", y = "Population Size") +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  xlim(20,100) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 50000)) +
  theme_classic(base_size = 16)

ggsave(filename = "pop1.png", plot = last_plot(), dpi = 400, width = 10, height = 8, units = "in")

#subset option for testing
#data_df1 <- data_df %>%
#  filter(year == 10 | year == 100) %>%
#  filter(loc == "dry")
#saveRDS(data_df1, file = "data_df_test.RDS")
#LOAD data_df_test if needed
#data_df <- readRDS('data_df_test.RDS')

#convert chr to num
data_df$L0A0 <- as.numeric(data_df$L0A0)
data_df$L0A1 <- as.numeric(data_df$L0A1)
data_df$L1A0 <- as.numeric(data_df$L1A0)
data_df$L1A1 <- as.numeric(data_df$L1A1)
data_df$L2A0 <- as.numeric(data_df$L2A0)
data_df$L2A1 <- as.numeric(data_df$L2A1)
data_df$L3A0 <- as.numeric(data_df$L3A0)
data_df$L3A1 <- as.numeric(data_df$L3A1)

#going to first see what the total should be
#this through line 63 only necessary for proportions and is slightly redundant with above pop object
Population <- data_df %>%
  group_by(year, replicate) %>%
  tally()

#summary stats more useful for pop
Population1 <- Population %>%
  group_by(year, replicate) %>%
  summarize(m = mean(n), sdev = sd(n))

#going to make seperate dataframes here for each locus
#sum gives the total number of alleles in the population
##L0 and L1 are adaptive, Neutral is L4, plastic L3

data_df_L0 <- data_df %>%
  group_by(year, replicate) %>%
  summarise_at(vars(L0A0:L0A1), sum, na.rm = TRUE)

data_df_L1 <- data_df %>%
  group_by(year, replicate) %>%
  summarise_at(vars(L1A0:L1A1), sum, na.rm = TRUE)

#plastic region a bit different, need the zeroes
#do allele count
data_L2A1 <- data_df %>%
  group_by(year, replicate) %>%
  dplyr::count(L2A1, name = "L2A1_count")

#spread allele to different columns
data_L2A1_spread <- data_L2A1 %>%
  spread(L2A1, L2A1_count)

#count the next allele
data_L2A0 <- data_df %>%
  group_by(year, replicate) %>%
  dplyr::count(L2A0, name = "L2A0_count")

data_L2A0_spread <- data_L2A0 %>%
  spread(L2A0, L2A0_count)

#merge the two count/spread dataframes
data_df_L2 <- merge(x=data_L2A0_spread, y=data_L2A1_spread, by=c("year","replicate"), all = TRUE)

head(data_df_L2)

#next sum together across

data_df_L2$L2A0 <- data_df_L2$'0.x' + data_df_L2$'0.y'
#this will throw an error if all switches flipped
data_df_L2$L2A1 <- data_df_L2$'1.x' + data_df_L2$'1.y'
data_df_L2$L2A2 <- data_df_L2$'2.x' + data_df_L2$'2.y'

#remove old rows
data_df_L2$'0.x' <- NULL
data_df_L2$'0.y' <- NULL
data_df_L2$'1.x' <- NULL
data_df_L2$'1.y' <- NULL
data_df_L2$'2.x' <- NULL
data_df_L2$'2.y' <- NULL

data_df_L3 <- data_df %>%
  group_by(year, replicate) %>%
  summarise_at(vars(L3A0:L3A1), sum, na.rm = TRUE)

#bringing together for comparisons
df_all <- merge(x = data_df_L0, y = data_df_L1, by=c("year", "replicate"), all = TRUE)
df_all <- merge(x = df_all, y = data_df_L2, by=c("year", "replicate"), all = TRUE)
df_all <- merge(x= df_all, y=data_df_L3, by=c("year", "replicate"), all = TRUE)

#make sure year is numeric
df_all$year <- as.numeric(df_all$year)



#gathering the columns of alleles
df_all_plot <- gather(df_all, key = "Allele", value, L0A0:L3A1)

#going to split allele column to include locus for better graphing
df_all_plot <- separate(data = df_all_plot, col = Allele, into = c('Locus', 'Allele'), sep = 2)

#for plasticity locus only plot
df_plastic_plot <- subset(df_all_plot, df_all_plot$Locus != "L0" & df_all_plot$Locus != "L2")

head(df_plastic_plot)


#for graph
#rename loci for  plot
df_plot_final <- df_all_plot %>% 
  mutate(Locus = replace(Locus, Locus == 'L0', "Adaptive1")) %>%
  mutate(Locus = replace(Locus, Locus == 'L1', "Adaptive2")) %>%
  mutate(Locus = replace(Locus, Locus == 'L3', "Neutral")) %>%
  mutate(Locus = replace(Locus, Locus == 'L2', "Plastic"))

#make locus a factor
df_plot_final$Locus <- as.factor(df_plot_final$Locus)

df_plot_final$Locus <- factor(df_plot_final$Locus,levels(df_plot_final$Locus)[c(1,2,4,3)])


#add Population count data to do proportion
df_plot_final <- merge(x=df_plot_final, y=Population, by=c("year", "replicate"), all = TRUE)
df_plot_final$value <- df_plot_final$value / (2*df_plot_final$n)

#last thing to do is average and sd across mc
df_plot_final1 <- df_plot_final %>%
  filter(!is.na(value)) %>%
  group_by(year, Locus, Allele) %>%
  summarize(m = mean(value), sdev = sd(value))



#create min and max column for the ribbon/standard deviation
df_plot_final1$ymin = df_plot_final1$m - df_plot_final1$sdev
df_plot_final1$ymax = df_plot_final1$m + df_plot_final1$sdev

gen_plot <- ggplot(data=df_plot_final1, aes(x = year, y = m, color=as.factor(Allele), fill=as.factor(Allele), shape=as.factor(Allele))) +
  geom_point(size=4, alpha=0.55) +
  geom_line(size=1.25)+
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill=as.factor(Allele)), alpha = .3, colour = NA) +
  facet_grid(cols = vars(Locus)) +
  labs(x = "Time (years)", y = "Allele Proportion", color="Allele", fill="Allele", shape="Allele") +
  scale_color_brewer(palette = "Dark2")+
  scale_fill_brewer(palette="Dark2") +
  theme_bw(base_size=14)
gen_plot
ggsave(gen_plot, filename="gen_plot.png", dpi = 400, width = 10, height = 8, units = "in")
