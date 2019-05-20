
# "R Workshop PART 1 - CCSC11 - Cracow 2019"

# R is a programming language and software environment for 
# - statistical analysis, 
# - graphics representation,
# - reporting. 

# We have six data types (vector objects):
  
# boolean data:
vo <- TRUE 
print(class(vo))

vo <- "TRUE"
print(class(vo))

# We have some numeric types:
vo <- 8.8
print(class(vo))
vo <- 2L
print(class(vo))
vo <- 12+50i
print(class(vo))

# Containers my be vectors...
PhD <- c('horrible','looong',"searched")
print(PhD)
print(class(PhD))

# ... lists:
the_list <- list(c(1,2,3),45.6,sin(9),"coffee break")
print(the_list)


# ... two dimentional matrix:
the_M = matrix( c('y','o','u','a','r','e','n','e','o'), nrow = 3, ncol = 3, byrow = TRUE)
print(the_M)

# ... x-dimentional array
the_array <- array(c('kill','bill'),dim = c(3,3,2))
print(the_array)

#  
PhD <- c('horrible','looong',"searched")
factor_PhD <- factor(PhD)
print(factor_PhD)
print(nlevels(factor_PhD))

#  Example of a dataframe: 
PhD_register <- data.frame(name = c('Darren','Tom','Anna'), factor_PhD, 
  years = c(5, 10.5, 0), 
  grade = c(2, 5, 0))
print(PhD_register)
 
# Libraries:
library(car)
# ...

#Experiment: how the genetical modification in eye gene (exp_group 1) or control group (exp_group 0) / sex / brightness / stai / influence temporal estimation (estimated_time) of a of the picture time exposition. How the heart rate (hr) influence reaction times (RT). 

# setting the path to our working directory
setwd("C:/Users/DominikaD/Desktop/CCSC_WORKSHOP")

# reading data from csv/xlsx
data <- read.csv(file="workshop_data.csv", header = TRUE, sep = ';', dec = ",")
#data <- read.xlsx("workshop_data.csv.xlsx", sheetIndex = 1)

# ploting the first lines of the dataset (visuual check) 
head(data) 

# setting all the numeric variables
data$RT<-as.numeric(data$RT)
data$stai<-as.numeric(data$stai)
data$post_hr<-as.numeric(data$post_hr)
data$trial_num<-as.numeric(data$trial_num)
data$estimated_time<-as.numeric(data$estimated_time)

# setting our factors
data$id <- factor(data$id)
data$sex <- factor(data$sex)
data$exp_group <- factor(data$exp_group)
data$brightness <- factor(data$brightness)

# when we want to work only on the part of the dataset
data_women <- subset(data, data$sex == "K")

# when we need to z-score
data$stai_zscore <- ave(data$stai, data$id, FUN=scale)

# data aggregation procedure
library(plyr)
library(dplyr)

data_agg <- ddply(data, .(id, brightness), summarize, 
                  estimated_time.mean = mean(estimated_time, na.rm=TRUE), 
                  estimated_time.sd = sd(estimated_time, na.rm=TRUE))

# ... and seeing its result
summary(data_agg)

# ... and seeing it again on other manner
library(psych)
describe(data_agg$estimated_time.mean)

# our first plots, histogram
library(gplots)
library(ggplot2)
ggplot(data,
      aes(x=estimated_time)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", binwidth = 3) +
      geom_density(alpha=.15, colour="red", fill="grey") +
      xlab('estimated time') +
      ylab('frequency') + 
      labs(title = "histogram") +
      labs(caption = "... based on CCSC11 WORKSHOP data") +
      theme(axis.title=element_text(face="bold", size="12", color="black")) +
      theme(plot.title = element_text(face="bold", size="12", color="black")) + 
      theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))

# ... boxplot
box <- ggplot(data_agg, aes(brightness, estimated_time.mean, colour = brightness)) + 
  geom_boxplot(notch = TRUE, notchwidth = 0.2, outlier.colour = "black", outlier.fill = "black", outlier.shape = 1) + 
  coord_flip() +
  xlab('brightness') +
  ylab('estimated time') + 
  labs(title = "brightness ~ estimated time") +
  theme(axis.title=element_text(face="bold", size="12", color="black")) +
  theme(plot.title = element_text(face="bold", size="12", color="black")) + 
  theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))
plot(box)

# normality of distribution tests
ks.test(data_agg$estimated_time.mean, "pnorm")
shapiro.test(data_agg$estimated_time.mean)

# ANOVA_1 (estimated_time.mean ~ brightness)
model_1 = with(data_agg_H2, lm(estimated_time.mean ~ brightness, data = data_agg))
anova(model_1)

# non-parametric equivalent of ANOVA
kruskal.test(estimated_time.mean ~ brightness, data = data_agg) 

# multiple comparissons
library(multcomp)
library(agricolae)
mult_comp_1 <- glht(model_1,linfct=mcp(brightness="Tukey"))
confint(mult_comp_1)

# ANOVA_2 (estimated_time.mean ~ sex + brightness)
data_agg_H2 <- ddply(data, .(id,brightness, sex), summarize, 
                     estimated_time.mean = mean(estimated_time, na.rm=TRUE), 
                     estimated_time.sd = sd(estimated_time, na.rm=TRUE))
model_2 = with(data_agg_H2, lm(estimated_time.mean ~ sex + brightness))
anova(model_2)

# plots playground
ggplot(data, aes(x=trial_num, y=RT) ) + 
  geom_point(( aes(color=sex)), size=1) +
  theme(axis.title=element_text(face="bold", size="12", color="black")) +
  theme(plot.title = element_text(face="bold", size="12", color="black")) + 
  theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))

ggplot(data, aes( x = estimated_time, y = RT,  color = sex )) + 
  geom_point(size=1) +
  geom_smooth(se = FALSE, method="lm") +
  theme(axis.title=element_text(face="bold", size="12", color="black")) +
  theme(plot.title = element_text(face="bold", size="12", color="black")) + 
  theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))

data_agg_bar <- ddply(data, .(id, brightness, sex), summarize, 
                      estimated_time.mean = mean(estimated_time, na.rm=TRUE), 
                      estimated_time.sd = sd(estimated_time, na.rm=TRUE))

ggplot(data_agg_bar, aes(x = sex, y = estimated_time.mean, fill = brightness)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  theme(axis.title=element_text(face="bold", size="12", color="black")) +
  theme(plot.title = element_text(face="bold", size="12", color="black")) + 
  theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))

data_agg <- ddply(data, .(id, brightness, sex), summarize, 
                  estimated_time.mean = mean(estimated_time, na.rm=TRUE), 
                  estimated_time.sd = sd(estimated_time, na.rm=TRUE))

ggplot(data = data, aes(x = brightness, y = estimated_time)) +
  geom_line() +
  facet_wrap(~ id, ncol=3, scales = "free") +
  theme(axis.title=element_text(face="bold", size="12", color="black")) +
  theme(plot.title = element_text(face="bold", size="12", color="black")) + 
  theme(plot.margin=margin(0.5,0.5,0.5,0.5,'cm'))

# t-test (estimated_time.mean ~ exp_group)
data_agg_ttest <- ddply(data, .(id,exp_group), summarize, 
                        estimated_time.mean = mean(estimated_time, na.rm=TRUE), 
                        estimated_time.sd = sd(estimated_time, na.rm=TRUE))
t.test = lm(exp_group ~ estimated_time.mean, data=data_agg_ttest) 
summary(t.test)

# non-parametric equivalent of t-test
wilcox.test(estimated_time.mean ~ exp_group, data=data_agg_ttest) 

# regression
model_r <- lm(hr ~ RT, data=data)
summary(model_r)
model_r$coefficients

library(lm.beta)
model_r.beta <- lm.beta(model_r)
print(model_r.beta)

# tidyverse syntax 
data_agg <- data %>%
  dplyr:::group_by(id,brightness) %>%
  dplyr:::summarise(estimated_time.mean = mean(estimated_time), estimated_time.sd = sd(estimated_time))


# Once again, what to install yourself:
# 
# 1. R language: https://cloud.r-project.org/
# 
# 2. RStudio: https://www.rstudio.com/products/rstudio/download/#download
# 
# 
# ... and where to find some of the things we have done today:
# 
# 1. Useful package of packages (ggplot2, dplyr) with the documentation
#     https://www.tidyverse.org/
#     
# 2. How to perform ANOVA in R, not only the way we did it today:
#     http://homepages.inf.ed.ac.uk/bwebb/statistics/ANOVA_in_R.pdf
#     
# 3. How to deal with the regression in R and what we can infer from it:
#     http://r-statistics.co/Linear-Regression.html
# 
# 4. The idea behind aggregation/grouping by:
#     https://davetang.org/muse/2013/05/22/using-aggregate-and-apply-in-r/
# 
# 5. A little about the strange syntax of tidyverse:
#     https://style.tidyverse.org/pipes.html
