WD <- getwd()
setwd(WD)

data <- read.csv(file="final_data_ecg.csv", header = TRUE, sep = ';', dec = ",")

data$Rd<-as.numeric(data$Rd)
data$Td<-as.numeric(data$Td)
data$participant <- factor(data$participant)
data$sound <- factor(data$sound)
data$duration <- factor(data$duration)
data$K6 <- factor(data$K6)
data$A2 <- factor(data$A2)
data$V2 <- factor(data$V2)

head(data) 
summary(data) 
str(data) 
colnames(data)

# calculating AE (Mioni et al, 2014)
library(plyr)
library(dplyr)
data <- mutate(data, AE = abs(Rd-Td)/Td)
data <- mutate(data, RATIO = Rd/Td)

# aggregation SOUND/DURATION (Brown, 1997)
agg_data <- ddply(data, .(participant, sound, duration, A2), 
                  summarize, 
                  Rd.mean=mean(Rd, na.rm=TRUE), 
                  Td.mean=mean(Td,na.rm=TRUE), 
                  Rd.sd = sd(Rd, na.rm=TRUE))
agg_data <- mutate(agg_data, CV = Rd.sd/Rd.mean)


library(ggplot2)
ks.test(data$AE, "pnorm")
ggplot(data, aes(x=AE)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") 
plot1.1 <- ggplot(data, aes(sound, AE)) + geom_boxplot() + ggtitle("AE ~ SOUND")
plot1.1
plot1.2 <- ggplot(data, aes(duration, AE)) + geom_boxplot() + ggtitle("AE ~ DURATION")
plot1.2
plot1.3 <- ggplot(data, aes(A2, AE)) + geom_boxplot()+ ggtitle("AE ~ A2")
plot1.3

ks.test(data$RATIO, "pnorm")
ggplot(data, aes(x=RATIO)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") 
plot2.1 <- ggplot(data, aes(sound, RATIO)) + geom_boxplot() + ggtitle("RATIO ~ SOUND ")
plot2.1
plot2.2 <- ggplot(data, aes(duration, RATIO)) + geom_boxplot() + ggtitle("RATIO ~ DURATION")
plot2.2
plot2.3 <- ggplot(data, aes(A2, RATIO)) + geom_boxplot()+ ggtitle("RATIO ~ A2 ")
plot2.3

ks.test(agg_data$CV, "pnorm")
ggplot(agg_data, aes(x=CV)) + geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") 
plot3.1 <- ggplot(agg_data, aes(sound, CV)) + geom_boxplot() + ggtitle("CV ~ SOUND")
plot3.1
plot3.2 <- ggplot(agg_data, aes(duration, CV)) + geom_boxplot() + ggtitle("CV ~ DURATION")
plot3.2
plot3.3 <- ggplot(agg_data, aes(A2, CV)) + geom_boxplot()+ ggtitle("CV ~ A2")
plot3.3

### two-way ANOVA
AE.aov <- with(data, aov(AE ~ duration * sound * A2))
summary(AE.aov)

RATIO.aov <- with(data, aov(RATIO ~ duration * sound * A2))
summary(RATIO.aov)

CV.aov <- with(agg_data, aov(CV ~ duration * sound * A2))
summary(CV.aov)

library(sm)
library(brms)
library(loo)

### bayes dla AE

# base model: no influence of factors 
m0<- brm(AE ~ (1|participant), 
         data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                 link_ndt = "log"),
         chains = 2, 
         iter = 2000, 
         cores = 2, 
         warmup  = 500,
         save_all_pars = TRUE) 

summary(m0)
plot(m0)
pp_check(m0) 

# m1a: m0 + duration
m1a<- brm(AE ~ duration + (1|participant),
          data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                  link_ndt = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(m1a)
plot(m1a)
pp_check(m1a) 
marginal_effects(m1a, 'duration')

# m1b: m0 + sound
m1b<- brm(AE ~ sound + (1|participant),
          data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                  link_ndt = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500, 
          save_all_pars = TRUE)

summary(m1b)
plot(m1b)
pp_check(m1b) 
marginal_effects(m1b, 'sound')

###

# m1c: m0 + A2
m1c<- brm(AE ~ A2 + (1|participant),
          data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                  link_ndt = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(m1c)
plot(m1c)
pp_check(m1c) 
marginal_effects(m1c, 'A2')

# m2: m0 + duration + sound 
m2<- brm(AE ~ duration + sound + (1|participant),
          data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                  link_ndt = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(m2)
plot(m2)
pp_check(m2) 
marginal_effects(m2, "duration:sound")

# m3: m0 + duration * sound 
m3<- brm(AE ~ duration * sound + (1|participant),
         data = data, family = shifted_lognormal(link = "identity", link_sigma = "log",
                                                 link_ndt = "log"),
         chains = 2,
         iter = 2000,
         cores = 2,
         warmup = 500,
         save_all_pars = TRUE)

summary(m3)
plot(m3)
pp_check(m3) 
marginal_effects(m3, "duration:sound")

#compare models

waic_m0 <- WAIC(m0)
waic_m1a <- WAIC(m1a)
waic_m1b <- WAIC(m1b)
waic_m1c <- WAIC(m1c)
waic_m2 <- WAIC(m2)
waic_m3 <- WAIC(m3)

compare(waic_m0, waic_m1a)
compare(waic_m0, waic_m1b)
compare(waic_m0, waic_m1c)
compare(waic_m0, waic_m2)
compare(waic_m0, waic_m3)

#BF_brms_duration = bayes_factor(m1a, m0)
#BF_brms_sound = bayes_factor(m1b, m0)
#BF_brms_A2 = bayes_factor(m1c, m0)

#BF_brms_duration$bf
#BF_brms_sound$bf
#BF_brms_A2$bf

### bayes dla RATIO

# Base model: no influence of factors 
mr0<- brm(RATIO ~ (1|participant),
          data = data, family = lognormal(link = "identity", link_sigma = "log"),
          chains = 2, 
          iter = 2000, 
          cores = 2,
          warmup  = 500,
          save_all_pars = TRUE) 

summary(mr0)
plot(mr0)
pp_check(mr0) 

# mr1a: mr0 + duration
mr1a<- brm(RATIO ~ duration + (1|participant),
           data = data, family = lognormal(link = "identity", link_sigma = "log"),
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mr1a)
plot(mr1a)
pp_check(mr1a) 
marginal_effects(mr1a, 'duration')

# mr1b: mr0 + sound
mr1b<- brm(RATIO ~ sound + (1|participant),
           data = data, family = lognormal(link = "identity", link_sigma = "log"),
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mr1b)
plot(mr1b)
pp_check(mr1b) 
marginal_effects(mr1b, 'sound')

# mr1c: m0 + A2
mr1c<- brm(RATIO ~ A2 + (1|participant),
          data = data, family = lognormal(link = "identity", link_sigma = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(mr1c)
plot(mr1c)
pp_check(mr1c) 
marginal_effects(mr1c, 'A2')

# mr2: m0 + duration + sound
mr2<- brm(RATIO ~ duration + sound + (1|participant),
           data = data, family = lognormal(link = "identity", link_sigma = "log"),
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mr2)
plot(mr2)
pp_check(mr2) 
marginal_effects(mr2, "duration:sound")

# mr3: m0 + duration * sound
mr3<- brm(RATIO ~ duration * sound + (1|participant),
          data = data, family = lognormal(link = "identity", link_sigma = "log"),
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(mr3)
plot(mr3)
pp_check(mr3) 
marginal_effects(mr3, "duration:sound")


#compare models

waic_mr0 <- WAIC(mr0)
waic_mr1a <- WAIC(mr1a)
waic_mr1b <- WAIC(mr1b)
waic_mr1c <- WAIC(mr1c)
waic_mr2 <- WAIC(mr2)
waic_mr3 <- WAIC(mr3)

compare(waic_mr0, waic_mr1a)
compare(waic_mr0, waic_mr1b)
compare(waic_mr0, waic_mr1c)
compare(waic_mr0, waic_mr2)
compare(waic_mr0, waic_mr3)

#BF_brms_duration_r = bayes_factor(mr0, mr1a)
#BF_brms_sound_r = bayes_factor(mr0, mr1b)
#BF_brms_A2_r = bayes_factor(mr0, mr1c)

#BF_brms_duration_r$bf
#BF_brms_sound_r$bf
#BF_brms_A2_r$bf

### bayes dla CV

# Base model: no influence of factors 
mc0<- brm(CV ~ (1|participant),
          data = agg_data, family = gaussian,                                                                
          chains = 2, 
          iter = 2000, 
          cores = 2,
          warmup  = 500,
          save_all_pars = TRUE) 

summary(mc0)
plot(mc0)
pp_check(mc0) 

# mc1a: mc0 + duration
mc1a<- brm(CV ~ duration + (1|participant),
           data = agg_data, family = gaussian,
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mc1a)
plot(mc1a)
pp_check(mc1a) 
marginal_effects(mc1a, 'duration')

# mc1b: mc0 + sound
mc1b<- brm(CV ~ sound + (1|participant),
           data = agg_data, family = gaussian,
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mc1b)
plot(mc1b)
pp_check(mc1b) 
marginal_effects(mc1b, 'sound')

# mr1c: m0 + A2
mc1c<- brm(CV ~ A2 + (1|participant),
           data = agg_data, family = gaussian,
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
           save_all_pars = TRUE)

summary(mc1c)
plot(mc1c)
pp_check(mc1c) 
marginal_effects(mc1c, 'A2')

# mc2: m0 + duration + sound
mc2<- brm(CV ~ sound + duration + (1|participant),
           data = agg_data, family = gaussian,
           chains = 2,
           iter = 2000,
           cores = 2,
           warmup = 500,
          save_all_pars = TRUE)

summary(mc2)
plot(mc2)
pp_check(mc2) 
marginal_effects(mc2, "duration:sound")

# mc3: m0 + duration * sound
mc3<- brm(CV ~ sound * duration + (1|participant),
          data = agg_data, family = gaussian,
          chains = 2,
          iter = 2000,
          cores = 2,
          warmup = 500,
          save_all_pars = TRUE)

summary(mc3)
plot(mc3)
pp_check(mc3) 
marginal_effects(mc3, "duration:sound")

#compare models
 
waic_mc0 <- WAIC(mc0)
waic_mc1a <- WAIC(mc1a)
waic_mc1b <- WAIC(mc1b)
waic_mc1c <- WAIC(mc1c)
waic_mc2 <- WAIC(mc2)
waic_mc3 <- WAIC(mc3)

compare(waic_mc0, waic_mc1a)
compare(waic_mc0, waic_mc1b)
compare(waic_mc0, waic_mc1c)
compare(waic_mc0, waic_mc2)
compare(waic_mc0, waic_mc3)

#BF_brms_duration_c = bayes_factor(mc1a, mc0)
#BF_brms_sound_c = bayes_factor(mc1b, mc0)
#BF_brms_A2_c = bayes_factor(mc1c, mc0)
#BF_brms_duartionA2_c = bayes_factor(mc2, mc0)

#BF_brms_duration_c$bf
#BF_brms_sound_c$bf
#BF_brms_A2_c$bf
#BF_brms_duartionA2_c$bf
