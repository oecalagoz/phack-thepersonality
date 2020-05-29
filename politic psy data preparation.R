rm(list=ls())
library(haven)
library(sjlabelled)
library(robustHD)

data <- read_sav("Meek_PolPsy_UKDS_SPSS.sav")

data <- remove_all_labels(data)

data <- as_numeric(data)

data<- data [order(-data$sum_per, na.last=NA), ]

data <- na.omit(data)

#Some personality items are reverse coded. To make high scores indicate the favorable attribute, I recoded them.

data$O1 <- 6-data$O1
data$O4 <- 6-data$O4

data$C2 <- 6-data$C2
data$C4 <- 6-data$C4

data$E2 <- 6-data$E2
data$E4 <- 6-data$E4

data$A3 <- 6-data$A3
data$A4 <- 6-data$A4

data$N1 <- 6-data$N1
data$N3 <- 6-data$N3

data$Openness <- data$O1 + data$O2 + data$O3 + data$O4

data$Conscientiousness <- data$C1 + data$C2 + data$C3 + data$C4

data$Extraversion <- data$E1 + data$E2 + data$E3 + data$E4                        

data$Agreeableness <- data$A1 + data$A2 + data$A3 + data$A4

data$Neuroticism <- data$N1 + data$N2 + data$N3 + data$N4


data$sum_per <- data$Neuroticism + data$Agreeableness + data$Extraversion + data$Conscientiousness + data$Openness

####################

#data$Left_Right

#Low scores LEFT
#High scores RIGHT

# the first four items should be reversed to make low scores indicate RIGHT

data$Redistrb <- 6-data$Redistrb
data$BigBusnN <- 6-data$BigBusnN
data$Wealth <- 6-data$Wealth
data$RichLaw <- 6-data$RichLaw

data$Left_Right<- data$Redistrb + data$BigBusnN + data$Wealth + data$RichLaw

####################

#data$Libertarianism

#Low scores AUTHORITARIAN =RIGHT
#High scores LIBERTARIAN = LEFT

#no reverse item

####################

#data$Welfarism

#Low Scores RIGHT
#High scores LEFT

#2nd, 7th and 8th items should be reversed 

data$MoreWelf <- 6-data$MoreWelf
data$DamLives <- 6-data$DamLives
data$ProudWlf <- 6-data$ProudWlf


data$Welfarism <- data$WelfHelp + data$MoreWelf + data$UnempJob + data$SocHelp + data$DoleFidl + data$WelfFeet + data$DamLives + data$ProudWlf

####################

#data$Immigration

#Low scores RIGHT
#HIGH scores LEFT

#2nd and 4th items should be reversed

data$Immigrt2 <- 6-data$Immigrt2
data$Immigrt4 <- 6-data$Immigrt4

data$Immigration<- data$Immigrt1 + data$Immigrt2 + data$Immigrt3 + data$Immigrt4 + data$Immigrt6

###################

#data$Religiosity

#LOW scores Religious
#HIGH scores not

# no need to reverse

###################

#data$Theocratism

#Low scores not
#High scores theocrat

#reverse to make low scores indicate more theocracy

data$Theocratism <- 6-data$Theocratism


## Variable'ları toplayıp yeni variable haline getirerek analiz yapılabilir

## Variable'lar standardize edilip öyle toplanabilir. Bu sayede 0'a yakın değerler her değişken açısından ortalama insanı temsil ederken, toplamı yüksek ya da alçak olanlar da ortalama insandan farklı görüşleri trmsil ediyor olacak


data$left<-data$Left_Right
data$welfare<-data$Welfarism
data$immigrant<-data$Immigration
data$libertarianism<-data$Libertarianism
data$religiousity<-data$Religiosity
data$theocratism<-data$Theocratism
