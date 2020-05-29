#remove# 
rm(list=ls())

# SOME INFO ###########################
## 26 variables measuring nerdiness Q1-Q26 ##

## 10 TIPI items measuring Big5 ##

## gender, Sexual orientation variable, marriage status variable, age variable, ethnicity variable (multiply selectible)
## autism SD variable ~1600 persons diagnosed
## education level variable, where has life been spent most: rural-urban-suburban

### Research Question: "Does gender explain nerdiness level of people?" 

### In 538, people have to decide in which political group do they want to p-hack the results: Democrats or Republicians
### In this dataset, people have to decide in which gender do they want to p-hack the results: Males or Females

### The p-hacking behaviour that I will implement is: 


##"Choosing to include different measured variables as covariates, independent variables, mediators, or moderators"
# By selecting different sets of subgroups, such as marriage status, sexual orientation, and where have they lived most;
# findings may change remarkably

##"Using alternative inclusion and exclusion criteria got selecting participants in analyses"
# People with higher education level (or vice versa), only whites or only any other ethinicity etc. 

##"Trying out different ways to score the chosen primary dependent variable"
# Using combinations of TIPI variables to assess nerdiness, or weighing nerdiness sum score with these personality traits



# data###############################################
npas <- read.delim("B:/Methodology and Statistics/Dersler/Traineeship/NPAS data/data.csv", quote="")


# EXCLUDE PEOPLE WHO KNOW NON-EXISTING WORDS ####################

npas$VCL6[npas$VCL6==1]= NA

npas$VCL9[npas$VCL9==1]= NA

npas$VCL12[npas$VCL12==1]= NA

summary(npas$VCL6)
#2651 NA
summary(npas$VCL9)
#1763 NA
summary(npas$VCL12)
#5431 NA


#since there is no NAs in the data except these, I used complete cases func. to exclude them

data<-npas[complete.cases(npas),]

# nmbr of cases deleted == 7258 

# RECODING MISSING VALUES INTO THE SAME VARIABLES AS ""NA""########

#missing data are coded as 0 in all variables except race variables.
#So, simply recoding 0s to NAs would be OK. then it is easier to recode those NAs on race variables into 0, again.



data[data==0]<-NA

data[,70:77][is.na(data[,70:77])]=0




# CREATING VARIABLES FOR ANALYSES ##################################

#Total score of the NPAS main variables measuring nerdy personality attributes

data$sumnerd<-apply(data[,1:26], 1, sum)

#Creating gender variable again without "OTHER" option

names(data)[59]<-"genderall"

data$gender<-data$genderall

data$gender[data$gender==0 | data$genderall==3]=NA

#make it dummy->

data$gender<-data$gender-1

#0=male 1=female

summary(data$gender)

# recoding orientation variable into separate dummy variables, for homosexuals and heterosexuals (==3 and ==1)

# for homosexuals

data$homosexuals<-data$orientation
data$homosexuals[data$homosexuals== 1| data$homosexuals== 2 | data$homosexuals==4 | data$homosexuals==5]=0

data$homosexuals[data$homosexuals==3]=1
summary(data$homosexuals)


#for heterosexuals

data$heterosexuals<-data$orientation
data$heterosexuals[data$heterosexuals== 3| data$heterosexuals== 2 | data$heterosexuals==4 | data$heterosexuals==5]=0

summary(data$heterosexuals)

### #recoding marriage status to separate into dummy variables

names(data)[68]<- "marriagestatus"

#divorced

data$divorced<-data$marriagestatus

data$divorced[data$divorced== 1| data$divorced== 2]=0

data$divorced[data$divorced==3]=1

summary(data$divorced)


#currently married

data$married<-data$marriagestatus

data$married[data$married== 1| data$married== 3]=0

data$married[data$married==2]=1

summary(data$married)



#never married

data$singles<-data$marriagestatus

data$singles[data$singles== 2| data$singles== 3]=0

data$singles[data$singles==1]=1

summary(data$singles)



###recoding living areas into separate dummys

names(data)[58]<-"livingarea"

#rural

data$rural<-data$livingarea

data$rural[data$rural== 2| data$rural== 3]=0

data$rural[data$rural==1]=1

summary(data$rural)



#suburban

data$suburban<-data$livingarea

data$suburban[data$suburban== 1| data$suburban== 3]=0

data$suburban[data$suburban==2]=1

summary(data$suburban)



#urban

data$urban<-data$livingarea

data$urban[data$urban==1 | data$urban==2]=0

data$urban[data$urban==3]=1

summary(data$urban)






# ANALYSES ###################


#GENDER DIFFERENCE IN NERDINESS WITHOUT ANY EXTERNAL VARIABLE############

genderdifference<-lm(data$sumnerd~data$gender)


#ORIENTATION-SPECIFIC GENDER-NERDINESS RELATIONSHIPS #############

#heterosexual males vs females

heterosexual_fe_male<-data$heterosexuals*data$gender

heterosexuals_gender_difference<-lm(data$sumnerd~heterosexual_fe_male)

#homosexual males vs homosexual females

#variable including only homosexuals
homosexual_fe_male<-data$homosexuals*data$gender

homosexuals_gender_difference<-lm(data$sumnerd~homosexual_fe_male)


# MARRIAGE-SPECIFIC GENDER DIFFERENCES ####################################

marriagestatus_expain_nerd<-lm(data$sumnerd ~data$marriagestatus)
#it doesn't mean much thing, marriage status is a multinomial variable with 3 cat. ANOVA needed. 

# SINGLE PEOPLE# GENDER DIFFERENCES IN NERDINESS ################

single_fe_male<-data$singles*data$gender
singles_gender_difference<-lm(data$sumnerd~single_fe_male)

# MARRIED PEOPLE# GENDER DIFFERENCES IN NERDINESS ############### 

married_fe_male<-data$married*data$gender
married_gender_difference<-lm(data$sumnerd ~married_fe_male)

# DIVORCED PEOPLE# GENDER DIFFERENCES IN NERDINESS #################

divorced_fe_male<-data$divorced*data$gender
divorced_gender_difference<-lm(data$sumnerd ~divorced_fe_male)

# MARRIAGE-ORIENTATION VARIABLE CREATING###################################################

# SELECTING BOTH OPTIONS 

#GENDER DIFFERENCES IN SINGLE HOMOSEXUALS AND SINGLE HETEROSEXUALS

single_homosexual_fe_male<- single_fe_male*data$homosexuals

single_heterosexual_fe_male<- single_fe_male*data$heterosexuals

#GENDER DIFFERENCES IN MARRIED HOMOSEXUALS AND MARRIED HETEROSEXUALS


married_homosexual_fe_male<- married_fe_male*data$homosexuals

married_heterosexual_fe_male<- married_fe_male*data$heterosexuals

# GENDER DIFFERENCES IN DIVORCED HOMOSEXUALS AND DIVORCED HETEROSEXUALS ################


divorced_homosexual_fe_male<- divorced_fe_male*data$homosexuals

divorced_heterosexual_fe_male<- divorced_fe_male*data$heterosexuals

single_homosexuals_gender_difference<-lm(data$sumnerd ~ single_homosexual_fe_male)

single_heterosexuals_gender_difference<-lm(data$sumnerd ~ single_heterosexual_fe_male)

married_homosexuals_gender_difference<-lm(data$sumnerd ~ married_homosexual_fe_male)

married_heterosexuals_gender_difference<-lm(data$sumnerd ~ married_heterosexual_fe_male)

divorced_homosexuals_gender_difference<-lm(data$sumnerd ~ divorced_homosexual_fe_male)

divorced_heterosexuals_gender_difference<-lm(data$sumnerd ~ divorced_heterosexual_fe_male)


# RESEARCH QUESTION # #-# Is there any gender difference in nerd personality attributes? #-#

# RESULTS ##########################################
# WITHOUT CONTROLLING FOR SOME VARIABLES #########

summary(genderdifference)


# Females are .22 point more nerd than males, however it is not significant p=.353. 

# What to do? Maybe I can find this difference between heterosexual males and females?


# ADDING ORIENTATION ###############

summary(heterosexuals_gender_difference)

# Here it is! I knew it! Heterosexual males are 4.2 point more nerd than heterosexual females and it is significant p<.001

#But what about homosexual people?

summary(homosexuals_gender_difference)

# Interesting. Homosexual males are 2.24 point less nerd than homosexual females. It is the opposite.

# Well, I was expecting to find an opposite result. I would like to see heterosexual males are less nerd than females.
# Let's try adding the marriage status than.

# ADDING MARRIAGE STATUS ###############

summary(married_gender_difference)

# That is no go. There is no significant difference between married males and married females.
# Let's see divorced people.

summary(divorced_gender_difference)

# OK. That is not bad. At least I turned non-significant gender differences into significant one. 
# Divorced males are 1.70 points more nerd than divorced females, p=.04. Fiyuh, almost!
# I am calling my final card now. Singles:

summary(singles_gender_difference)

# Gotcha! Single males are less nerd than single females. But the difference is only .42 points and it is sig. <.05.
# So, it is not so astonishing. Maybe I can increase the difference a little bit more by including sex. orientation.

# ADDING: SEXUAL ORIENTATION + MARRIAGE STATUS #######################
# let's see whether the difference between single males and females changes when only examine heterosexuals

summary(single_heterosexuals_gender_difference)

# We found that heterosexual single males are 3.96 points more nerd than heterosexual single females, p<.001.

# let's see homosexual single males and females

summary(single_homosexuals_gender_difference)

# Yes! I increased the mean difference between single males and females by examining only homosexuals.
# I can do HARKing now. Let's find some literature about gender differences, sex. orientation, and marriage, and ...
# ... you can get published!

# Remember there were no sig. difference between married males and females. Let's make it significant.

summary(married_heterosexuals_gender_difference)

# Here it is. Married heterosexual males are 2.50 points more nerd than females. I knew there was a sig. difference. 
# A good researcher should know how to dig a dataset to find hidden "truth".
# Want the difference even bigger? take a look at it:

summary(married_homosexuals_gender_difference)

# Married homosexual males are 10 points more nerd than females although p value is not impressive.

# Last but not least, there was a barely significant difference between divorced males' and females' nerd prsnality score.

summary(divorced_homosexuals_gender_difference)

# Although divorced homosexual males are 1.724 points more nerd than females, it is not significant p=.76.
# Let's try our chance with divorced heterosexuals:

summary(divorced_heterosexuals_gender_difference)

# It is still the males who are more nerd. Divorced heterosexual males are 3.70 points more nerd than divorced heterosexual females.


#### ADDITIONAL DVs#############################################

### DEFINITION OF NERD ..FROM WIKIPEDIA..

#A nerd is a person seen as overly intellectual, obsessive, introverted or lacking social skills. 
#Such a person may spend inordinate amounts of time on unpopular, little known, or non-mainstream activities ...
#... which are generally either highly technical, abstract, or relating to topics of science fiction or fantasy,...
#... to the exclusion of more mainstream activities.

#With a combination of TIPI variables, we can create an "alternative" dependent variable for nerd personality measurement
#These variables could be 
#------------------------------------------
# TIPI1	Extraverted, enthusiastic. (R)    |<-<-
#                                         |<-<- Extraversion ---will be used reversed, as it measures introversion
# TIPI6	Reserved, quiet.                  |<-<-
#------------------------------------------
# TIPI10	Conventional, uncreative. (R)   |<-<-
#                                         |<-<- Openness to Experiences ---will be used as it is
# TIPI5	open to new experiences, complex  |<-<-
#------------------------------------------

#reverse-coding TIPI6 and summing the EXTRAVERSION factor

data$r_TIPI6<-sqrt((data$TIPI6-8)^2)
data$introversion<-data$r_TIPI6+data$TIPI1             

summary(data$introversion)


#reverse-coding TIPI10 and summing the OPENNESS factor

data$r_TIPI10<-sqrt((data$TIPI10-8)^2)
data$openness<- data$r_TIPI10+data$TIPI5

summary(data$openness)

######### COVARIATE ###############3


