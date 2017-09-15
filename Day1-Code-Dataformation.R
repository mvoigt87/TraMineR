### ------------------------------- ###
### TraMineR - Longpop summerschool ###    
### ------------------------------- ###

# https://github.com/mvoigt87/TraMineR

# packages #
# -------- #

library("TraMineR")
library("tidyverse")
library(reshape)

load("060_ss_cc2002.RData")

### 1. join the pension to the census individuals (using innerjoin = there are some individuals in the
###    social security data which were not in the 2001 census)
### 1.1. split the fnac variable

retire <- inner_join(ss.benefits,ss.benefits.c2002,by="kID") %>% 
  # split the fnac variable in year, month, day
  separate(FNAC, c("byear","bmonth","bday"),sep=c(4,6)) %>% 
  mutate(byear=as.numeric(byear)) %>% 
  mutate(bmonth=as.numeric(bmonth)) %>% 
  mutate(bday=as.numeric(bday)) %>%
  
  # split the start.date (entry to retirement) variable in year, month, day
  separate(start.date, c("syear","smonth","sday"),sep=c(4,6)) %>% 
  mutate(syear=as.numeric(syear)) %>% 
  mutate(smonth=as.numeric(smonth)) %>% 
  mutate(sday=as.numeric(sday)) %>%
  
  # create an age variable from the birth year
  mutate(entry.year = 2002) %>% 
  mutate(age = entry.year - byear) %>% 
  # extract the age groups 55-70 & Beginning state in 2001
  filter(age>55 & age<=70 & syear>=2002)
  
 # 1.2. order by kID (because yu have an unordered long dataset and subset)

 retire <- retire[order(retire$kID) , ]
  
 # simple way = subset
 rs1 <- retire[1:2500,]
 
 # clean from cases with two retirement entries (like 104)
 # see cases with two identical spells (2 times retirement) = remember "dublicated" function)
 rs.test <- rs1[duplicated(rs1[1:2]) | duplicated(rs1[1:2], fromLast=TRUE),] 
 # double pension of any kind = 46 cases
 # listwise deletion
 
 # now extract them!
 rs1 <-  rs1[!duplicated(rs1[1:2]),]
 
### delete the big data sets
# rm(parned.c2002, ss.benefits.c2002, ss.benefits)

### 2. Prepare the TraMineR sts data set
 
##  2.1. Define the entry dates
 
# working or not working (data from 2002)
 
 rs1$Work <- 1
 rs1$Work[rs1$SITP==9] <- 0 # define the once who are working
 # and overwrite the disabled cases
 rs1$Work[rs1$INVAL==1] <- 2

 # listwise deletion of cases with no disability information
 table(rs1$INVAL)
 rs1 <- rs1 %>% filter(INVAL < 9)
   
 
 # create 15 year variable
 rs1 <- rs1 %>% mutate(y1 = 2002) %>% 
   mutate(y2 = 2003) %>% 
   mutate(y3 = 2004) %>% 
   mutate(y4 = 2005) %>% 
   mutate(y5 = 2006) %>% 
   mutate(y6 = 2007) %>% 
   mutate(y7 = 2008) %>% 
   mutate(y8 = 2009) %>% 
   mutate(y9 = 2010) %>% 
   mutate(y10 = 2011) %>% 
   mutate(y11 = 2012) %>% 
   mutate(y12 = 2013) %>% 
   mutate(y13 = 2014) %>% 
   mutate(y14 = 2015) %>% 
 
 # create 15 state variable - original missing values
 mutate(s1 = Work)
 
 
   
 
 
 
   
   
   # create a new factor variable for sex
   ls$sex1 <- ls$sex
 ls$sex <- factor(ls$sex,labels=c('male','female'))   ### WHY factors? => they are hard to deal with = relevel for cox
 relevel (ls$sex,'female') ->  ls$sex
 sex.tb <- table(ls$sex)
 round(100*sex.tb/sum(sex.tb), digits=2) 
 
 
 ## Education variable ##
 #----------------------#
 # => add esreal (education variable) ##
 ed.tb <- table(ls$esreal)
 ed.tb # categories from 0-10 - 0 is a missing value (3953), 1 - illiterate ...
 ## clean the zeros
 ls$esreal[ls$esreal==0] <- NA
 ls <- na.omit(ls)
 # give new labels - make factors out of it  
 ls$nedu <- NA
 ls$nedu[ls$esreal==1]<-"illiterate"
 ls$nedu[ls$esreal==2]<-"incomplete"
 ls$nedu[ls$esreal==3]<-"elementary"
 ls$nedu[ls$esreal>=4 & ls$esreal<=6]<-"secondary"
 ls$nedu[ls$esreal>6]<-"tertiary"
 # make factor out of it
 ls$nedu <- as.factor(ls$nedu)
 
 
 ## Civil Status ##
 #----------------#
 
 ### Marital/civil status 10 years ago (in census) !!! ###
 # --- There are 3953 NAs
 civ.tb <- table(ls$ecivil)
 civ.tb  # no missings so far (0=NAs)    ## separate and divorced are two similar categories with relatively
 ## small case numbers
 
 ls$ecivil.new [ls$ecivil==1] <- "single"
 ls$ecivil.new [ls$ecivil==2] <- "married"
 ls$ecivil.new [ls$ecivil==3] <- "widowed"
 ls$ecivil.new [ls$ecivil>=4] <- "separated/divorced"
 ls$ecivil.new <- as.factor(ls$ecivil.new)
 
 relevel (ls$ecivil.new, "married") ->  ls$ecivil.new   # relevel to make "married" the reference
 summary(ls$ecivil.new)
