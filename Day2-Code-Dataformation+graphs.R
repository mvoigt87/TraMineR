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
  
  # split the end.date (entry to retirement) variable in year, month, day
  separate(end.date, c("eyear","emonth","eday"),sep=c(4,6)) %>% 
  mutate(eyear=as.numeric(eyear)) %>% 
  mutate(emonth=as.numeric(emonth)) %>% 
  mutate(eday=as.numeric(eday)) %>%
  
  # create an age variable from the birth year
  mutate(entry.year = 2002) %>% 
  mutate(age = entry.year - byear) %>% 
  # extract the age groups 55-70 & Beginning state in 2001 & and widowhood
  filter(age>50 & age<=55 & syear>=2002 & benefit.type!="Viudedad")
  
  
 # 1.2. order by kID (because yu have an unordered long dataset and subset)

 # easy way
 retire <- retire[order(retire$kID) , ]
  
 # simple way = subset
 rs1 <- retire[1:2500,]
 
 # clean from cases with two retirement entries (like 104)
 # see cases with two identical spells (2 times retirement) = remember "dublicated" function)
 rs.test <- rs1[duplicated(rs1[1:2]) | duplicated(rs1[1:2], fromLast=TRUE),] 
 
 # double pension of any kind = 46 cases (meaning the same benefit.type for the same ID)
 # listwise deletion
 
 # now extract them!
 rs1 <-  rs1[!duplicated(rs1[1:2]),]
 
 
 ### 1.2. Separate the time data from the rest 
 
 rs2 <- rs1 %>% mutate(kID2 = kID) %>% 
   select(kID,benefit.type,syear, entry.year, end.cause,SITP,age,INVAL, byear, eyear) 

   
 ### check the number of cases with disability rent
 
 # rs.test2 <- rs2[duplicated(rs2[1]) | duplicated(rs2[1], fromLast=TRUE),]  # 104 individuals
 
 # Year of entry in disability as variable
 rs2$dis.y <- 9999
 rs2$dis.y[rs2$benefit.type=="Incapacidad"] <- rs2$syear[rs2$benefit.type=="Incapacidad"]
 
 # Year of entry in retirement as variable
 rs2$retire.y <- 9999
 rs2$retire.y[rs2$benefit.type=="Jubilación"] <- rs2$syear[rs2$benefit.type=="Jubilación"]
 
###################################################################################################
###################################################################################################
 
# give disability information to the same individual
 
 rs2 <- rs2 %>% select(-benefit.type) %>% group_by(kID) %>% summarise_all(funs(min))


### delete the big data sets
# rm(parned.c2002, ss.benefits.c2002, ss.benefits)
 


###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###### 
### 2. Prepare the TraMineR sts data set
 
 ### Alphabet
 
 # W - Working
 # NW - Not Working
 # D - Disabled
 # R - Retired
 
##  2.1. Define the entry dates
 
# working or not working (data from 2002)
 
 rs2$Work <- "W"
 rs2$Work[rs2$SITP==9] <- "NW" # define the once who are working
 # and overwrite the disabled cases
 rs2$Work[rs2$INVAL==1] <- "D"
 table(rs2$Work)

 # listwise deletion of cases with no disability information
 table(rs2$INVAL)
 rs2 <- rs2 %>% filter(INVAL < 9)
  
   ### test data 
   # data(actcal)
   # actcal.seq <- seqdef(actcal[,13:24])
 
## 2.2. Build the sts object
 
 
 
 # create 15 age at wave variable variable
 rs2 <- rs2 %>% mutate(a1 = age) %>% 
   mutate(a2 = age+1) %>% 
   mutate(a3 = age+2) %>% 
   mutate(a4 = age+3) %>% 
   mutate(a5 = age+4) %>% 
   mutate(a6 = age+5) %>% 
   mutate(a7 = age+6) %>% 
   mutate(a8 = age+7) %>% 
   mutate(a9 = age+8) %>% 
   mutate(a10 = age+9) %>% 
   mutate(a11 = age+10) %>% 
   mutate(a12 = age+11) %>% 
   mutate(a13 = age+12) %>% 
   mutate(a14 = age+13) %>% 
 
 # create 15 state variable - original missing values
  mutate(s1 = Work) %>% 
  mutate(s2 = Work) %>% 
  mutate(s3 = Work) %>% 
  mutate(s4 = Work) %>% 
  mutate(s5 = Work) %>% 
  mutate(s6 = Work) %>% 
  mutate(s7 = Work) %>% 
  mutate(s8 = Work) %>% 
  mutate(s9 = Work) %>% 
  mutate(s10 = Work) %>% 
  mutate(s11 = Work) %>% 
  mutate(s12 = Work) %>% 
  mutate(s13 = Work)
   

 # ---------- #
 # Disability #
 # ---------- #
cbind(names(rs2))

 ## run loop for disability
 for (i in 1:12){
   rs2[,27+i] <- ifelse(rs2$dis.y<=2002+i, "D", rs2$s1)
 }
 
 
 # ---------- #
 # Retirement #
 # ---------- #
 cbind(names(rs2))
 
 # check if peoples retirement entry is in the same year as there entry in disability
 table(rs2$entry.year[rs2$dis.y==rs2$retire.y])

 rs2$s2 <- ifelse(rs2$retire.y<=2003,"R",rs2$s1)
 rs2$s3 <- ifelse(rs2$retire.y<=2004,"R",rs2$s2)
 rs2$s4 <- ifelse(rs2$retire.y<=2005,"R",rs2$s3)
 rs2$s5 <- ifelse(rs2$retire.y<=2006,"R",rs2$s4)
 rs2$s6 <- ifelse(rs2$retire.y<=2007,"R",rs2$s5)
 rs2$s7 <- ifelse(rs2$retire.y<=2008,"R",rs2$s6)
 rs2$s8 <- ifelse(rs2$retire.y<=2009,"R",rs2$s7)
 rs2$s9 <- ifelse(rs2$retire.y<=2010,"R",rs2$s8)
 rs2$s10 <- ifelse(rs2$retire.y<=2011,"R",rs2$s9)
 rs2$s11 <- ifelse(rs2$retire.y<=2012,"R",rs2$s10)
 rs2$s12 <- ifelse(rs2$retire.y<=2013,"R",rs2$s11)
 rs2$s13 <- ifelse(rs2$retire.y<=2014,"R",rs2$s12)

 
 
 
 
 
 
 
# create a new factor variable for sex
 rs2$sex1 <- ls$sex
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
