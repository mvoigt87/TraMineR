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
  filter(age==55 & syear>=2002 & benefit.type!="Viudedad")
  
  
 # 1.2. order by kID (because yu have an unordered long dataset and subset)

 # easy way
 retire <- retire[order(retire$kID) , ]
 
 # Year of entry in disability as variable
 retire$dis.y <- 9999
 retire$dis.y[retire$benefit.type=="Incapacidad"] <- retire$syear[retire$benefit.type=="Incapacidad"]
 
 # Year of entry in retirement as variable
 retire$retire.y <- 9999
 retire$retire.y[retire$benefit.type=="Jubilación"] <- retire$syear[retire$benefit.type=="Jubilación"]
 
 # create a new factor variable for sex
 retire$sex <- "male"
 retire$sex[retire$SEXO==6] <- "female"
 retire$sex <- as.factor(retire$sex)
 relevel (retire$sex,'female') ->  retire$sex
 sex.tb <- table(retire$sex)
 round(100*sex.tb/sum(sex.tb), digits=2) 
 
 # ------------------------------------------------------- #
 # clean from cases with two retirement entries (like 104)
 # see cases with two identical spells (2 times retirement) = remember "dublicated" function)
 r.test <- retire[duplicated(rs1[1:2]) | duplicated(rs1[1:2], fromLast=TRUE),] 
 
 # double pension of any kind = 46 cases (meaning the same benefit.type for the same ID)
 # listwise deletion
 
 # now extract them!
 retire <-  retire[!duplicated(rs1[1:2]),]
 
 # ------------------------------------------------------ #
 
 # give disability information to the same individual
 
 retire <- retire %>% select(-benefit.type) %>% group_by(kID) %>% summarise_all(funs(min))
 

 # ------------------------------------------------------- #
 
 ##########  
 # SAMPLE #
 #--------#
 
 # sample size 2500
 
 rs2 <- retire[sample(nrow(retire), 2500), ]
 
 
 ### 1.2. Separate the time data from the rest 
 
 rs2 <- rs2%>% select(kID, syear, entry.year, sex ,SITP,age,INVAL, byear, eyear,dis.y,retire.y) 

   
 ### check the number of cases with disability rent
 
 # rs.test2 <- rs2[duplicated(rs2[1]) | duplicated(rs2[1], fromLast=TRUE),]  # 104 individuals
 
 # # Year of entry in disability as variable
 # rs2$dis.y <- 9999
 # rs2$dis.y[rs2$benefit.type=="Incapacidad"] <- rs2$syear[rs2$benefit.type=="Incapacidad"]
 # 
 # # Year of entry in retirement as variable
 # rs2$retire.y <- 9999
 # rs2$retire.y[rs2$benefit.type=="Jubilación"] <- rs2$syear[rs2$benefit.type=="Jubilación"]
 # 
 # # give disability information to the same individual
 # 
 # rs2 <- rs2 %>% select(-benefit.type) %>% group_by(kID) %>% summarise_all(funs(min))


### delete the big data sets
# rm(parned.c2002, ss.benefits.c2002, ss.benefits)
 


###### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ###### 
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
 
 
 
 #
 rs2 <- rs2 %>% 
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
   rs2[,13+i] <- ifelse(rs2$dis.y<=2002+i, "D", rs2$s1)
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

 ###################################################################################################
 
 ### Enter data into TraMineR
 
 # Prepare Matrix for the seqdef() command
 cbind(names(rs2))
 
 seqmat <- rs2[,c(13:25)]
 
 
 # seqdef to create an object for TraMineR
 
 ?seqdef
 
 RetSeq <- seqdef(seqmat,informat = "STS", alphabet = c("W","NW","D","R"),id="auto", start = 55, 
           labels = c("Working","NotWorking","Disablitiy","Retired"))
 
 summary(RetSeq)
 
 # [>] sequence object created with TraMineR version 2.0-7 
 # [>] 2498 sequences in the data set, 33 unique 
 # [>] min/max sequence length: 13/13
 # [>] alphabet (state labels):  
 #   1=W (Working)
 # 2=NW (NotWorking)
 # 3=D (Disablitiy)
 # 4=R (Retired)
 # [>] dimensionality of the sequence space: 39 
 # [>] colors: 1=#7FC97F 2=#BEAED4 3=#FDC086 4=#FFFF99
 
 
 ###################################################################################################

 ### Plotting the sequences
 ?seqplot
 
 par(mfrow=c(1,2))
 seqplot(RetSeq,type = "i", with.legend = FALSE)
 seqlegend(RetSeq)
 
 # big I plot by sex (and ordered sequences)
 par(mfrow=c(1,2))
 seqplot(RetSeq, type = "I",group = rs2$sex, with.legend = FALSE, sort="from.end")

 
 # d- plot - cummulated state plot
 par(mfrow=c(1,2))
 seqdplot(RetSeq, with.legend = FALSE)
 seqlegend(RetSeq)
 
 # f- plot - cummulated frequencies of sequences
 seqfplot(RetSeq)
 
 ###################################################################################################
 ###################################################################################################
 
 
 ### ----------------------------
 ### Sequence length - seq.length
 
 table(seqlength(RetSeq)) # all 2498 have a length of 13 => may be use death
 
 
 ### ----------------------------------------------------------------
 ### state distribution table as numerical counterpart for the d-plot
 
 seqstatd(RetSeq)
 
 
 #       s1    s2   s3    s4    s5     s6     s7     s8     s9    s10    s11    s12   s13
 # W  0.663 0.661 0.66 0.656 0.655 0.6025 0.5641 0.5344 0.4988 0.4576 0.1541 0.1281 0.110
 # NW 0.327 0.323 0.32 0.321 0.320 0.2906 0.2826 0.2754 0.2642 0.2586 0.0933 0.0725 0.067
 # D  0.011 0.010 0.01 0.010 0.010 0.0092 0.0092 0.0092 0.0092 0.0088 0.0048 0.0048 0.004
 # R  0.000 0.006 0.01 0.013 0.016 0.0977 0.1441 0.1809 0.2278 0.2750 0.7478 0.7946 0.819
 
 ### --------------------
 ### Sequence Frequencies
 
 seqtab(RetSeq)
 
 #           Freq Percent
 # W/10-R/3   758    30.3
 # NW/10-R/3  413    16.5
 # W/13       275    11.0
 # NW/13      167     6.7
 # W/5-R/8    130     5.2
 # W/9-R/4    103     4.1
 # W/6-R/7     96     3.8
 # W/8-R/5     89     3.6
 # W/7-R/6     74     3.0
 # NW/5-R/8    73     2.9

 
 ### --------------------
 ### Distinct state - DSS
 seqdss(head(RetSeq))
 
 
 ### -----------------------------
 ### Distinct state Duration - DSS
 seqdur(head(RetSeq))
 
 ### ------------------------
 ### Sequence duration state 
 seqistatd(RetSeq[1:6,])

 ### ----------------
 ### Transition Rate
 seqtrate(RetSeq)
 round(seqtrate(RetSeq),2)
 
 ############ COMPLEXITY MEASURES
 
 ### ----------------
 ### Shannon Entropy
 seqient(head(RetSeq))
 
 
 ### ----------
 ### Turbulence
 hist(seqST(RetSeq))
 
 
 ######### Group Differences
 
 ## vector (T/F) for the being male or not in the length of the dataframe (row number)
 filter.men <- which(rs2$sex=="male")
 ## we use this vector to extract the males from the data set
 men.sts <- RetSeq[filter.men,]
 

 ## and do the same for the females
 filter.women <- which(rs2$sex=="female")
 women.sts <- RetSeq[filter.women,]
 
 ### See difference in turbulence
 
 turb.men <- seqST(men.sts)
 
 turb.women <- seqST(women.sts)
 
 boxplot(turb.men,ylim=c(0,15))
 boxplot(turb.women,ylim=c(0,15)) 
 
 ##### to do:
 
# 1. complexty measures
# 2. add the death as a state
# 3. clustering