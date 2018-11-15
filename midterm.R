library(readr)
StaffDemo16 <- read_delim("Desktop/biosta 203A/midterm/StaffDemo16.txt", 
                          "\t", escape_double = FALSE, col_types = cols(AcademicYear = col_number(), 
                                                                        Age = col_number(), DistrictCode = col_character(), 
                                                                        EthnicGroup = col_number(), `FTE PupilServices` = col_number(), 
                                                                        `FTE Teaching` = col_number(), `FTE administrative` = col_number(), 
                                                                        FileCreated = col_date(format = "%m/%d/%Y"), 
                                                                        RecID = col_number(), YearsInDistrict = col_number(),
                                                                        YearsTeaching = col_number()), trim_ws = TRUE)
library(readr)
filesdropouts <- read_delim("Desktop/biosta 203A/midterm/filesdropouts.txt", 
                            "\t", escape_double = FALSE, col_types = cols(CDS_CODE = col_character(), 
                                                                          D10 = col_number(), D11 = col_number(), 
                                                                          D12 = col_number(), D7 = col_number(), 
                                                                          D8 = col_number(), D9 = col_number(), 
                                                                          DTOT = col_number(), DUS = col_number(), 
                                                                          E10 = col_number(), E11 = col_number(), 
                                                                          E12 = col_number(), E7 = col_number(), 
                                                                          E8 = col_number(), E9 = col_number(), 
                                                                          ETHNIC = col_number(), ETOT = col_number(), 
                                                                          EUS = col_number(), YEAR = col_number()), 
                            trim_ws = TRUE)
library(readxl)
DistrictSubset <- read_excel("Desktop/biosta 203A/midterm/DistrictSubset.xlsx")



# extract data from origin data
staff_sub <- StaffDemo16[StaffDemo16$DistrictCode %in% DistrictSubset$DistrictCode,]
dropouts_sub <- filesdropouts[substring(filesdropouts$CDS_CODE,1,7) %in% DistrictSubset$DistrictCode,]

# gender
gender <-staff_sub$GenderCode
gender <- factor(staff_sub$GenderCode, levels=c('F','M',''))
freq_gender <- table(gender)
percent_gender <- prop.table(freq_gender)

# age
age_level <- ifelse(
  staff_sub$Age<=30, '17-30',
  ifelse(
    staff_sub$Age<=45,'31-45',
    ifelse(
      staff_sub$Age<=60,'46-60',
      ifelse(
        staff_sub$Age<=75,'61-75',
        ifelse(
          staff_sub$Age,'76 or older'
        )
      )
    )
  )
)
new_age <- data.frame(staff_sub,age_level)
freq_age <- table(new_age$age_level)
percent_age<- prop.table(freq_age)

#education level
education <- ifelse(
  staff_sub$EducationLevel=='D', 'Doctorate',
  ifelse(
    staff_sub$EducationLevel %in% c('M','V'),'Master’s Degree',
    ifelse(
      staff_sub$EducationLevel %in% c('U','F','Y'),'Fifth year',
      ifelse(
        staff_sub$EducationLevel %in% c('B','C'),'Baccalaureate',
        ifelse(
          staff_sub$EducationLevel=='A','Associate Degree',
          ifelse(
            staff_sub$EducationLevel %in% c('N','S'),'Not Reported or Special',
            ifelse(
              'missing'
            )
          )
        )
      )
    )
  )
)

new_education <- data.frame(staff_sub,education)
freq_education <- table(new_education$education)
percent_education <- prop.table(freq_education)

#ethinic
freq_ethinic <- table(staff_sub$EthnicGroup)
percent_ethinic <- prop.table(freq_ethinic)

#employment 
employment_status<-staff_sub$EmploymentStatusCode
employment_status <- factor(employment_status,exclude='',labels=c('L','O','P','T','N'))
freq_employment <- table(employment_status)
percent_employment <- prop.table(freq_employment)

# mean and sd
mean(staff_sub$YearsTeaching)
sd(staff_sub$YearsTeaching)
mean(staff_sub$YearsInDistrict)
sd(staff_sub$YearsInDistrict)
mean(staff_sub$`FTE Teaching`)
sd(staff_sub$`FTE Teaching`)
mean(staff_sub$`FTE administrative`)
sd(staff_sub$`FTE administrative`)
mean(staff_sub$`FTE PupilServices`)
sd(staff_sub$`FTE PupilServices`)





# district_level
dist <- substr(dropouts_sub$CDS_CODE,1,7)
dist_list <- unique(DistrictSubset$DistrictCode)
new_drop <- data.frame(dist,dropouts_sub)


#gender
total_enroll <- new_drop$E7+new_drop$E8+new_drop$ETOT
new_drop <- data.frame(new_drop,total_enroll)
female_enroll <-c()
for (i in 1:750){
  s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$GENDER=='F',c('total_enroll')])
  female_enroll <- c(female_enroll,s) 
  }
summary(female_enroll)




male_enroll <-c()
for (i in 1:750){
  s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$GENDER=='M',c('total_enroll')])
  male_enroll <- c(male_enroll,s) 
}
summary(male_enroll)


#ethnic
ethnic_enroll <- function(j){
  ethnic <-c()
  for (i in 1:750){
    s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$ETHNIC==j,c('total_enroll')])
    ethnic <- c(ethnic,s) 
  }
  return (ethnic)
}
for (j in 0:9){
  print (summary(ethnic_enroll(j)))
}


# second solution
E7_8 <- new_drop$E7+new_drop$E8
E9_10 <- new_drop$E9+new_drop$E10
E11_12 <- new_drop$E11+new_drop$E12
new_drop <- data.frame(new_drop,E7_8,E9_10,E11_12)

enroll_grade <- function(data){
  enroll <-c()
  for (i in 1:750){
    s <- sum(new_drop[new_drop$dist==dist_list[i],c(data)])
    enroll<- c(enroll,s) 
  }
  return (enroll)
}


#dropout
total_drop <- new_drop$D7+new_drop$D8+new_drop$DTOT
new_drop <- data.frame(new_drop,total_drop)

#gender
female_drop <-c()
for (i in 1:750){
  s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$GENDER=='F',c('total_drop')])
  female_drop <- c(female_drop,s) 
}
female_drop_rate <-female_drop/female_enroll
summary(female_drop/female_enroll)
sd(female_drop/female_enroll,na.rm = T)

male_drop <-c()
for (i in 1:750){
  s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$GENDER=='M',c('total_drop')])
  male_drop <- c(male_drop,s) 
}
male_drop_rate <-male_drop/male_enroll
summary(male_drop/male_enroll)
sd(male_drop/male_enroll, na.rm = T)

# ethnic
ethnic_drop <- function(j){
  ethnic <-c()
  for (i in 1:750){
    s <- sum(new_drop[new_drop$dist==dist_list[i] & new_drop$ETHNIC==j,c('total_drop')])
    ethnic <- c(ethnic,s) 
  }
  return (ethnic)
}

#x <- (ethnic_drop(1)/ethnic_enroll(1))[!is.infinite(ethnic_drop(1)/ethnic_enroll(1))]

 
# grade drop
D7_8 <- new_drop$D7+new_drop$D8
D9_10 <- new_drop$D9+new_drop$D10
D11_12 <- new_drop$D11+new_drop$D12
new_drop <- data.frame(new_drop,D7_8,D9_10,D11_12)

drop_grade <- function(data){
  drop <-c()
  for (i in 1:750){
    s <- sum(new_drop[new_drop$dist==dist_list[i],c(data)])
    drop<- c(drop,s) 
  }
  return (drop)
}

summary(drop_grade('D7_8')/enroll_grade('E7_8'))

#
total_rate <- c()
for (i in 1: length(dist_list)){
  total_drop <- sum(new_drop[new_drop$dist==dist_list[i],'total_drop'])
  total_enroll <-sum(new_drop[new_drop$dist==dist_list[i],'total_enroll'])
  rate <- total_drop/total_enroll
  total_rate <- c(total_rate,rate)
}

#-	mean 
mean_age <- c()
mean_staff_Years_Teaching <-c()
mean_staff_Years_in_District <- c()
mean_FTE_Teaching <- c()
mean_FTE_Administrative <-c()
mean_FTE_Pupil_Services <-c()

for (i in 1: length(dist_list)){
  dist_1 <- staff_sub[staff_sub$DistrictCode==dist_list[i],c('Age','YearsTeaching','YearsInDistrict','FTE Teaching','FTE administrative','FTE PupilServices')]
  mean_age <- c(mean_age,mean(dist_1$Age))
  mean_staff_Years_Teaching <-c(mean_staff_Years_Teaching,mean(dist_1$YearsTeaching))
  mean_staff_Years_in_District <- c(mean_staff_Years_in_District,mean(dist_1$YearsInDistrict))
  mean_FTE_Teaching <-c(mean_FTE_Teaching,mean(dist_1$`FTE Teaching`))
  mean_FTE_Administrative <- c(mean_FTE_Administrative,mean(dist_1$`FTE administrative`))
  mean_FTE_Pupil_Services <- c(mean_FTE_Pupil_Services, mean(dist_1$`FTE PupilServices`))
}

#-	Percentage of staff with a Masters or Doctorate  / tenu
per_staff <-c()
per_tenu <-c()
for (i in 1: length(dist_list)){
  dist <- staff_sub[staff_sub$DistrictCode==dist_list[i],c('EducationLevel',"EmploymentStatusCode")]
  edu <- dist$EducationLevel %in% c('D','M')
  employ <- dist$EmploymentStatusCode %in% c('T')
  per<- sum(edu)/nrow(dist)
  per_t <- sum(employ)/nrow(dist)
  per_staff <- c(per_staff,per)
  per_tenu <-c(per_tenu,per_t)
  
}

#merge
new_merge <-data.frame(dist_list,mean_age,mean_staff_Years_Teaching,mean_FTE_Administrative,mean_FTE_Pupil_Services,mean_FTE_Teaching,mean_staff_Years_in_District,mean_staff_Years_in_District,per_staff,per_tenu,total_rate)

compare_median <- total_rate<=median(total_rate)
new_merge <- data.frame(new_merge,compare_median)

#  <= median
less_median <- new_merge[new_merge$compare_median==TRUE,]
more_median <-new_merge[new_merge$compare_median==FALSE,]

mean(more_median$mean_staff_Years_Teaching,na.rm=T)

#plot
library(tidyr)
FET <- gather(new_merge,key=type,value=mean_fte,c('mean_FTE_Teaching','mean_FTE_Administrative','mean_FTE_Pupil_Services'))
library(ggplot2)
  ggplot(FET,aes(x=total_rate,y=mean_fte,color=type))+
  geom_smooth(method = 'lm',se=FALSE)+
  geom_point()
  labs(x="drop_rate",y="FTE",title = "FTE-drop rate")

  