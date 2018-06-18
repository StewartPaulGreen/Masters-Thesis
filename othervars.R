rm(list=ls())
library(readr)
data <- read_csv("B:/Project/26_64 Datasets/Few Variables/31varsCPUMA.csv")

n=11117637
#INS
INS=as.numeric(data$hins1==1|data$hins2==1|data$hins3==1|data$hins3==1|data$hins4==1|data$hins5==1|data$hins6==1)
data=cbind(data,INS)

#Post
POST=as.numeric(data$year>=14)
data=cbind(data,POST)


#Gender
GENDER=as.numeric(data$sex==1)
data=cbind(data,GENDER)

#Ethnicity
ETHNICITY=as.numeric(data$hisp!=1)
data=cbind(data,ETHNICITY)

#Expansion
EXPANSIONlist=c(53,41,6,32,4,35,8,38,27,19,17,5,21,39,54,25,10,34,36,44,9,24,50,11,15)
EXPANSION=as.numeric(data$st==53|data$st==41|data$st==6|data$st==32|data$st==4|data$st==35|data$st==8|data$st==38|data$st==27|data$st==19|data$st==17|data$st==5|data$st==21|data$st==39|data$st==54|data$st==25|data$st==10|data$st==34|data$st==36|data$st==44|data$st==9|data$st==24|data$st==50|data$st==11|data$st==15)
data=cbind(data,EXPANSION)

#Employed
EMP=as.numeric(data$wrk==1)

#Hours workedd
wkhp <- read_csv("B:/Project/26_64 Datasets/Few Variables/wkhp.csv")
write_csv(data,"B:/Project/26_64 Datasets/Few Variables/37varsEMP.csv")
data=cbind(data,wkhp)

#Parttime
PARTTIME=as.numeric(data$wrk==1&wkhp<=30)
data=cbind(data,PARTTIME)

#HS degree or less
HSless=as.numeric(data$schl<18)
data=cbind(data,HSless)

#Some college or more
SCmore=as.numeric(data$schl>=18)
data=cbind(data,SCmore)

#Married
MARRIED=as.numeric(data$hht==1)
data=cbind(data,MARRIED)

dropnapuma=data[-which(is.na(data$CPUMA)),]
write_csv(dropnapuma,"B:/Project/26_64 Datasets/Few Variables/42varsnochildunder18dropnapuma.csv")
#egen factorcpuma=group(cpuma), Stata code


data$POSTM=data$POST*data$M
data$POSTE=data$POST*data$E

data$POSTEXPANSION=data$POST*data$EXPANSION
data$POSTEXPANSIONM=data$POST*data$EXPANSION*data$M
data$POSTEXPANSIONE=data$POST*data$EXPANSION*data$E

data=cbind(data,dropnapuma$jwtr)
write_csv(data,"B:/Project/26_64 Datasets/Few Variables/48notinlaborforce.csv")

EMPLOYED=as.numeric(data$wrk==1)
EMPLOYED[which(is.na(EMPLOYED))]=0

UNEMPLOYED=as.numeric(data$wrk==2)
UNEMPLOYED[which(is.na(UNEMPLOYED))]=0

data=cbind(data,EMPLOYED,UNEMPLOYED)
write_csv(data,"B:/Project/26_64 Datasets/Few Variables/50nilfempunemp.csv")

data <- read_csv("B:/Project/26_64 Datasets/Few Variables/50nilfempunemp.csv")
nilf=as.numeric(data$`dropnapuma$jwtr`)
nilf[which(is.na(nilf))]=100
nilf[which(nilf!=100)]=0
nilf[which(nilf==100)]=1
data=cbind(data,nilf)
write_csv(data,"B:/Project/26_64 Datasets/Few Variables/52nilfempunemp.csv")

POSTEM=data$POST*(data$E+data$M)
POSTEXPANSIONEM=data$POST*data$EXPANSION*(data$E+data$M)
data=cbind(data,POSTEM,POSTEXPANSIONEM)
write_csv(data,"B:/Project/26_64 Datasets/Few Variables/54table6.csv")

#Race variables
n=11115191
black=numeric(n)
white=numeric(n)
asian=numeric(n)
other=numeric(n)
black=as.numeric(data$rac1p==2)
white=as.numeric(data$rac1p==1)
asian=as.numeric(data$rac1p==6)
other=as.numeric(data$rac1p==3|data$rac1p==4|data$rac1p==5|data$rac1p==7|data$rac1p==8|data$rac1p==9)

hispanic=numeric(n)
hispanic=as.numeric(data$hisp!=1)

data=cbind(data,black,white,asian,other,hispanic)

#Aggreated pumas
library(readxl)
crosswalk <- read_excel("B:/Project/PUMAs/PUMA2000_PUMA2010_crosswalk.xls")
crosswalk$PUMA00=as.numeric(crosswalk$PUMA00)
crosswalk$PUMA10=as.numeric(crosswalk$PUMA10)
crosswalk$CPUMA00=as.numeric(crosswalk$CPUMA00)
crosswalk$CPUMA10=as.numeric(crosswalk$CPUMA10)
n=11115191
CPUMA=numeric(n)
for(i in 1:n){
  if(data$year[i]<=11){
    CPUMA[i]=crosswalk$CPUMA00[which(crosswalk$PUMA00==data$puma[i]&crosswalk$State00==data$st[i])[1]]
  }
  else{
    CPUMA[i]=crosswalk$CPUMA10[which(crosswalk$PUMA10==data$puma[i]&crosswalk$State00==data$st[i])[1]]
  }
  if(i%%100000==0){
    print(i)
  }
}

crosswalk$State00=as.numeric(crosswalk$State00)
data$cpuma=CPUMA

#M*
M=numeric(n)
CPUMAlist=unique(data$cpuma)
for(i in CPUMAlist){
  total=length(which(data$cpuma==i&data$year<=13))
  less138=length(which(data$cpuma==i&data$year<=13&data$povpip<=138&data$ins==0))
  ratio=less138/total
  M[which(data$cpuma==i)]=ratio
  #M[which(data$CPUMA==2)]=(length(which(data$CPUMA==i&data$year<=13&data$povpip<=138&data$INS==0)))/(length(which(data$CPUMA==i&data$year<=13)))
}

#E*
E=numeric(n)
for(i in CPUMAlist){
  total=length(which(data$cpuma==i&data$year<=13))
  bt139399=length(which(data$cpuma==i&data$year<=13&data$povpip>=139&data$povpip<=399&data$ins==0))
  ratio=bt139399/total
  E[which(data$cpuma==i)]=ratio
}

data$m=M
data$e=E

data$s2m=(data$m-mean(data$m))
data$s2e=(data$e-mean(data$e))

data$s2postm=data$post*data$s2m
data$s2poste=data$post*data$s2e

data$s2expansionpostm=data$post*data$expansion*data$s2m
data$s2expansionposte=data$post*data$expansion*data$s2e

data$s2postem=data$post*(data$s2e+data$s2m)
data$s2expansionpostem=data$post*data$expansion*(data$s2e+data$s2m)

#Not in labor force
data$nifl=as.numeric(data$esr==6)

#Other health insurance variables
data$hins4=as.numeric(data$hins4==1)
data$hins2=as.numeric(data$hins2==1)
data$hins1=as.numeric(data$hins1==1)

write_csv(data,"B:/Project/26_64 Datasets/Few Variables/76vars.csv")
