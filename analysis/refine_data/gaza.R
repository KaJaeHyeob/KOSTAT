##============================================================================
## Title : 제주도의 범죄율이 유독 높은 이유는?
## Author : Miru
##============================================================================

# delete all objs
rm(list=ls())

# set working directory
setwd("jaehyeob")
setwd("Downloads")
setwd("intern")
setwd("refine_data")

# 데이터 불러오기
install.packages("rJava", type = 'source')
install.packages("readxl")
library(readxl)
AdmArea <- read.xlsx("AdmArea.xlsx",sheetIndex=1)
Aging <- read.xlsx("Aging.xlsx",sheetIndex=1)
Arrest <- read.xlsx("Arrest.xlsx",sheetIndex=1)
Car_num <- read.xlsx("Car_num.xlsx",sheetIndex=1)
CrimePerThousand <- read.xlsx("CrimePerThousand.xlsx", sheetIndex = 1)
EcoAct <- read.xlsx("EcoAct.xlsx", sheetIndex = 1)
EcoDev <- read.xlsx("EcoDev.xlsx", sheetIndex=1)
FM <- read.xlsx("FM.xlsx", sheetIndex=1)
ForeignerPerThousand <- read.xlsx("ForeignerPerThousand.xlsx", sheetIndex=1)
Grdp <- read.xlsx("GRDP.xlsx",sheetIndex=1)
Houseprice <- read.xlsx("Houseprice.xlsx", sheetIndex = 1)
HyDrink <- read.xlsx("HyDrink.xlsx",sheetIndex = 1)
Hystress <- read.xlsx("Hystress.xlsx",sheetIndex=1)
Moving_rate <- read.xlsx("Moving_rate.xlsx", sheetIndex = 1)
Movingin_rate <- read.xlsx("Movingin_rate.xlsx", sheetIndex = 1)
Outgoing_rate <- read.xlsx("Outgoing_rate.xlsx", sheetIndex=1)
Park <- read.xlsx("Park.xlsx", sheetIndex=1)
PolicePop <- read.xlsx("PolicePop.xlsx", sheetIndex = 1)
PopDensity <- read.xlsx("PopDensity.xlsx", sheetIndex=1)
PopGrowth <- read.xlsx("PopGrowth.xlsx", sheetIndex=1)
Unemply_rate <- read.xlsx("Unemply_rate.xlsx",sheetIndex=1)
Population <- read.xlsx("Population.xlsx", sheetIndex = 1)

# 타지역과 비교할 것이기 때문에 단순한 수는 취급안함_비율만
#Police <- read.xlsx("Police.xlsx", sheetIndex=1)
#Crime <- read.xlsx("Crime.xlsx",sheetIndex = 1)
#CrimeArre <- read.xlsx("CrimeArre.xlsx", sheetIndex = 1)
#Region <- read.xlsx("Region.xlsx", sheetIndex = 1)

## NA값 삭제 / Factor type 변호
AdmArea <- AdmArea[-c(18,19),-c(10:59)]
str(AdmArea)

Aging <- Aging[-9,]
str(Aging)
Aging$X2011 <-  as.numeric(gsub(",","",Aging$X2011))
Aging$X2010 <- as.numeric(gsub(",","",Aging$X2010))

Car_num <- Car_num[-c(9,19,20),-c(10:58)]
str(Car_num)
Car_num$X2011 <-  as.numeric(gsub(",","",Car_num$X2011))
Car_num$X2010 <- as.numeric(gsub(",","",Car_num$X2010))

CrimePerThousand <- CrimePerThousand[-9,]
str(CrimePerThousand)

EcoAct <- EcoAct[-c(9,19,20),-c(10:35)]
str(EcoAct)
EcoAct$X2016 <-  as.numeric(gsub(",","",EcoAct$X2016))
EcoAct$X2015 <- as.numeric(gsub(",","",EcoAct$X2015))
EcoAct$X2014 <-  as.numeric(gsub(",","",EcoAct$X2014))
EcoAct$X2013 <- as.numeric(gsub(",","",EcoAct$X2013))
EcoAct$X2012 <-  as.numeric(gsub(",","",EcoAct$X2012))
EcoAct$X2011 <- as.numeric(gsub(",","",EcoAct$X2011))
EcoAct$X2010 <- as.numeric(gsub(",","",EcoAct$X2010))

EcoDev <- EcoDev[-c(18,19,20),-c(10:35)]
str(EcoDev)

FM <- FM[-c(9,19,20),]
str(FM)
FM$X2011 <- as.numeric(gsub(",","",FM$X2011))
FM$X2010 <- as.numeric(gsub(",","",FM$X2010))

ForeignerPerThousand <- ForeignerPerThousand[-9,]
str(ForeignerPerThousand)
ForeignerPerThousand$X2011 <- as.numeric(gsub(",","",ForeignerPerThousand$X2011))
ForeignerPerThousand$X2010 <- as.numeric(gsub(",","",ForeignerPerThousand$X2010))

Grdp <- Grdp[-c(19,20,9),-c(10:35)]
str(Grdp)

Houseprice <- Houseprice[-c(9,19,20),-c(10:58)]
str(Houseprice)
Houseprice$X2011 <- as.numeric(gsub(",","",Houseprice$X2011))
Houseprice$X2010 <- as.numeric(gsub(",","",Houseprice$X2010))

HyDrink <- HyDrink[-c(9,19,20),-c(10:35)]
str(HyDrink)
HyDrink$X2011 <- as.numeric(gsub(",","",HyDrink$X2011))
HyDrink$X2010 <- as.numeric(gsub(",","",HyDrink$X2010))

Hystress <- Hystress[-c(9,19,20),-c(10:35)] 
str(Hystress)
Hystress$X2011 <- as.numeric(gsub(",","",Hystress$X2011))
Hystress$X2010 <- as.numeric(gsub(",","",Hystress$X2010))

Moving_rate <- Moving_rate[-9,]
str(Moving_rate)

Movingin_rate <- Movingin_rate[-9,]
str(Movingin_rate)
Movingin_rate$X2011 <- as.numeric(gsub(",","",Movingin_rate$X2011))
Movingin_rate$X2010 <- as.numeric(gsub(",","",Movingin_rate$X2010))

Outgoing_rate <- Outgoing_rate[-9,]
str(Outgoing_rate)
Outgoing_rate$X2011 <- as.numeric(gsub(",","",Outgoing_rate$X2011))
Outgoing_rate$X2010 <- as.numeric(gsub(",","",Outgoing_rate$X2010))

Park <- Park[-c(9,19,20),-c(10:58)]
str(Park)
Park$X2011 <- as.numeric(gsub(",","",Park$X2011))
Park$X2010 <- as.numeric(gsub(",","",Park$X2010))

PolicePop <- PolicePop[-9,]
str(PolicePop)
PolicePop$X2017 <- as.numeric(gsub(",","",PolicePop$X2017))
PolicePop$X2016 <- as.numeric(gsub(",","",PolicePop$X2016))
PolicePop$X2015 <- as.numeric(gsub(",","",PolicePop$X2015))
PolicePop$X2014 <- as.numeric(gsub(",","",PolicePop$X2014))
PolicePop$X2013 <- as.numeric(gsub(",","",PolicePop$X2013))
PolicePop$X2012 <- as.numeric(gsub(",","",PolicePop$X2012))
PolicePop$X2011 <- as.numeric(gsub(",","",PolicePop$X2011))
PolicePop$X2010 <- as.numeric(gsub(",","",PolicePop$X2010))

PopDensity <- PopDensity[-9,]
str(PopDensity)

PopGrowth <- PopGrowth[-c(9,19),-10]
str(PopGrowth)
PopGrowth$X2012 <- as.numeric(gsub(",","",PopGrowth$X2012))
PopGrowth$X2011 <- as.numeric(gsub(",","",PopGrowth$X2011))
PopGrowth$X2010 <- as.numeric(gsub(",","",PopGrowth$X2010))

Unemply_rate <- Unemply_rate[-c(18,19,20),-c(10:58)] 
str(Unemply_rate)

Population <- Population[-9,]
str(Population)

# new dataframe _ Jeju
Jeju <- data.frame(period=c(2017:2010))
Jeju$AdmArea <- c(AdmArea[17,-1])
Jeju$Aging <- c(Aging[17,-1])
Jeju$Arrest <- c(Arrest[17,-1])
Jeju$Car_num <- c(Car_num[17,-1])
Jeju$CrimePerThousand <- c(CrimePerThousand[17,-1])
Jeju$EcoAct <- c(EcoAct[17,-1])
Jeju$EcoDev <- c(EcoDev[17,-1])
Jeju$FM <- c(FM[17,-1])
Jeju$ForeignerPerThousand <- c(ForeignerPerThousand[17,-1])
Jeju$Grdp <- c(Grdp[17,-1])
Jeju$Houseprice <- c(Houseprice[17,-1])
Jeju$HyDrink <- c(HyDrink[17,-1])
Jeju$Hystress <- c(Hystress[17,-1])
Jeju$Moving_rate <- c(Moving_rate[17,-1])
Jeju$Movingin_rate <- c(Movingin_rate[17,-1])
Jeju$Outgoing_rate <- c(Outgoing_rate[17,-1])
Jeju$Park <- c(Park[17,-1])
Jeju$PolicePop <- c(PolicePop[17,-1])
Jeju$PopDensity <- c(PopDensity[17,-1])
Jeju$PopGrowth <- c(PopGrowth[17,-1])
View(Jeju)

## dataframe _ Jeju 에서 회귀분석
