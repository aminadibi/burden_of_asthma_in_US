options(java.parameters = "- Xmx1024m") #prevents Java memory issue with XLConnect

library(XLConnect)
library(tidyverse)

tidyCost <- function (province, type) {
  wb <- loadWorkbook("./burden_of_COPD/Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  excelName <- paste0(province, "_cost_", type)
  data <- readNamedRegion(wb, name = excelName)
  data$province <- province
  data$type <- type
  data %>% select (-total) %>% gather("genderage", "value", 2:9) %>% separate (genderage, into=c("gender", "age"))
}

tidyNumber <- function (province) {
  wb <- loadWorkbook("./burden_of_COPD/Burden_of_COPD_BC_ProvidenceAPR04.xlsx", create=F)
  excelName <- paste0(province, "_number")
  data <- readNamedRegion(wb, name = excelName)
  data$province <- province
  data %>% select (-total) %>% gather("genderage", "value", 2:9) %>% separate (genderage, into=c("gender", "age"))
}

#hosp
cost <- tidyCost("BC", "hosp")
cost <- rbind(cost, tidyCost("ON", "hosp"))
cost <- rbind(cost, tidyCost("AB", "hosp"))
cost <- rbind(cost, tidyCost("MB", "hosp"))
cost <- rbind(cost, tidyCost("NB", "hosp"))
cost <- rbind(cost, tidyCost("NL", "hosp"))
cost <- rbind(cost, tidyCost("NS", "hosp"))
cost <- rbind(cost, tidyCost("PE", "hosp"))
cost <- rbind(cost, tidyCost("QC", "hosp"))
cost <- rbind(cost, tidyCost("SK", "hosp"))

#MSP
cost <- rbind(cost, tidyCost("BC", "MSP"))
cost <- rbind(cost, tidyCost("ON", "MSP"))
cost <- rbind(cost, tidyCost("AB", "MSP"))
cost <- rbind(cost, tidyCost("MB", "MSP"))
cost <- rbind(cost, tidyCost("NB", "MSP"))
cost <- rbind(cost, tidyCost("NL", "MSP"))
cost <- rbind(cost, tidyCost("NS", "MSP"))
cost <- rbind(cost, tidyCost("PE", "MSP"))
cost <- rbind(cost, tidyCost("QC", "MSP"))
cost <- rbind(cost, tidyCost("SK", "MSP"))

#Pharm
cost <- rbind(cost, tidyCost("BC", "pharm"))
cost <- rbind(cost, tidyCost("ON", "pharm"))
cost <- rbind(cost, tidyCost("AB", "pharm"))
cost <- rbind(cost, tidyCost("MB", "pharm"))
cost <- rbind(cost, tidyCost("NB", "pharm"))
cost <- rbind(cost, tidyCost("NL", "pharm"))
cost <- rbind(cost, tidyCost("NS", "pharm"))
cost <- rbind(cost, tidyCost("PE", "pharm"))
cost <- rbind(cost, tidyCost("QC", "pharm"))
cost <- rbind(cost, tidyCost("SK", "pharm"))

#sum of the above three types
cost <- rbind(cost, tidyCost("BC", "sum"))
cost <- rbind(cost, tidyCost("ON", "sum"))
cost <- rbind(cost, tidyCost("AB", "sum"))
cost <- rbind(cost, tidyCost("MB", "sum"))
cost <- rbind(cost, tidyCost("NB", "sum"))
cost <- rbind(cost, tidyCost("NL", "sum"))
cost <- rbind(cost, tidyCost("NS", "sum"))
cost <- rbind(cost, tidyCost("PE", "sum"))
cost <- rbind(cost, tidyCost("QC", "sum"))
cost <- rbind(cost, tidyCost("SK", "sum"))

#adding "all" row for province
all_province <- cost %>% group_by(Year, age, gender, type) %>% summarize(value = sum(value))
all_province$province <- "Canada"
cost <- rbind(cost, as.data.frame(all_province))

#adding "all" row for gender
all_gender <- cost %>% group_by(Year, age, province, type) %>% summarize(value = sum(value))
all_gender$gender <- "all genders"
cost <- rbind(cost, as.data.frame(all_gender))

#adding "all" row for age group
all_age <- cost %>% group_by(Year, gender, province, type) %>% summarize(value = sum(value))
all_age$age <- "all ages"
cost <- rbind(cost, as.data.frame(all_age))



#Number
copdNumber <-  tidyNumber("BC")
copdNumber <- rbind(copdNumber, tidyNumber("ON"))
copdNumber <- rbind(copdNumber, tidyNumber("AB"))
copdNumber <- rbind(copdNumber, tidyNumber("MB"))
copdNumber <- rbind(copdNumber, tidyNumber("NB"))
copdNumber <- rbind(copdNumber, tidyNumber("NL"))
copdNumber <- rbind(copdNumber, tidyNumber("NS"))
copdNumber <- rbind(copdNumber, tidyNumber("PE"))
copdNumber <- rbind(copdNumber, tidyNumber("QC"))
copdNumber <- rbind(copdNumber, tidyNumber("SK"))

#adding "all" row for province
all_province <- copdNumber %>% group_by(Year, age, gender) %>% summarize(value = sum(value))
all_province$province <- "Canada"
copdNumber <- rbind(copdNumber, as.data.frame(all_province))

#adding "all" row for gender
all_gender <- copdNumber %>% group_by(Year, age, province) %>% summarize(value = sum(value))
all_gender$gender <- "all genders"
copdNumber <- rbind(copdNumber, as.data.frame(all_gender))

#adding "all" row for age group
all_age <- copdNumber %>% group_by(Year, gender, province) %>% summarize(value = sum(value))
all_age$age <- "all ages"
copdNumber <- rbind(copdNumber, as.data.frame(all_age))

#save
cost <- write_rds(cost, "./burden_of_COPD/cost.rds")
copdNumber <- write_rds(copdNumber, "./burden_of_COPD/copdNumber.rds")