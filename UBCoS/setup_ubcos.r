# Cleaning local memory
rm(list = ls())
gc(reset=T)

# For the first time, uncomment and run following two lines to install needed packages.
#install.packages(c('RCurl', 'rjson'), repos=c('http://cran.rstudio.com/', 'http://www.stats.ox.ac.uk/pub/RWin/'))
#install.packages('opal', repos='http://cran.obiba.org', type='source')
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, warning = FALSE, message = FALSE)

# Previous to run following lines, review if the following packages are already installed.
list.of.packages <- c("knitr", "haven", "sjmisc", "bitops", "RCurl", "rjson", "mime", "opal", "tibble", "car", "labelled", "Hmisc", "scales", "dplyr", "lubridate", "stringi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# loading UBCoS source dataset prepared by Anna Goodman in Stata format.
db <- read_dta("/home/albert/athlos_confidential/UBCoS/AthlosHarmonise_UBCoS_170830.dta")

# Names of variables to include in each wave
namesW1 <- names(db[,c(1,6:11,12,18,24,28,38,53,54,60,61,67,68,74,75,81,82,88,89,95,96)])
namesW2 <- names(db[,c(1,6:11,13,19,25,29,33,39,45,49,53,54,55,60,61,62,67,68,69,74,75,76,81,82,83,88,89,90,95,96,97, 102:104, 117:119, 132:134, 147:149, 162:164)])
namesW3 <- names(db[, c(1,6:11,14,20,26,40,44,53:54,56,60:61,63,67:68,70,74:75,77,81:82,84,88:89,91,95:96,98, 105:107, 120:122, 135:137, 150:152, 165:167)])
namesW4 <- names(db[, c(1,6:11,15,21,27,30,34,35,41,46,50,53:54,57,60:61,64,67:68,71,74:75,78,81:82,85,88:89,92,95:96,99, 108:110, 123:125, 138:140, 153:155, 168:170)])
namesW5 <- names(db[, c(1,6:11,16,22,31,36,42,47,51,53:54,58,60:61,65,67:68,72,74:75,79,81:82,86,88:89,93,95:96,100, 111:113, 126:128, 141:143, 156:158, 171:173)])
namesW6 <- names(db[, c(1,6:11,17,23,32,37,43,48,52,53:54,59,60:61,66,67:68,73,74:75,80,81:82,87,88:89,94,95:96,101, 114:116, 129:131, 144:146, 159:161, 174:176)])

# First generation

ubcosG1W1 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW1,"ubcos1","ubcos1partner")]
ubcosG1W2 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW2,"ubcos1","ubcos1partner")]
ubcosG1W3 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW3,"ubcos1","ubcos1partner")]
ubcosG1W4 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW4,"ubcos1","ubcos1partner")]
ubcosG1W5 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW5,"ubcos1","ubcos1partner")]
ubcosG1W6 <- db[ which(db$ubcos1==1 | db$ubcos1partner==1), c(namesW6,"ubcos1","ubcos1partner")]

# Second generation
ubcosG2W1 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW1,"ubcos2","ubcos2partner")]
ubcosG2W2 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW2,"ubcos2","ubcos2partner")]
ubcosG2W3 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW3,"ubcos2","ubcos2partner")]
ubcosG2W4 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW4,"ubcos2","ubcos2partner")]
ubcosG2W5 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW5,"ubcos2","ubcos2partner")]
ubcosG2W6 <- db[ which(db$ubcos2==1 | db$ubcos2partner==1), c(namesW6,"ubcos2","ubcos2partner")]

# Remove not applicable participants:
# int_status == 0 'not yet born/inappropriate' should be removed
# int_status == 4 'no respond, died in previous waves' should be removed
# int_status == 6 'no respond, emigrated in previous waves' should be removed
# Individuals who died in previous waves even though some information is collected in that wave.

ubcosG1W1 <- ubcosG1W1[!ubcosG1W1$int_status1960 %in% c(0,4,6),]  
ubcosG1W2 <- ubcosG1W2[!ubcosG1W2$int_status1970 %in% c(0,4,6),]  
ubcosG1W2 <- ubcosG1W2[ is.na(ubcosG1W2$date_death) | ubcosG1W2$date_death >= ymd("19601101"),]  
ubcosG1W3 <- ubcosG1W3[!ubcosG1W3$int_status1980 %in% c(0,4,6),]  
ubcosG1W3 <- ubcosG1W3[ is.na(ubcosG1W3$date_death) | ubcosG1W3$date_death >= ymd("19701101"),]  
ubcosG1W4 <- ubcosG1W4[!ubcosG1W4$int_status1990 %in% c(0,4,6),]  
ubcosG1W4 <- ubcosG1W4[ is.na(ubcosG1W4$date_death) | ubcosG1W4$date_death >= ymd("19800915"),]  
ubcosG1W5 <- ubcosG1W5[!ubcosG1W5$int_status2000 %in% c(0,4,6),]  
ubcosG1W5 <- ubcosG1W5[ is.na(ubcosG1W5$date_death) | ubcosG1W5$date_death >= ymd("19901101"),]  
ubcosG1W6 <- ubcosG1W6[!ubcosG1W6$int_status2008 %in% c(0,4,6),]
ubcosG1W6 <- ubcosG1W6[ is.na(ubcosG1W6$date_death) | ubcosG1W6$date_death >= ymd("20001101"),]  

ubcosG2W1 <- ubcosG2W1[!ubcosG2W1$int_status1960 %in% c(0,4,6),]  
ubcosG2W2 <- ubcosG2W2[!ubcosG2W2$int_status1970 %in% c(0,4,6),]  
ubcosG2W2 <- ubcosG2W2[ is.na(ubcosG2W2$date_death) | ubcosG2W2$date_death >= ymd("19601101"),]  
ubcosG2W3 <- ubcosG2W3[!ubcosG2W3$int_status1980 %in% c(0,4,6),]  
ubcosG2W3 <- ubcosG2W3[ is.na(ubcosG2W3$date_death) | ubcosG2W3$date_death >= ymd("19701101"),]  
ubcosG2W4 <- ubcosG2W4[!ubcosG2W4$int_status1990 %in% c(0,4,6),]  
ubcosG2W4 <- ubcosG2W4[ is.na(ubcosG2W4$date_death) | ubcosG2W4$date_death >= ymd("19800915"),]  
ubcosG2W5 <- ubcosG2W5[!ubcosG2W5$int_status2000 %in% c(0,4,6),]  
ubcosG2W5 <- ubcosG2W5[ is.na(ubcosG2W5$date_death) | ubcosG2W5$date_death >= ymd("19901101"),]  
ubcosG2W6 <- ubcosG2W6[!ubcosG2W6$int_status2008 %in% c(0,4,6),]
ubcosG2W6 <- ubcosG2W6[ is.na(ubcosG2W6$date_death) | ubcosG2W6$date_death >= ymd("20001101"),]  
