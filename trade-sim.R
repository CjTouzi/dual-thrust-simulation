library('lubridate')
library('zoo')

a = read.csv('min1_sh000300.csv')
head(a)

minutedata = read.zoo(a[,3:9], FUN=ymd_hms)
head(minutedata)

daydata = gendaydata(minutedata)
daydata = as.zoo(daydata[,2:5], as.Date(daydata[,1]))
save(daydata, file='saved-daydata.RData')
