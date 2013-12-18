gendaydata <- function(minutedata){
    alldaysdata = data.frame(Date=NULL, Open=NULL, High=NULL, Low=NULL, Close=NULL)

    tmphigh = NULL
    tmplow = NULL
    tmpopen = NULL
    tmpclose = NULL

    for(i in 1:(nrow(minutedata)-1)){
        print(i)

        if(as.Date(index(minutedata)[i])==as.Date(index(minutedata)[i+1])){
            tmpopen = c(tmpopen, minutedata[i]$Open)
            tmphigh = c(tmphigh, minutedata[i]$High)
            tmplow = c(tmplow, minutedata[i]$Low)
        }

        if(as.Date(index(minutedata)[i])!=as.Date(index(minutedata)[i+1])){
            tmphigh = c(tmphigh, minutedata[i]$High)
            tmplow = c(tmplow, minutedata[i]$Low)
            tmpclose = minutedata[i]$Close

            dayhigh = max(tmphigh)
            daylow = min(tmplow)
            dayopen = tmpopen[1]
            dayclose = tmpclose[1]
            daydate = as.character(index(minutedata)[i])
            singledaydata = data.frame(Date=daydate, Open=dayopen, High=dayhigh, Low=daylow, Close=dayclose)
            alldaysdata = rbind(alldaysdata, singledaydata)

            tmphigh = NULL
            tmplow = NULL
            tmpopen = NULL
            tmpclose = NULL
        }

        if(as.Date(index(minutedata)[i])==as.Date(index(minutedata)[i+1]) && i+1==nrow(minutedata)){
            #tmpopen = c(tmpopen, minutedata[i]$Open)  # not needed
            #tmphigh = c(tmphigh, minutedata[i]$High)  # not needed
            #tmplow = c(tmplow, minutedata[i]$Low)  # not needed
            tmpclose = minutedata[i+1]$Close  #tmpclose = minutedata[i]$Close  # changed!!

            dayhigh = max(tmphigh)
            daylow = min(tmplow)
            dayopen = tmpopen[1]
            dayclose = tmpclose[1]
            daydate = as.character(index(minutedata)[i])
            singledaydata = data.frame(Date=daydate, Open=dayopen, High=dayhigh, Low=daylow, Close=dayclose)
            alldaysdata = rbind(alldaysdata, singledaydata)

            tmphigh = NULL
            tmplow = NULL
            tmpopen = NULL
            tmpclose = NULL
        }
    }

    return(alldaysdata)
}
