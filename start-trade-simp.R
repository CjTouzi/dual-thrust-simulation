starttradesimp <- function(minutedata, daydata, minutesinday=240, k1=0.5, k2=0.2, startmoney=1000000){
    daydata$hmc = daydata$High - daydata$Close
    daydata$cml = daydata$Close - daydata$Low
    daydata$maxhmccml = (daydata$hmc + daydata$cml + abs(daydata$hmc - daydata$cml)) / 2
    daydata$trigger1 = daydata$maxhmccml * k1
    daydata$trigger2 = daydata$maxhmccml * k2
    print(daydata)

    timevetor = c()
    cashvetor = c()
    stockassetvetor = c()
    allvetor = cashvetor + stockassetvetor

    cash = startmoney
    hands = 0
    stockasset = 0

    for(i in 2:nrow(daydata)){
        trigger1 = as.numeric(daydata$trigger1[i-1])
        trigger2 = as.numeric(daydata$trigger2[i-1])

        for(k in ((i-1)*minutesinday+1):(i*minutesinday)){
            # access this day's minute data
            if(as.numeric(minutedata[k]$Open) > (as.numeric(daydata[i]$Open)+trigger1)){
                # buy
                print('buyyyyyyyyyyyyy!')
                thishands = cash %/% as.numeric(minutedata[k]$Open)
                cash = cash - thishands * as.numeric(minutedata[k]$Open)
                hands = thishands + hands
                stockasset = hands * as.numeric(minutedata[k]$Open)
            } else if(as.numeric(minutedata[k]$Open) < (as.numeric(daydata[i]$Open)-trigger2)){
                # sell
                print('sellllllllllllll!')
                cash = cash + hands * as.numeric(minutedata[k]$Open)
                hands = 0
                stockasset = 0
            } else{
                stockasset = hands * as.numeric(minutedata[k]$Open)
            }

            timevetor = c(timevetor, index(minutedata)[k])
            cashvetor = c(cashvetor, cash)
            stockassetvetor = c(stockassetvetor, stockasset)
            allvetor = c(allvetor, cash+stockasset)
            print(paste('i:', i, ', k:', k, ', cash:', cash, ', stockasset:', stockasset, ', ',index(minutedata)[k] ))
        }
    }

    return(data.frame(time=as.POSIXct(timevetor, origin='1970-01-01', tz='UTC'), cash=cashvetor, stockasset=stockassetvetor, all=allvetor))
}
