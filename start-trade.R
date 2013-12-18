starttrade <- function(minutedata, daydata, minutesinday=240, k1=0.5, k2=0.2, startmoney=1000000, borrowed_rate = 0.5){
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
    borrowed_money = startmoney * borrowed_rate
    borrowed_hands = 0
    has_borrowed = FALSE

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
                hands = thishands + hands - borrowed_hands
                stockasset = hands * as.numeric(minutedata[k]$Open)
                borrowed_hands = 0
                has_borrowed = FALSE
            } else if(as.numeric(minutedata[k]$Open) < (as.numeric(daydata[i]$Open)-trigger2)){
                # sell
                print('sellllllllllllll!')
                if(!has_borrowed){
                    borrowed_hands_this_time = borrowed_money %/% as.numeric(minutedata[k]$Open)
                    has_borrowed = TRUE
                } else{
                    borrowed_hands_this_time = 0
                }
                borrowed_hands = borrowed_hands + borrowed_hands_this_time
                cash = cash + (borrowed_hands_this_time + hands) * as.numeric(minutedata[k]$Open)
                hands = 0
                stockasset = 0
            } else{
                stockasset = hands * as.numeric(minutedata[k]$Open)
            }

            #print(borrowed_hands*as.numeric(minutedata[k]$Open))
            #print(borrowed_hands)
            #print(cash)
            #print(cash-borrowed_hands*as.numeric(minutedata[k]$Open))
            #print(as.numeric(minutedata[k]$Open))
            realcash = cash-borrowed_hands*as.numeric(minutedata[k]$Open)
            timevetor = c(timevetor, index(minutedata)[k])
            cashvetor = c(cashvetor, realcash)
            stockassetvetor = c(stockassetvetor, stockasset)
            allvetor = c(allvetor, realcash+stockasset)
            print(paste('i: ', i, '  k: ', k, '  realcash: ', realcash, '  stockasset: ', stockasset, '  ',index(minutedata)[k] ))
        }
    }

    return(data.frame(time=as.POSIXct(timevetor, origin='1970-01-01', tz='UTC'), realcash=cashvetor, stockasset=stockassetvetor, all=allvetor))
}
