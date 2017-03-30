#' @docType data
"jenkins"

#' @import lubridate
#' @import magrittr
#' @import dplyr
#' @import xts
#' @import zoo
#' @import scales
#' @import forecast
#' @export 

instances <- function(date) {
  data(jenkins)
    
  ts <- jenkins %>% 
     select(datetime, Busy.executors)
  
  t <- xts(ts$Busy.executors, ts$datetime)

  ts.h <- ts(to.hourly(t)$t.High, frequency = 24)
  ts.h

  last_time <- tail(ts$datetime, n=1)
  i <- interval(last_time, ymd_hm(date))
  hours <- as.numeric(i)/3600

  Tbatsfit <- tbats(ts.h)
  summary(Tbatsfit)
  tbatsf <- forecast(Tbatsfit, h=hours)

  as.numeric(tbatsf$mean)
}