#' Sound pressure level at 6 min interval

#' @param spl6 Sound pressure level uses a logarithmic scale to represent the sound pressure of a sound relative to a reference pressure
#' @usage spl6
#' @import tuneR
#' @import seewave
#' @import data.table
#' @import stats
#' @return Sound presure level
#' @export
#' @examples
#' #This code analyzes for a 1-minute interval
#' #spl6()->spl6min #Remember always chance the vector name
#' #spl6min #Call the vector, and see your results.
#'
#' @author Oscar Ramírez (\email{osoramirez@@gmail.com}). Implements a
#' loops using base function from seewave and soundecology.
#'
#' @references  {
#' Uwe Ligges, Sebastian Krey, Olaf Mersmann, and Sarah Schnackenberg (2018). tuneR: Analysis of Music and Speech. URL: https://CRAN.R-project.org/package=tuneR
#' }
#'

spl6 <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )
  minutos<-seq(0:5)# 6-minute interval

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes",  header = FALSE, toWaveMC = NULL)
      freq<-wav@samp.rate
      spectrum0<-data.table(meanspec(wav,f=freq,norm=F,plot=T, main=files[file]))
      SPL_Un<-NULL
      x<-NULL
      y<-NULL
      spectrum0=data.table(meanspec(wav,f=freq,norm=F,plot=T))
      spectrum0[,SPL_Un:=20*log10(y/(2*10e-5))]
      SPLm<- data.table(mean(spectrum0$SPL_Un))
      SPLsd<- data.table(sd(spectrum0$SPL_Un))
      SPLQ25<- data.table(quantile(spectrum0$SPL_Un, .25))
      SPLMe<- data.table(median(spectrum0$SPL_Un))
      SPLQ75<- data.table(quantile(spectrum0$SPL_Un, .75))
      SPLMax<- data.table(max(spectrum0$SPL_Un))
      SPLMin<- data.table(min(spectrum0$SPL_Un))
      ttest<- t.test(spectrum0$SP)
      lwrci <- ttest$conf.int[1]
      uprci <- ttest$conf.int[2]
      finf <- data.table(file.info(dir(getwd()), extra_cols = F))
      finft<- data.table(finf$mtime[file=file])

      z <- list(SPLm=SPLm, SPLsd=SPLsd,SPLQ25=SPLQ25, SPLMe=SPLMe, SPLQ75=SPLQ75,
                SPLMax=SPLMax, SPLMin=SPLMin, lwrci=lwrci,uprci=uprci,finft=finft)

      df <- rbind(df[], data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))

    }
  }

  names(df)[1]<-paste("SPLmean")
  names(df)[2]<-paste("SPLsd")
  names(df)[3]<-paste("SPLQ25")
  names(df)[4]<-paste("SPL50")
  names(df)[5]<-paste("SPLQ75")
  names(df)[6]<-paste("SPLMax")
  names(df)[7]<-paste("SPLMin")
  names(df)[8]<-paste("lwr.ci(95%)")
  names(df)[9]<-paste("upr.ci(95%)")
  names(df)[10]<-paste("Date-Time")


  return(df)
}
