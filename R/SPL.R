# SPL
#
# This is an example function named 'Sound pressure level'
#' Sound pressure level
#' @param SPL Sound pressure level uses a logarithmic scale to represent the sound pressure of a sound relative to a reference pressure
#' @usage SPL
#' @import tuneR
#' @import seewave
#' @import data.table
#' @import stats
#' @import vegan
#' @return Sound presure level
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #SPL()->SPL #Remember always chance the vector name
#' #SPL

SPL <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  time=time
  minutos<-seq(time)

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],#Thank to Dr. Esteban Acevedo-Trejos <acevedoesteban@gmail.com>, who helped to improve this function.
                      units = "minutes",  header = FALSE, toWaveMC = NULL)
      freq=wav@samp.rate
      spectrum0<-data.table(meanspec(wav,f=freq,norm=F,plot=T))
      SPL_Un<-NULL
      y<-NULL
      SP<- spectrum0[,SPL_Un:=20*log10(y/(2*10e-5))]
      SPL<- as.data.frame(SP)
      SPLm<- data.table(mean(SPL$SPL_Un))
      SPLsd<- data.table(sd(SPL$SPL_Un))
      SPLQ25<- data.table(quantile(SPL$SPL_Un, .25))
      SPLMe<- data.table(median(SPL$SPL_Un))
      SPLQ75<- data.table(quantile(SPL$SPL_Un, .75))
      SPLMax<- data.table(max(SPL$SPL_Un))
      SPLMin<- data.table(min(SPL$SPL_Un))
      ttest<- t.test(SPL$SP)
      lwr.ci <- ttest$conf.int[1]
      upr.ci <- ttest$conf.int[2]
      z <- list( SPLm=SPLm, SPLsd=SPLsd,SPLQ25=SPLQ25, SPLMe=SPLMe, SPLQ75=SPLQ75,
                 SPLMax=SPLMax, SPLMin=SPLMin, lwr.ci=lwr.ci,upr.ci=upr.ci )

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))

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
  return(df)
}
