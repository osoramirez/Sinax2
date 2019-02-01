# spl
#
# This is an example function named 'Sound pressure level'
#' Sound pressure level
#' @param spl Sound pressure level uses a logarithmic scale to represent the sound pressure of a sound relative to a reference pressure
#' @usage spl
#' @import tuneR
#' @import seewave
#' @import data.table
#' @import stats
#' @return Sound presure level
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #spl()->spl1 #Remember always chance the vector name
#' #spl1
#'

spl <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )
  time <- time
  minutos<-seq(time)#eat: limites de los minutos que quieres probar

  # esto va a calcular los indices acusticos para cada archivo
  # usar 1:length() crea un indice numerico
  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){ #eat:loop para cada intervalo de minutos

      # leer cada archivo .wav en cada iteracion de 1 a lo largo de files
      # eat: en lnea 129 modifique los intervalos de los minutos
      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes",  header = FALSE, toWaveMC = NULL)

      freq<-wav@samp.rate
      spectrum0<-data.table(meanspec(wav[],f=freq,norm=F,plot=T, main=files[file]))
      SPL_Un<-NULL
      y<-NULL
      SPLd<- as.data.frame(spectrum0[,SPL_Un:=20*log10(y/(2*10e-5))]) #uncalibrated
      SPLm<- as.data.frame(mean(SPLd$SPL_Un))
      SPLsd<- as.data.frame(sd(SPLd$SPL_Un))
      SPLQ25<- as.data.frame(quantile(SPLd$SPL_Un, .25))
      SPLMe<- as.data.frame(median(SPLd$SPL_Un))
      SPLQ75<- as.data.frame(quantile(SPLd$SPL_Un, .75))
      SPLMax<- as.data.frame(max(SPLd$SPL_Un))
      SPLMin<- as.data.frame(min(SPLd$SPL_Un))
      ttest<- t.test(SPLd$SP)
      lwrci <- ttest$conf.int[1]
      uprci <- ttest$conf.int[2]


      z <- list(SPLm=SPLm, SPLsd=SPLsd,SPLQ25=SPLQ25, SPLMe=SPLMe, SPLQ75=SPLQ75,
                SPLMax=SPLMax, SPLMin=SPLMin, lwrci=lwrci,uprci=uprci)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file]), unique = TRUE)))

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
