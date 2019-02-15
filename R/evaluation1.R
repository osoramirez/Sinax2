#' Estimate if your file contain rain and insects for 1 min intervals
#
#' @param evaluation1 Estimates a evaluation of insect and rain
#' @import tuneR
#' @import seewave
#' @import soundecology
#' @import vegan
#' @return a evaluation of rain and insects
#' @export
#' @examples
#' #This code analyzes for a 1-minute interval
#' #evaluation1()->evaluation1min #Vector that contain your results
#' #evaluation1min #Call the vector, and see your results.
#'
#'
#'
#' @author Oscar Ramírez Alán (\email{osoramirez@@gmail.com}). Implements a
#' loops using base function from seewave and soundecology.
#'
#' @references {
#' Luis J. Villanueva-Rivera and Bryan C. Pijanowski (2018). soundecology: Soundscape Ecology. R package version 1.3.3. https://CRAN.R-project.org/package=soundecology.
#'
#' Sueur, J., Aubin, T., & Simonis, C. (2008). Seewave, a free modular tool for sound analysis and synthesis. Bioacoustics, 18(2), 213-226.
#'
#' Uwe Ligges, Sebastian Krey, Olaf Mersmann, and Sarah Schnackenberg (2018). tuneR: Analysis of Music and Speech. URL: https://CRAN.R-project.org/package=tuneR.
#' }
#'

evaluation1 <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )
  minutos<-seq(0.1:0.60)# 60-minute interval

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes",  header = FALSE, toWaveMC = NULL)

      x <-soundscapespec(wav, plot=TRUE, main=files[file]) 	## function call to compute soundscape power in R-seewave
      v <- as.vector(x[,2]) 				## soundscape power F 1-2 kHz

      Pow.12 <-  x[1,2] 				## 1-2 kHz
      Pow.12 <- as.data.frame(Pow.12)

      Pow02  <-x[c(0:2),2] 	## 0-2 kHz
      Pow02Sum <- sum(Pow02)
      Pow02Mean<- mean(Pow02)

      Pow910  <-x[c(9,10),2]  			## 9-10 kHz
      Pow910Sum <- sum(Pow910)
      PowC910Mean <-mean(Pow910)


      BiocC <-x[c(2,3,4,5,6,7,8),2] 			## 2-8 kHz
      BiocC28 <-as.data.frame(BiocC)
      BiocC28 <- mean(BiocC28$BiocC)

      Technophony <- v[1]						## Technophony

      TB<-(Technophony/BiocC28)

      MAE<-M(wav) #Median of amplitude envelope

      freq<-wav@samp.rate
      spectrum0<-data.table(meanspec(wav,f=freq,norm=F,plot=F))
      SPL_Un<-NULL
      x<-NULL
      y<-NULL
      spectrum0=data.table(meanspec(wav,f=freq,norm=F,plot=T,  main=files[file]))
      spectrum0[,SPL_Un:=20*log10(y/(2*10e-5))]
      SPLm<- data.table(mean(spectrum0$SPL_Un))


      dom.frec<-dfreq(wav, f=22050, wl=512, threshold=0.015, main=paste(files[file], '-', minutos[i], sep=""))#eat:modifique para ajustar el nombre del archivo
      as.data.frame(dom.frec)->dom.frec1
      dm.12<-dom.frec1[dom.frec1$y>1 & dom.frec1$y<=2, ]
      length.fd.12<-length(dm.12$y)
      dm.23<-dom.frec1[dom.frec1$y>2 & dom.frec1$y<=3, ]
      length.fd.23<-length(dm.23$y)
      dm.34<-dom.frec1[dom.frec1$y>3 & dom.frec1$y<=4, ]
      length.fd.34<-length(dm.34$y)
      dm.56<-dom.frec1[dom.frec1$y>5 & dom.frec1$y<=6, ]
      length.fd.56<-length(dm.56$y)

      z <- list( SPLm=SPLm,
                 criterion1=MAE,  criterion2=length.fd.12,
                 criterion3=length.fd.23,  criterion4=length.fd.34, criterion5=length.fd.56,
                 Pow.12=Pow.12,TB=TB,
                 Technophony=Technophony,
                 BiocC28=BiocC28,
                 Pow02Mean=Pow02Mean,Pow02Sum=Pow02Sum,
                 PowC910Mean=PowC910Mean, Pow910Sum=Pow910Sum)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))

    }
  }

  df <- decostand(df, method="hellinger", na.rm = FALSE)
  df$Raindetector <- df$BiocC28[]/df$TB[]
  names(df)[1]<-paste("SPLmean")
  return(df)

}
