# evaluation
#
# This is an example function named 'evaluation'
#' evaluation!
#' @param evaluation Estimates a evaluation of insect and rain
#' @import tuneR
#' @import seewave
#' @import soundecology
#' @import vegan
#' @return a evaluation of rain and insects
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #evaluation()->evaluation #Remember always chance the vector name
#' #evaluation

evaluation <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  time=time
  minutos<-seq(time)

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],#Thank to Dr. Esteban Acevedo-Trejos <acevedoesteban@gmail.com>, who helped to improve this function.
                      units = "minutes",  header = FALSE, toWaveMC = NULL)
      x <-soundscapespec(wav, plot=TRUE) 	## function call to compute soundscape power in R-seewave(no plots)
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
      BiocC28


      Technophony <- v[1]						## Technophony

      TB<-(Technophony/BiocC28)



      MAE<-M(wav) #Median of amplitude envelope


      dom.frec<-dfreq(wav, f=22050, wl=512, threshold=0.015, main=paste(files[file], '-', minutos[i], sep=""))#eat:modifique para ajustar el nombre del archivo
      as.data.frame(dom.frec)->dom.frec1
      dm.23<-dom.frec1[dom.frec1$y>2 & dom.frec1$y<=3, ]
      length.fd.23<-length(dm.23$y)
      dm.34<-dom.frec1[dom.frec1$y>3 & dom.frec1$y<=4, ]
      length.fd.34<-length(dm.34$y)
      dm.56<-dom.frec1[dom.frec1$y>5 & dom.frec1$y<=6, ]
      length.fd.56<-length(dm.56$y)

      z <- list(criterion1=MAE,
                criterion2=length.fd.23,  criterion3=length.fd.34, criterion4=length.fd.56,
                Pow.12=Pow.12,TB=TB,
                Technophony=Technophony,
                BiocC28=BiocC28,
                Pow02Mean=Pow02Mean,Pow02Sum=Pow02Sum,
                PowC910Mean=PowC910Mean, Pow910Sum=Pow910Sum)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))

    }
  }

  df <- decostand(df, method="hellinger", na.rm = FALSE)

  df$Decision_rain<-ifelse(df$Pow.12<0.40 & df$PowC910<0.09 &  df$criterion4>0.01 &
                             df$criterion3<0.8, "Probably with Rain", "Looks Clear")

  return(df)
}
