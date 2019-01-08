# evaluation
#
# This is an example function named 'evaluation'
#' evaluation!
#' @param evaluation Estimates a evaluation of insect and rain
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

      Bioc <-x[11,2] 			## 10-11 kHz

      Technophony <- v[1]						## Technophony

      TB<-(Technophony/Bioc)


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
                Pow.12=Pow.12,
                Technophony=Technophony, BIOAC=Bioc, TB=TB)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))

    }
  }

  df <- decostand(df, method="hellinger", na.rm = FALSE)

  df$Decision_insect<-ifelse(df$criterion2<900 & df$criterion4<5000 &df$criterion1<0.15& df$criterion3<3650,
                             "Look good","Probably with insects")

  df$Decision_rain<-ifelse(df$Pow.12>0.35 & df$TB>8, "Probably with Rain", "Look Clear")

  df$Decision_manualy<-ifelse(df$criterion2<900 &df$criterion4<5000 &df$criterion1<0.15& df$criterion3<3650
                              & df$Pow.12>.03 & df$Pow.12<.18, "Good","check manually")


  return(df)
}
