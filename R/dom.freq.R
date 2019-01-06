#' dom.freq

#' @param dom.freq Estimates the dominant frequency, using the function in seewave v.2.1.0 package)
#' @usage dom.freq()
#' @import tuneR
#' @import seewave
#' @import soundecology
#' @author Oscar Ramirez Alan (\email{osoramirez@@gmail.com})
#' @return return a table with a different soundecology index
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #dom.freq()->Fdom.freq #The vector "Fdom.freq" call your result
#' #Fdom.freq #Call the vector, and now you can see all index by channel.
#'
#'
dom.freq <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  time=time
  minutos<-seq(time)#eat:

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],#Thank to Dr. Esteban Acevedo-Trejos <acevedoesteban@gmail.com>, who helped to improve this function.
                      units = "minutes",  header = FALSE, toWaveMC = NULL)
      dom.frec<-dfreq(wav, f=22050, wl=512, threshold=0.015, main=paste(files[file], '-', minutos[i], sep=""))#eat:modifique para ajustar el nombre del archivo
      as.data.frame(dom.frec)->dom.frec1

      x <-soundscapespec(wav, plot=FALSE) 	## function call to compute soundscape power in R-seewave(no plots)
      v <- as.vector(x[,2]) 				## soundscape power F 1-2 kHz

      Prom.Dom.frec<-mean(dom.frec1$y, na.rm=TRUE)

      #Frecuencias Dominiantes por Khz
      dm.01<-dom.frec1[dom.frec1$y<=1, ]
      dm.12<-dom.frec1[dom.frec1$y>1 & dom.frec1$y<=2, ]
      dm.23<-dom.frec1[dom.frec1$y>2 & dom.frec1$y<=3, ]
      dm.34<-dom.frec1[dom.frec1$y>3 & dom.frec1$y<=4, ]
      dm.45<-dom.frec1[dom.frec1$y>4 & dom.frec1$y<=5, ]
      dm.56<-dom.frec1[dom.frec1$y>5 & dom.frec1$y<=6, ]
      dm.67<-dom.frec1[dom.frec1$y>6 & dom.frec1$y<=7, ]
      dm.78<-dom.frec1[dom.frec1$y>7 & dom.frec1$y<=8, ]
      dm.89<-dom.frec1[dom.frec1$y>8 & dom.frec1$y<=9, ]
      dm.910<-dom.frec1[dom.frec1$y>9 & dom.frec1$y<=10, ]
      dm.1011<-dom.frec1[dom.frec1$y>10 & dom.frec1$y<=11, ]
      dm.1112<-dom.frec1[dom.frec1$y>11 & dom.frec1$y<=12, ]
      dm.1213<-dom.frec1[dom.frec1$y>12 & dom.frec1$y<=13, ]
      dm.1314<-dom.frec1[dom.frec1$y>13 & dom.frec1$y<=14, ]
      dm.1415<-dom.frec1[dom.frec1$y>14 & dom.frec1$y<=15, ]
      dm.15<-dom.frec1[dom.frec1$y>15, ]

      length.fd.01<-length(dm.01$y)
      length.fd.12<-length(dm.12$y)
      length.fd.23<-length(dm.23$y)
      length.fd.23<-length(dm.23$y)
      length.fd.34<-length(dm.34$y)
      length.fd.45<-length(dm.45$y)
      length.fd.56<-length(dm.56$y)
      length.fd.67<-length(dm.67$y)
      length.fd.78<-length(dm.78$y)
      length.fd.89<-length(dm.89$y)
      length.fd.910<-length(dm.910$y)
      length.fd.910<-length(dm.910$y)
      length.fd.1011<-length(dm.1011$y)
      length.fd.1112<-length(dm.1112$y)
      length.fd.1213<-length(dm.1213$y)
      length.fd.1314<-length(dm.1314$y)
      length.fd.1415<-length(dm.1415$y)
      length.fd.15<-length(dm.15$y)

      # crear una lista que servira como una fila con todas medidas para
      # el archivo corriente
      # los nombres de los elementos de la lista seran los nombres de la
      # columna del data frame
      # crear una lista que servira como una fila con todas medidas para
      # el archivo corriente
      # los nombres de los elementos de la lista seran los nombres de la
      # columna del data frame
      z <- list(Mean=Prom.Dom.frec,
                F01=length.fd.01,
                F12=length.fd.12,
                F23=length.fd.23,
                F34=length.fd.34,
                F45=length.fd.45,
                F56=length.fd.56,
                F67=length.fd.67,
                F78=length.fd.78,
                F89=length.fd.89,
                F910=length.fd.910,
                F1011=length.fd.1011,
                F1112=length.fd.1112,
                F1213=length.fd.1213,
                F1314=length.fd.1314,
                F1415=length.fd.1415,
                F15=length.fd.15)

      #rows <- file
      #df <- rbind(df, z, rows)
      # pegar cada lista o fila al data frame vacio

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
  }

  return(df)
}




