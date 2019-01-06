#' powerindex


#' @param powerindex Estimates the power spectral analysis of your sound file in dB by frequency of 1kHz.
#' @usage powerindex()
#' @author Oscar Ramirez Alan (\email{osoramirez@@gmail.com})
#' @return return a table with a different power spectral index
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #powerindex()->powervector #Remember always chance the vector name
#' #powervector #Call the vector, and now you can see all index by channel.
#'


powerindex <- function(){

  df <- data.frame()

  # creando una lista de los archivos .wav en el directorio de trabajo
  # esto sera la lista donde el loop tomara el indice de iteracion
  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )
  #rows <- c()

  time=time
  minutos<-seq(time)#eat

  # esto va a calcular los indices acusticos para cada archivo
  # usar 1:length() crea un indice numerico
  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){ #eat:loop para cada intervalo de minutos

      # leer cada archivo .wav en cada iteracion de 1 a lo largo de files
      # eat: en lnea 129 modifique los intervalos de los minutos
      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes",  header = FALSE, toWaveMC = NULL)

      ## Calculate Soundscape Power and associated statistics
      x <-soundscapespec(wav, plot=T) 	## function call to compute soundscape power in R-seewave(no plots)
      FSUM <- colSums(x) 					## soundscape power sum
      FSUM<-FSUM[2:2]
      FMEAN <- colMeans(x) 				## soundscape power mean
      FMEAN <- FMEAN[2:2]
      v1 <- as.vector(x[,2]) 				## soundscape power F 1-2 kHz

      Biophony <- colSums(x)-v1[1]				## Biophony
      Technophony <- v1[1]						## Technophony

      Pow.12 <-  x[1,2] 				## 1-2 kHz
      Pow.12 <- as.data.frame(Pow.12)
      Pow.23 <- x[2,2] 				## 2-3 kHz
      Pow.23 <- as.data.frame(Pow.23)
      Pow.34 <- x[3,2] 				## 3-4 kHz
      Pow.34 <- as.data.frame(Pow.34)
      Pow.45 <- x[4,2] 				## 4-5 kHz
      Pow.45 <- as.data.frame(Pow.45)
      Pow.56 <- x[5,2] 				## 5-6 kHz
      Pow.56 <- as.data.frame(Pow.56)
      Pow.67 <- x[6,2] 				## 6-7 kHz
      Pow.67 <- as.data.frame(Pow.67)
      Pow.78 <- x[7,2] 				## 7-8 kHz
      Pow.78 <- as.data.frame(Pow.78)
      Pow.89 <- x[8,2] 				## 8-9 kHz
      Pow.89 <- as.data.frame(Pow.89)
      Pow.910 <-x[9,2] 				## 9-10 kHz
      Pow.910 <- as.data.frame(Pow.910)
      Pow.1011 <-x[10,2] 			## 10-11 kHz
      Pow.1011 <-as.data.frame(Pow.1011)

      Biophony <-x[11,2] 			## 10-11 kHz
      Biophony <-as.data.frame(Biophony)

      z <- list(Pow.12=Pow.12,Pow.23=Pow.23,
                Pow.34=Pow.34,Pow.45=Pow.45,
                Pow.56=Pow.56,Pow.67=Pow.67,
                Pow.78=Pow.78,Pow.89=Pow.89,
                Pow.910=Pow.910,Pow.1011=Pow.1011,
                FSUM=FSUM, FMEAN=FMEAN,
                Technophony=Technophony, Biophony=Biophony)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
  }

  return(df)
}




