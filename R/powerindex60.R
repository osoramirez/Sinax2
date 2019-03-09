#' Estimate power spectral density (PSD) in 60 min intervals
#'
#' @param powerindex60 Estimates the power spectral analysis of your sound file in dB by frequency of 1kHz.
#' @usage powerindex60()
#' @return return a table with a different power spectral index
#' @export
#' @examples
#' #This code analyzes for a 60-minutes intervals
#' #powerindex60()->powerindex60min #Vector that contain your results
#' #powerindex60min #Call the vector, and see your results.
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

powerindex60 <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  minutos<-seq(0:58)# 1-minute interval

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],
                      units = "minutes",  header = FALSE, toWaveMC = NULL)

      ## Calculate Soundscape Power and associated statistics
      x <-soundscapespec(wav, plot=TRUE, main=files[file]) 	## function call to compute soundscape power in R-seewave
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

      finf <- data.table(file.info(dir(getwd()), extra_cols = F))
      finft<- data.table(finf$mtime[file=file])

      z <- list(Pow.12=Pow.12,Pow.23=Pow.23,
                Pow.34=Pow.34,Pow.45=Pow.45,
                Pow.56=Pow.56,Pow.67=Pow.67,
                Pow.78=Pow.78,Pow.89=Pow.89,
                Pow.910=Pow.910,Pow.1011=Pow.1011,
                FSUM=FSUM, FMEAN=FMEAN,
                Technophony=Technophony, Biophony=Biophony,
                DateTime=finft)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
  }
  names(df)[15]<-paste("Date-Time")
  return(df)
}

