#' Estimates sound index analysis for 1 min intervals

#' @param soundindex1 Estimates sound index analysis. This function using a loop that include the most common sound ecology index from (soundecolgy v. 1.3.3 and seewave v.2.1.0 package)
#' @usage soundindex1()
#' @import tuneR
#' @import seewave
#' @import soundecology
#' @return return a table with a different soundecology index
#' @export
#' @examples
#' #This code analyzes for a 1-minutes intervals
#' #soundindex1()->soundindex1min #Vector that contain your results
#' #soundindex1min #Call the vector, and see your results.
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
#'
soundindex1 <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  minutos<-seq(0.1:0.60)#eat:

  for(file in 1:length(files)){

    for(i in 1:(length(minutos))){

      wav <- readWave(files[file], from = minutos[i]-1, to = minutos[i],#Thank to Dr. Esteban Acevedo-Trejos <acevedoesteban@gmail.com>, who helped to improve this function.
                      units = "minutes",  header = FALSE, toWaveMC = NULL)
      x <-soundscapespec(wav, plot=FALSE) 	## function call to compute soundscape power in R-seewave(no plots)
      v <- as.vector(x[,2]) 				## soundscape power F 1-2 kHz

      f=11000

      Bioph <- colSums(x)-v[1]				## Biophony
      Technophony <- v[1]						## Technophony

      Bioc <-x[11,2] 			## 10-11 kHz

      TB<-(Technophony/Bioc)

      AEI <- acoustic_evenness(wav,max_freq = 10000)#Acoustic Evenness Index  {soundecology}
      AEI.L <- AEI$aei_left
      AEI.R <- AEI$aei_right

      ADI <- acoustic_diversity(wav, max_freq = 10000) #Acoustic Diversity Index {soundecology}
      ADI.L <- ADI$adi_left
      ADI.R <- ADI$adi_right

      ACIsee <-ACI(wav) #Acoustic Complexity Index #{seewave}

      ACI <- acoustic_complexity(wav, min_freq=2000,max_freq = 11000)#Acoustic Complexity Index #{soundecology}
      ACI.L <- ACI$AciTotAll_left
      ACI.R <- ACI$AciTotAll_right

      BIO <- bioacoustic_index(wav,min_freq = 2000, max_freq = 8000)#Bioacoustic Index
      BIO.L <-BIO$left_area
      BIO.R <-BIO$right_area

      NDSI <- ndsi(wav) #Normalized Difference Soundscape Index {soundecology}
      NDSI.L<-(NDSI$ndsi_left)
      NDSI.R<-(NDSI$ndsi_right)

      TE<-H(wav) #(Total entropy), total entropy of a time wave. #{seewave}

      envorni<-env(wav,f=22050,plot=FALSE)
      Ht<-th(envorni) # (Temporal entropy), entropy of a temporal envelope. Calculate the temporal Entropy (Ht; Sueur et al. 2008b) by calling the "env" function from the "seewave" package.


      speca<-spec(wav,f=f) #{seewave}
      Hf<-sh(speca) #calculate the frequency Entropy (Hf; Sueur et al. 2008b) by calling the "sh" function from the "seewave" package

      MAE<-M(wav) #Median of amplitude envelope #{seewave}

      spec <- meanspec(wav, plot=F) #{seewave}
      peaks<-fpeaks(spec)
      NP<-length(peaks)/2

      z <- list(AEI.L = AEI.L,
                AEI.R = AEI.R,
                ADI.L=ADI.L,
                ADI.R=ADI.R,
                ACIsee=ACIsee,
                ACI.L=ACI.L,
                ACI.R=ACI.R,
                BIO.L=BIO.L,
                BIO.R=BIO.R,
                NDSI.L=NDSI.L,
                NDSI.R=NDSI.R,
                TE=TE,
                Ht=Ht,
                Hf=Hf,
                MAE=MAE,
                NP=NP,
                Technophony=Technophony, BIOAC=Bioc, TB=TB)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
  }

  return(df)
}




