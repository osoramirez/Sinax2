#' soundindex

#' @param soundindex Estimates sound index analysis. This function using a loop that include the most common sound ecology index from (soundecolgy v. 1.3.3 and seewave v.2.1.0 package)
#' @usage soundindex()
#' @import tuneR
#' @import seewave
#' @import soundecology
#' @author Oscar Ramirez Alan (\email{osoramirez@@gmail.com})
#' @return return a table with a different soundecology index
#' @export
#' @examples
#' #firt, created a vector call: "time" and define your sequence time
#' #time<-seq(0:3)
#' #soundindex()->filevector #Remember always chance the vector name
#' #filevector #Call the vector, and now you can see all index by channel.
#'
#'
soundindex <- function(){

  df <- data.frame()

  files <- list.files(path = getwd(), pattern = "wav$", ignore.case = T )

  time=time
  minutos<-seq(time)#eat:

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

      ACI <- acoustic_complexity(wav, min_freq=2000,max_freq = 1000)#Acoustic Complexity Index #{soundecology}
      ACI.L <- ACI$AciTotAll_left
      ACI.R <- ACI$AciTotAll_right

      BIO <- bioacoustic_index(wav,min_freq = 2000, max_freq = 8000)#Bioacoustic Index
      BIO.L <-BIO$left_area
      BIO.R <-BIO$right_area

      NDSI <- ndsi(wav) #Normalized Difference Soundscape Index
      NDSI.L<-(NDSI$ndsi_left)
      NDSI.R<-(NDSI$ndsi_right)

      TE<-H(wav) #(Total entropy), total entropy of a time wave.

      envorni<-env(wav,f=22050,plot=FALSE)
      Ht<-th(envorni) # (Temporal entropy), entropy of a temporal envelope. Calculate the temporal Entropy (Ht; Sueur et al. 2008b) by calling the "env" function from the "seewave" package.


      speca<-spec(wav,f=f)
      Hf<-sh(speca) #calculate the frequency Entropy (Hf; Sueur et al. 2008b) by calling the "sh" function from the "seewave" package

      MAE<-M(wav) #Median of amplitude envelope

      spec <- meanspec(wav, plot=F)
      peaks<-fpeaks(spec)
      NP<-length(peaks)/2




      dom.frec<-dfreq(wav, f=22050, wl=512, threshold=0.015, main=paste(files[file], '-', minutos[i], sep=""))#eat:modifique para ajustar el nombre del archivo
      as.data.frame(dom.frec)->dom.frec1
      dm.23<-dom.frec1[dom.frec1$y>2 & dom.frec1$y<=3, ]
      length.fd.23<-length(dm.23$y)
      dm.34<-dom.frec1[dom.frec1$y>3 & dom.frec1$y<=4, ]
      length.fd.34<-length(dm.34$y)
      dm.56<-dom.frec1[dom.frec1$y>5 & dom.frec1$y<=6, ]
      length.fd.56<-length(dm.56$y)

      z <- list(AEI.L = AEI.L,
                AEI.R = AEI.R,
                ADI.L=ADI.L,
                ADI.R=ADI.R,
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
                F23=length.fd.23,  F34=length.fd.34, F56=length.fd.56,
                Technophony=Technophony, BIOAC=Bioc, TB=TB)

      df <- rbind(df, data.frame(z, row.names = make.names(rep(files[file], length(z[[1]])), unique = TRUE)))
    }
  }

  return(df)
}




