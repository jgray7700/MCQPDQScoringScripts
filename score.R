#    Copyright 2017 Joshua Gray
#
#    MCQPDQScoringScripts is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 2 or later.
#
#    MCQPDQScoringScripts is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with MCQPDQScoringScripts.  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.
#

load("Gray.RData")

#' PDQ Scoring algorithm
#'
#' ...
#'
#' @param localPDQdata 
#' @author Josh Gray <jgray7700@gmail.com>, Shawn Gilroy <shawn.gilroy@temple.edu>
#' @return Scored PDQ questionairre
#' @examples
#' res <- scorePDQ(PDQdata)
#' @export
scorePDQ <- function(localPDQdata) {
  pdqLocal = localPDQdata
  
  if (ncol(pdqLocal) != 31) {
    stop("incorrect # of columns. Please lists as IDcol, PDQ1, PDQ2, ...")
  }
  
  allColNames <- colnames(pdqLocal)
  
  for (i in 1:30) {
    tempColName <- paste("PDQ", i, sep = "")
    
    if (tempColName != allColNames[i+1]) {
      stop("Columns are incorrectly named and/or ordered.")
    }
  }  
  
  localPDQdata$PDQ1 <- localPDQdata$PDQ1*1
  localPDQdata$PDQ2 <- localPDQdata$PDQ2*2
  localPDQdata$PDQ3 <- localPDQdata$PDQ3*4
  localPDQdata$PDQ4 <- localPDQdata$PDQ4*8
  localPDQdata$PDQ5 <- localPDQdata$PDQ5*16
  localPDQdata$PDQ6 <- localPDQdata$PDQ6*32
  localPDQdata$PDQ7 <- localPDQdata$PDQ7*64
  localPDQdata$PDQ8 <- localPDQdata$PDQ8*128
  localPDQdata$PDQ9 <- localPDQdata$PDQ9*256
  localPDQdata$PDQ10 <- localPDQdata$PDQ10*512
  localPDQdata$Block1Seq <- with (localPDQdata, PDQ1+PDQ2+PDQ3+PDQ4+PDQ5+PDQ6+PDQ7+PDQ8+PDQ9+PDQ10-1022)
  
  localPDQdata$PDQ11 <- localPDQdata$PDQ11*1
  localPDQdata$PDQ12 <- localPDQdata$PDQ12*2
  localPDQdata$PDQ13 <- localPDQdata$PDQ13*4
  localPDQdata$PDQ14 <- localPDQdata$PDQ14*8
  localPDQdata$PDQ15 <- localPDQdata$PDQ15*16
  localPDQdata$PDQ16 <- localPDQdata$PDQ16*32
  localPDQdata$PDQ17 <- localPDQdata$PDQ17*64
  localPDQdata$PDQ18 <- localPDQdata$PDQ18*128
  localPDQdata$PDQ19 <- localPDQdata$PDQ19*256
  localPDQdata$PDQ20 <- localPDQdata$PDQ20*512
  localPDQdata$Block2Seq <- with (localPDQdata, PDQ11+PDQ12+PDQ13+PDQ14+PDQ15+PDQ16+PDQ17+PDQ18+PDQ19+PDQ20-1022)
  
  localPDQdata$PDQ21 <- localPDQdata$PDQ21*1
  localPDQdata$PDQ22 <- localPDQdata$PDQ22*2
  localPDQdata$PDQ23 <- localPDQdata$PDQ23*4
  localPDQdata$PDQ24 <- localPDQdata$PDQ24*8
  localPDQdata$PDQ25 <- localPDQdata$PDQ25*16
  localPDQdata$PDQ26 <- localPDQdata$PDQ26*32
  localPDQdata$PDQ27 <- localPDQdata$PDQ27*64
  localPDQdata$PDQ28 <- localPDQdata$PDQ28*128
  localPDQdata$PDQ29 <- localPDQdata$PDQ29*256
  localPDQdata$PDQ30 <- localPDQdata$PDQ30*512
  localPDQdata$Block3Seq <- with (localPDQdata, PDQ21+PDQ22+PDQ23+PDQ24+PDQ25+PDQ26+PDQ27+PDQ28+PDQ29+PDQ30-1022)

  #Remove unwanted columns
  localPDQdata[2:31] <- list(NULL)
  
  #Maintain row order
  localPDQdata$id <- 1:nrow(localPDQdata)
  
  #Merge in PDQindices from lookup table
  localPDQdata <- (merge(lookup1PDQ, localPDQdata, by = 'Block1Seq'))
  localPDQdata <- (merge(lookup2PDQ, localPDQdata, by = 'Block2Seq'))
  localPDQdata <- (merge(lookup3PDQ, localPDQdata, by = 'Block3Seq'))
  
  #Return to the original order of rows
  localPDQdata <- localPDQdata[order(localPDQdata$id),]
  
  #Arrange columns in ideal order
  localPDQdata <- localPDQdata[c(13,9,10,11,12,5,6,7,8,1,2,3,4)]
   
  localPDQdata
}

#' MCQ Scoring algorithm
#'
#' ...
#'
#' @param localMCQdata 
#' @author Josh Gray <jgray7700@gmail.com>, Shawn Gilroy <shawn.gilroy@temple.edu>
#' @return Scored MCQ questionairre
#' @examples
#' res <- scoreMCQ(MCQdata)
#' @export
scoreMCQ <- function(localMCQdata) {
  mcqLocal = localMCQdata
  
  if (ncol(mcqLocal) != 28) {
    stop("incorrect # of columns. Please lists as IDcol, PDQ1, PDQ2, ...")
  }
  
  allColNames <- colnames(mcqLocal)
  
  for (i in 1:27) {
    tempColName <- paste("MCQ", i, sep = "")
    
    if (tempColName != allColNames[i+1]) {
      stop("Columns are incorrectly named and/or ordered.")
    }
  }  

  #Calculate unique value for each sequence of responses
  mcqLocal$MCQ13 <- mcqLocal$MCQ13*1
  mcqLocal$MCQ20 <- mcqLocal$MCQ20*2
  mcqLocal$MCQ26 <- mcqLocal$MCQ26*4
  mcqLocal$MCQ22 <- mcqLocal$MCQ22*8
  mcqLocal$MCQ3 <- mcqLocal$MCQ3*16
  mcqLocal$MCQ18 <- mcqLocal$MCQ18*32
  mcqLocal$MCQ5 <- mcqLocal$MCQ5*64
  mcqLocal$MCQ7 <- mcqLocal$MCQ7*128
  mcqLocal$MCQ11 <- mcqLocal$MCQ11*256
  mcqLocal$SmlSeq <- with (mcqLocal, MCQ13+MCQ20+MCQ26+MCQ22+MCQ3+MCQ18+MCQ5+MCQ7+MCQ11-510)
  
  mcqLocal$MCQ1 <- mcqLocal$MCQ1*1
  mcqLocal$MCQ6 <- mcqLocal$MCQ6*2
  mcqLocal$MCQ24 <- mcqLocal$MCQ24*4
  mcqLocal$MCQ16 <- mcqLocal$MCQ16*8
  mcqLocal$MCQ10 <- mcqLocal$MCQ10*16
  mcqLocal$MCQ21 <- mcqLocal$MCQ21*32
  mcqLocal$MCQ14 <- mcqLocal$MCQ14*64
  mcqLocal$MCQ8 <- mcqLocal$MCQ8*128
  mcqLocal$MCQ27 <- mcqLocal$MCQ27*256
  mcqLocal$MedSeq <- with (mcqLocal, MCQ1+MCQ6+MCQ24+MCQ16+MCQ10+MCQ21+MCQ14+MCQ8+MCQ27-510)
  
  mcqLocal$MCQ9 <- mcqLocal$MCQ9*1
  mcqLocal$MCQ17 <- mcqLocal$MCQ17*2
  mcqLocal$MCQ12 <- mcqLocal$MCQ12*4
  mcqLocal$MCQ15 <- mcqLocal$MCQ15*8
  mcqLocal$MCQ2 <- mcqLocal$MCQ2*16
  mcqLocal$MCQ25 <- mcqLocal$MCQ25*32
  mcqLocal$MCQ23 <- mcqLocal$MCQ23*64
  mcqLocal$MCQ19 <- mcqLocal$MCQ19*128
  mcqLocal$MCQ4 <- mcqLocal$MCQ4*256
  mcqLocal$LrgSeq <- with (mcqLocal, MCQ9+MCQ17+MCQ12+MCQ15+MCQ2+MCQ25+MCQ23+MCQ19+MCQ4-510)
  
  #Remove unwanted columns
  mcqLocal[2:28] <- list(NULL)
  
  #Maintain row order
  mcqLocal$id <- 1:nrow(mcqLocal)
  
  #Merge in MCQindices from lookup table
  mcqLocal <- (merge(lookup1MCQ, mcqLocal, by = 'SmlSeq'))
  mcqLocal <- (merge(lookup2MCQ, mcqLocal, by = 'MedSeq'))
  mcqLocal <- (merge(lookup3MCQ, mcqLocal, by = 'LrgSeq'))
  
  #Return to the original order of rows
  mcqLocal <- mcqLocal[order(mcqLocal$id),]

  #Arrange columns in ideal order
  mcqLocal <- mcqLocal[c(13,9,10,11,12,5,6,7,8,1,2,3,4)]

  mcqLocal
}
