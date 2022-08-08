# first clear memory with variables from earlier programms / runs

rm(list = ls())

# load used libraries

# ####################
# Initialize variables
# ####################

filenameBasket <- paste("K:\\RANDD\\99-Misc\\Bastiaan\\R-programms\\PCA_index_subbasket_selection\\AEX_basket.csv",sep="")
cIndexNames <- scan (file=filenameBasket,what=list("",""),sep=",",quiet=TRUE)


iNumberOfStocks<-(length(cIndexNames[[1]]))                         #
numberOfObservations <- numeric (iNumberOfStocks)                   #

prices <- matrix(numeric(iNumberOfStocks * 5000),iNumberOfStocks)   #
dates  <- matrix(character(iNumberOfStocks * 5000),iNumberOfStocks) #
pricesIndex <- matrix(numeric(5000),1)                              #
datesIndex  <- matrix(character(5000),1)                            #

allStocks<-list("","")

print (iNumberOfStocks)
print ("Start initializing")

# ####################
# Read stock prices
# ####################

for (i in 1:iNumberOfStocks)
{

    stock1 <- cIndexNames[[1]][[i]]
    filename <- paste("C:\\stoxx600_daily_quotes\\",cIndexNames[[1]][[i]]," Equity",".txt",sep="")

    x <- scan(file=filename,what=list("",""),sep=",",quiet=TRUE)

    numberOfObservations[i] <- length(x[[1]])
    for (j in 1:length(x[[1]]))
    {
      dates[i,j] <- x[[1]][[j]]
      prices[i,j] <- type.convert(x[[2]][[j]], dec=".")
    }
        
}

# ####################
# Read index prices
# ####################

    filename <- paste("C:\\stoxx600_daily_quotes\\AEX Index",".txt",sep="")

    x <- scan(file=filename,what=list("",""),sep=",",quiet=TRUE)

    numberOfObservations[i] <- length(x[[1]])
    for (j in 1:length(x[[1]]))
    {
      datesIndex[1,j] <- x[[1]][[j]]
      pricesIndex[1,j] <- type.convert(x[[2]][[j]], dec=".")
    }


observationsSync <- min(numberOfObservations)  # use min of 3rd to sync data

for (i in 1:iNumberOfStocks)
{
    if (numberOfObservations[i] ==  observationsSync)
    {
             syncStock=i
             startDate=dates[i,1]    # first data from which all underlyings have data
    }
}

     ################################################
     #                                              #
     #             MAIN                             #
     #                                              #
     ################################################

print ("Start with MAIN")

syncPrices <- matrix(numeric(iNumberOfStocks * observationsSync),iNumberOfStocks)
syncDates <-  matrix(numeric(iNumberOfStocks * observationsSync),iNumberOfStocks)
syncIndexPrices <- numeric(observationsSync)             #
syncIndexDates <- numeric(iNumberOfStocks)              #
syncBasketWeights <- numeric(iNumberOfStocks)
syncBasketNames <- character(iNumberOfStocks)
HistBasketPrices <- numeric(observationsSync)            # Used to reconstruct basket

#  #################################################################
#    Read SYNC data. This data has been generated with C# programm.
#  #################################################################

counter <-1

for (i in 1:iNumberOfStocks)
{

      print (i)
      print (cIndexNames[[1]][[i]])
      print (cIndexNames[[2]][[i]])

      stock1 <- cIndexNames[[1]][[i]]
      filename <- paste("c:\\syncAEX\\_BOOK_",cIndexNames[[1]][[i]],"_.csv",sep="")      #sync'ed data, synchronized with C#

      x <- scan(file=filename,what=list("","",""),sep=",",skip=1,quiet=TRUE)

      numberOfObservations[i] <- length(x[[1]])
      for (j in 1:length(x[[1]]))
      {
        syncDates[counter,j] <- x[[2]][[j]]
        syncPrices[counter,j] <-type.convert(x[[3]][[j]], dec=".")
      }

      syncBasketWeights[counter] <- type.convert(cIndexNames[[2]][[i]], dec=".")
      syncBasketNames[counter] <- cIndexNames[[1]][[i]]
      counter <- counter + 1

}

#  #################################################################
#    Seperately sync Index data
#  #################################################################

i <- 1
while ((i < length(datesIndex)) && (datesIndex[[i]] != startDate))
{
      i<-i+1
}

syncIndexDates <-  datesIndex[i:(i+observationsSync-1)]
syncIndexPrices <- pricesIndex[i:(i+observationsSync-1)]

#  #################################################################
#    calculate returns of all underlyings
#  #################################################################


syncReturns <- matrix(numeric((iNumberOfStocks) * (observationsSync-1)),iNumberOfStocks)
syncIndexReturns <- matrix(numeric((observationsSync-1)),1)

for (i in 1: dim(syncPrices)[1])
{
    print (i)
      for (j in 1:(observationsSync-1))
      {
        syncReturns[i,j] <-  syncBasketWeights[i]*log(syncPrices[i,(j+1)]/syncPrices[i,j])
        HistBasketPrices[j] <-HistBasketPrices[j] + syncBasketWeights[i]*syncPrices[i,j]
      }
      HistBasketPrices[observationsSync] <-HistBasketPrices[observationsSync] + syncBasketWeights[i]*syncPrices[i,observationsSync]
}

#  #################################################################
#    calculate returns of index
#  #################################################################

      for (j in 1:(observationsSync-1))
      {
        syncIndexReturns[1,j] <-  log(syncIndexPrices[(j+1)]/syncIndexPrices[j])
      }

#  #################################################################
#    calculate correlation matrix of returns
#  #################################################################


correlationMatrix <- cor(t(syncReturns))
covarianceMatrix <- cov(t(syncReturns))
varianceMatrix <-var(t(syncReturns))


#  #################################################################
#    calculate priciniple component analysis
#  #################################################################


PCAresults <- prcomp(t(syncReturns))

stdevPCA <- PCAresults$sdev

print(summary(PCAresults))

PCAcomponents <-scale(t(syncReturns),center = PCAresults$center, scale = PCAresults$scale) %*% t(PCAresults$rotation)

rr <- cor(PCAcomponents,t(syncReturns))

rr <- rr * rr
selection <- numeric(dim(rr)[1])

selectedBasket <- numeric(10)
noSelectedStocks <- 1

syncBasketPrices <- matrix(numeric(observationsSync),1)
syncBasketDates <- matrix(numeric(observationsSync),1)
syncBasketReturns <- matrix(numeric((observationsSync-1)),1)

filenameSUBBasket <- paste("K:\\RANDD\\99-Misc\\Bastiaan\\R-programms\\PCA_index_subbasket_selection\\AEX_subbasket.csv",sep="")

cat ("underlying,weight",file=filenameSUBBasket,sep="",append=FALSE)
cat ("\n",file=filenameSUBBasket,sep="",append=TRUE)

noStocksinSubBasket <- 0

for (i in 1:dim(rr)[1])
{
    selection[i] <- sum(rr[i,1:8])
    if (selection[i] > 0)
    {
       print (i)
       print (selection[i])
       print (cIndexNames[[1]][i])
       noStocksinSubBasket <- noStocksinSubBasket+1
       selectedBasket[noSelectedStocks] <- i
       noSelectedStocks <- noSelectedStocks + 1
       cat (selection[i],cIndexNames[[1]][i],file=filenameSUBBasket,sep=",",append=TRUE)
       cat ("\n",file=filenameSUBBasket,sep="",append=TRUE)

       for (j in 1:observationsSync)
       {
         syncBasketPrices[1,j] <- syncBasketPrices[1,j] + syncBasketWeights[i]*syncPrices[i,j]
       }
       
    }
}

print ("write results to disk")

for (j in 1:observationsSync)
{
    cat (syncBasketDates[1,j],file=filenamePricesSUBBasket,sep=",",append=TRUE)
    cat (syncBasketPrices[1,j],file=filenamePricesSUBBasket,sep=",",append=TRUE)
    cat ("\n",file=filenamePricesSUBBasket,sep="",append=TRUE)
}


# multiplier <- syncIndexPrices[1]/syncBasketPrices[1,1]
multiplier <- 1

syncBasketPrices <- syncBasketPrices * multiplier

MinY <- min(syncBasketPrices[1,],syncIndexPrices)         # determine boundaries for the graph  -> y axis
MaxY <- max(syncBasketPrices[1,],syncIndexPrices)

par (new=F)
plot ( syncBasketPrices[1,],type="l",col="red",ylim=c(MinY,MaxY),ylab="index / subbasket",xlab="T")
par (new=T)
plot ( syncIndexPrices,type="l",ylim=c(MinY,MaxY),ylab="",xlab="T")
par (new=T)
plot ( HistBasketPrices,type="p",col="blue",ylim=c(MinY,MaxY),ylab="",xlab="T")




