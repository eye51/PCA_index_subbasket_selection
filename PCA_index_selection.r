help# first clear memory with variables from earlier programms / runs

rm(list = ls())

# load used libraries

# ####################
# Initialize variables
# ####################

filenameAEXBasket <- paste("K:\\RANDD\\99-Misc\\Bastiaan\\R-programms\\PCA_index_subbasket_selection\\AEX_basket.txt",sep="")
cIndexNames <- scan (file=filenameAEXBasket,what=list("",""),sep=",",quiet=TRUE)

noOfStocksToExclude <- 2                                            # two stocks will be excluded because not enough data is available

iNumberOfStocks<-(length(cIndexNames[[1]]))                         #
numberOfObservations <- numeric (iNumberOfStocks)                   #
excludedStocks <- numeric (iNumberOfStocks)                         #
observationsExcludedStocks <- numeric (iNumberOfStocks)             #

prices <- matrix(numeric(iNumberOfStocks * 4000),iNumberOfStocks)   #
dates  <- matrix(character(iNumberOfStocks * 4000),iNumberOfStocks) #
allStocks<-list("","")

print (iNumberOfStocks)

# ####################
# Read stock prices
# ####################

print ("Start initializing")

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


for (j in 1:2)       # exclude 2 stocks because of small number of observations, use min of 3rd to sync data
{
  observationsExcludedStocks[j] <- min(numberOfObservations)

  for (i in 1:iNumberOfStocks)
  {
      if (numberOfObservations[i] ==  observationsExcludedStocks[j])
      {
               numberOfObservations[i]=9999
               excludedStocks[j]=i
      }
  }
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

syncPrices <- matrix(numeric((iNumberOfStocks-noOfStocksToExclude) * observationsSync),(iNumberOfStocks-noOfStocksToExclude))
syncDates <-  matrix(numeric((iNumberOfStocks-noOfStocksToExclude) * observationsSync),(iNumberOfStocks-noOfStocksToExclude))
syncIndexPrices <- matrix(numeric(observationsSync),1)
syncIndexDates <- matrix(numeric(observationsSync),1)

#  #################################################################
#    Read SYNC data. This data has been generated with C# programm.
#  #################################################################

excludedstock <- 1
counter <-1
for (i in 1:iNumberOfStocks)
{
    print (i)
    if (i!=excludedStocks[excludedstock])   # if stock is not supposed to be excluded -> read data
    {
      stock1 <- cIndexNames[[1]][[i]]
      filename <- paste("c:\\_BOOK_",cIndexNames[[1]][[i]],"_.csv",sep="")

      x <- scan(file=filename,what=list("","",""),sep=",",skip=1,quiet=TRUE)

      numberOfObservations[i] <- length(x[[1]])
      for (j in 1:length(x[[1]]))
      {
        syncDates[counter,j] <- x[[2]][[j]]
        syncPrices[counter,j] <-type.convert(x[[3]][[j]], dec=".")
      }
      counter <- counter + 1
    }
    else    # if stock is supposed to be excluded -> skip it.
    {
        excludedstock <- excludedstock + 1
    }
}

#  #################################################################
#    calculate returns of all underlyings
#  #################################################################


syncReturns <- matrix(numeric((iNumberOfStocks-noOfStocksToExclude) * (observationsSync-1)),(iNumberOfStocks-noOfStocksToExclude))
for (i in 1: dim(syncPrices)[1])
{
    print (i)
      for (j in 1:(observationsSync-1))
      {
        syncReturns[i,j] <-  type.convert(cIndexNames[[2]][[i]], dec=".")*log(syncPrices[i,(j+1)]/syncPrices[i,j])
#         syncReturns[i,j] <-  log(syncPrices[i,(j+1)]/syncPrices[i,j])
      }
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

excludedstocknr <- 0

for (i in 1:dim(rr)[1])
{
    selection[i] <- sum(rr[i,1:8])
    if (selection[i] > 1)
    {
     if (i == excludedStocks[excludedstocknr+1])
     {
        excludedstocknr <- excludedstocknr + 1
     }
     else
     {
       print (i)
       print (selection[i+excludedstocknr])
       print (cIndexNames[[1]][i+excludedstocknr])
     }
    }
}

