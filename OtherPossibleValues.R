#Disable Scientific Notation
options(scipen = 999)

#In Session, set Working Directory to Source File Location!

#import total and val tables. Change to name of CVS to your file names!
totalCSV <- read.csv("US11_TOTAL.csv",TRUE,",", stringsAsFactors=FALSE)
valCSV <- read.csv("US11_VAL.csv",TRUE,",", stringsAsFactors=FALSE)

#Select the industry to test
ind <-11#5 = Wood/Cork, 8 = Chemicals 11 = basic metals

#----------------------------------------------------------
#Sorting the data into matricies and vectors
#Make sure the table sizes line up with your data!

#Total Output (prices) of each sector
prices <- as.numeric(totalCSV[45, 3:35])

# s+v for each sector. Taxes count as added value taken by the state.
# Subsidies don't count as added value in LTV, so they get subtracted out.
addedVal <- as.numeric(totalCSV[44, 3:35]) + as.numeric(totalCSV[42, 3:35])

# v for each sector
v <- as.numeric(valCSV[9,3:35])

#calculating p
p <-addedVal - v

#Input-Output of each sector
IO <-  totalCSV[8:40,3:35]
IO[,] <- sapply(IO[,], as.numeric)

#creating the matrix out of IO, s.t. v and p are at bottom
#and each entry is divided by it's respective prices
mIO <- IO
mIO <- rbind(mIO, v)
mIO <- rbind(mIO, p)

#scaling mIO such that each colm is divided by total output in that
#industry. Turning result into a matrix.
mIO <- t(mIO)
mIO <- mIO / prices
mIO <- t(mIO)
mIO <- data.matrix(mIO)

#--------------------------------------------------------------------
#This is the iterative part. We will take an industry, look at all that
#it uses for constant capital, and break those down into V and P.
#Basically, we will end up with price = P+V, where P is profits, V is labor,
#both direct and indirect.

iterV <- numeric(33)
iterP <- numeric(33)
iterInd <- numeric(33)
for (i in c(1:33)){
  #Select industry, get c1,v1,p1
  cRemain <- IO[,i]
  vSum <- v[i]
  pSum <- p[i]
  indSum <- IO[i,ind]
  remaining <- sum(cRemain)
  
  #iteratively decompose cn into v(n+1) and p(n+1)
  #untill remaining is less then the sig fig of our data
  while(remaining > 0.1){
    cNew <- mIO %*% cRemain
    vSum <- vSum + cNew[34]
    pSum <- pSum + cNew[35]
    indSum <- indSum + cNew[ind]
    cRemain <- cNew[1:33]
    remaining <- sum(cRemain)
  }
  #add sums to the iterative vectors
  iterV[i] <- vSum
  iterP[i] <- pSum
  iterInd[i] <- indSum
}

#Without Removing irrelevant sectors that don't produce commodities
plot(prices , iterInd , 
     xlab = "Output(Prices)",
     ylab = "Industry input, Direct and Indirect", 
     main = "Direct and Indirect Industry vs Prices in Each Sector (with all sectors)")
abline(lm(iterInd ~ prices))

# Removing Retail, Finance, Real Estate, Renting Machines,
# Computer related activities, R&D, Education, Health, Local/state spending
remove = c(21, 25,26,27,28,29,30,31,32,33)

plot(prices[-remove] , iterInd[-remove] , 
     xlab = "Output(Prices)",
     ylab = "Industry input, Direct and Indirect", 
     main = "Direct and Indirect Industry vs Prices in Each Sector")
abline(lm(iterInd[-remove] ~ prices[-remove]))

corLbl = sprintf("correlation: %s",cor(iterInd[-remove] , prices[-remove]))
mtext(corLbl,4)