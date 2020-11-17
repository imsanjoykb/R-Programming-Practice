##### Hand-in-Exercise 3 #####

### Configuration values:
ds <- read.table(file="StatWiSo2006.txt",header=TRUE,sep="\t")
removeNAs = TRUE

### Task 1
MWs = ds[11:20]
# We could remove all NAs:
if (removeNAs)
{
  MWs <- MWs[complete.cases(MWs),]
}
MWs$Ones = rowSums(MWs[1:10])
MWs$Changes = rowSums(MWs[1:9] != MWs[2:10])

### Task 2
par(mfrow=c(2,2))
Ones.practical = table(factor(MWs$Ones, levels=0:10))
Changes.practical = table(factor(MWs$Changes, levels=0:9))
barplot(Ones.practical, main="Number of heads", col="red")
barplot(Changes.practical, main="Number of changes", col="blue")

### Task 3
# It follows a binomial distribution, see for yourself:
n <- sum(Ones.practical) # = sum(Changes.practical)
Ones.theoretical = dbinom(0:10, 10, 0.5)
Changes.theoretical = dbinom(0:9, 9, 0.5)
barplot(rbind(Ones.practical/n, Ones.theoretical), beside=TRUE, col=c("red","gray"))
barplot(rbind(Changes.practical/n, Changes.theoretical), beside=TRUE, col=c("blue","gray"))

### Task 4
source("MultinomialCBs.R")
MultinomialCBs(Ones.practical, p0=Ones.theoretical, overall.alpha=0.05)
MultinomialCBs(Changes.practical, p0=Changes.theoretical, overall.alpha=0.05)

