
#####################################
### Statistical Analysis of a     ###
### Categorical Variable (factor) ###
#####################################

ds <- read.table(file="StatWiSo2003.txt",header=TRUE)
X <- factor(ds$ZufZiffer,levels=0:9)
X

TX <- table(X)
TX
# TX looks like a matrix with two rows.
# But it is a one-dimensional array with
# named components:
str(TX)

# The one "NA" is automatically removed
n <- sum(TX)
n

# Empirical probabilities of the 10 digits:
p.emp <- TX/n
p.emp
round(p.emp,digits=3)


# What binom.test() really achieves:
result <- binom.test(TX[1],n, p=0.1)
result
str(result)

result$conf.int

# confidence bounds:
a <- rep(0,10)  # lower bounds
b <- rep(1,10)  # upper bounds

for (j in 1:10)
{
	result <- binom.test(TX[j],n,
		conf.level=1-0.05/10) #overall 95% confidence (over all intervalls
	a[j] <- result$conf.int[1]
	b[j] <- result$conf.int[2]
}

cbind(TX,p.emp,a,b)
# Looking at this table we may conclude with confidence
# 95% that certain digits are under- or overrepresented.
# Which ones?


# Once you have written your program:
source("MultinomialCBs.R")
MultinomialCBs(TX)


# Back to our example of the "random digits":
# A classical procedure to test whether a given
# theoretical distribution on a finite set fits
# given data is Pearson's Chisquared Test.
help.search("Pearson")
? chisq.test

# so here use:
chisq.test(TX) # number of occurences

# Explanation:



# (if possible).
TX2 <- c(166761, 151296, 164804, 158973, 156455, 149251, 159924, 145184, 141164, 154777, 150678, 163882)
MultinomialCBs(TX2) -> resultTX2
p0 = c(31,28,31,30,31,30,31,31,30,31,30,31)/365 # expected values

cbind(resultTX2, p0)
# => in winter & july (sultry), more people than expected 
# (if they were evenly distributed) are dying. Oh noez!

# with chi square test:
chisq.test(TX2, p=p0)