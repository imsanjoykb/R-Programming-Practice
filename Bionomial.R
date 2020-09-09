

##############################
### Binomial Distributions ###
##############################


help.search("binomial distribution")
? Binomial


n <- 30
x <- 0:n

# Visualize binom(n,p) for 9 different values opf p:
par(mfcol=c(3,3))
pv <- seq(from=0.1,to=0.9,by=0.1)
for (j in 1:9)
{
	barplot(dbinom(x,n,pv[j]),names.arg=x,
		main=paste("p = ", as.character(pv[j])))
}

# There are many ways to modify plots.
? par
# One interesting parameter is "mai"
# ("ma(rgin sizes in) i(nches)").
# An example:
# same as css margins, less space between plots
par(mfcol=c(3,3),mai=c(0.3,0.3,0.5,0))
pv <- seq(from=0.1,to=0.9,by=0.1)
for (j in 1:9)
{
	barplot(dbinom(x,n,pv[j]),names.arg=x,
		main=paste("p = ", as.character(pv[j])))
}

? binom.test


n <- 400
x <- 122

binom.test(x,n)
binom.test(x,n,conf.level=0.99)


binom.test(x,n,alternative="less")
binom.test(x,n,alternative="greater")
binom.test(x,n,alternative="two.sided")

binom.test(8,60)
binom.test(8,60,conf.level=0.99)


# Second illustration of binom.test():
# Use "scan()" to generate and store a "random vector"
# containing about 70 entries in {1,2}:
data <- scan("")
# Question: Are you a reliable random generator?

# First test:
# We determine n = length(data) and
# X = number of entries equal to "1":
n <- length(data)
x <- sum(data == 1)
binom.test(x,n)


data[1:(n-1)] != data[2:n] # if two following numbers are not the same => true
n2 <- n-1
x2 <- sum(data[1:(n-1)] != data[2:n]) #how many times did the coin change immediately?
binom.test(x2,n2)




binom.test(127, 1000, p=0.1, alternative="greater", conf.level=0.99)

binom.test(9,52, p=1/4, alternative="two.sided")

? binom.test

binom.test(127, 1000, p=0.1, alternative="greater")

# For second example: working hypothesis (alternative)
# is that p != 0.25, any deviation is possible.

binom.test(9, 52, p=0.25)
binom.test(9, 52, p=0.25, alternative="two.sided")

# Disappointed? Well, the variability of
# binom(52,0.25) is quite large:
barplot(dbinom(0:52,52,0.25),names.arg=0:52)

# If we would have had 5 times as many observations:

binom.test(45, 260, p=0.25, alternative="two.sided")
barplot(dbinom(0:260,260,0.25),names.arg=0:260)

