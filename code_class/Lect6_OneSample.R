
################################################################
#             Biostatistical Methods I: Lecture 6              #
#            Statistical Inference: One-Sample Mean            #
#           Author: Cody Chiuzan; Date: Sept 20, 2018          #
################################################################

rm(list=ls())


# Construct a 95% CI for n=10, mu=175, and known sigma=15
# Sigma represents the pooulation standard deviation
# 1-(alpha/2)=1-(0.05/2)=0.975

LCLz<-175 - qnorm(0.975) * 15/sqrt(10)
UCLz<-175 + qnorm(0.975) * 15/sqrt(10)
CLz<-c(LCLz, UCLz)

# What if we want a 99% CI?
LCLz<-175 - qnorm(0.995) * 15/sqrt(10)
UCLz<-175 + qnorm(0.995) * 15/sqrt(10)
CLz<-c(LCLz, UCLz)

# Notice that increasing the confidence level (decreasing alpha),
# leads to a wider confidence interval.


#################################################################
#                 Student's t-distribution                      #
#################################################################

# Student's t distributions with various degrees of freedom 
# Comparison to the normal distribution

# Function dt() calculates the density for t-distribution

x <- seq(-5, 5, length=100)
densnorm <- dnorm(x)

df <- c(1, 2, 5, 29)
colors <- c("red", "blue", "green", "orange", "black")

plot(x, densnorm, type="l", lty=2, xlab="x", ylab="Density", main="Student's t Distributions")

for (i in 1:4){
  lines(x, dt(x,df[i]), lwd=2, col=colors[i])
}

legend("topright", legend=c("df=1", "df=2", "df=5", "df=29", "normal"), 
        bty='n', lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)



# Construct a 95% CI for n=10 => df=10-1=9, mu=175, and known s=15
# s represents the sample standard deviation

LCLt<-175 - qt(0.975, df=9) * 15/sqrt(10)
UCLt<-175 + qt(0.975, df=9) * 15/sqrt(10)
CLt<-c(LCLt, UCLt)



# Infarct size example
# Test if the mean infract size is different from 25
# X_bar=16, s=10, N=40

t_stats<-(16-25)/(10/sqrt(40))

# Compare the test statistics with the critical value, alpha=0.05

qt(0.975,39) # 2.26

# Compute the p-value: t_stats<0, so the p-value is twice area to the left of a t distr. with 39 df

p.val<-2*pt(t_stats,39) # p.val<.0001, reject H0.



# We want to test the hypothesis that mothers with low socio-economic status
# deliver babies with lower than 'normal' birthweights. 
# Suppose the true mean birthweight follows a normal distribution with
# mean of 120oz and standard deviation of 24oz.

# Generate data and randomly select a random sample of 50.
set.seed(3)
bweight<-rnorm(50,120,24)

# Using the sample generated above, perform a t-test to test the hypotheses:
# The true mean birthweight is different than 120oz
# Use alpha=0.05

t.test(bweight, alternative='two.sided', mu=120)

# Output from R #
# One Sample t-test

# data:  bweight
# t = -0.50797, df = 49,                                     # test statistic: t=-0.51 > -qt(0.975,49) => fail to reject H0
# p-value = 0.6138                                           # p-value >0.05 => fail to reject H0
# alternative hypothesis: true mean is not equal to 120
# 95 percent confidence interval: 112.3968 124.5350          # 95% CI for the true mean weight: (112.4,124.5)
# sample estimates:mean of x 118.4659                        # Xbar=118.5


# The true mean birthweight is less than 120oz
# Use alpha=0.05
t.test(bweight, alternative='less', mu=120)


# The true mean birthweight is greater than 120oz
# Use alpha=0.05
t.test(bweight, alternative='greater', mu=120)





