################################################################
#             Biostatistical Methods I: Lecture 9              #
#                     Test for Proportions                     #
#           Author: Cody Chiuzan; Date: Oct 4, 2018            #
################################################################

rm(list=ls())

################################################################
# In a survey of 300 randomly selected drivers, 125 claimed that
# they regularly wear seat belts. Can we conclude from these data 
# that the population proportion who regularly wear seat belts is 0.50?
# Perform a hypothesis test and 
# Construct a 95% confidence interval for the true population proportion. 
################################################################

# p_hat=125/300
# p_0=0.50


# Prop.test performs a chi-squared test and not a z-test.
prop.test(125,300, p=0.5)


# Create your own function to perform a one-sample proportion test 
# and create a 100(1-alpha) CI using the Normal Approximation

one.proptest_norm <- function(x, n, p=NULL, conf.level=0.95, alternative="less") {
# x the number of 'cases' in the sample
# n the total sample size
# p is the hypothesized value
  
  z.stat <- NULL
  cint <- NULL
  p.val <- NULL
  phat <- x/n
  qhat <- 1 - phat
 
  if(length(p) > 0) { 
    q <- 1-p
    SE.phat <- sqrt((p*q)/n) 
    z.stat <- (phat - p)/SE.phat
    p.val <- pnorm(z.stat)
    
    if(alternative=="two.sided") {
       p.val <- p.val * 2}
    
    if(alternative=="greater") {
      p.val <- 1 - p.val
    }
  } else {
    
# Construct a confidence interval   
    SE.phat <- sqrt((phat*qhat)/n)
  }
  cint <- phat + c(-1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat),
                      ((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat))
  
  return(list(estimate=phat, z.stat=z.stat, p.val=p.val, cint=cint))
}

# In our example: 

one.proptest_norm(125,300, 0.5, alternative="two.sided") 

# P-hat estimate
#0.4166667

# Z-statistic: z.stat
# -2.886751

#$p.val
#0.003892417

# 95 % CI: (0.3600874 0.4732460)


####################################################################
#       Perform an Exact test, no normal approximation             #
#          This function uses Clopper-Pearson method               #
####################################################################

# used when normal approximation is not valid

binom.test(125, 300, p = 0.5, alternative = "two.sided", conf.level = 0.95)

# 95% Exact CI:
#0.3602804 0.4747154

# Exact p-value: 0.004589



#####################################################################
# How about two-sample test for proportions?                        #
# This will be part of HW 3.                                        #
# Create your own function, or adapt the one-sample above to test   #
# two population proportions under normal approximation.            #
#####################################################################


two.proptest_norm <- function(x1, n1, x2, n2, p=NULL, conf.level=0.95, alternative="two.sided") {
  # x the number of 'cases' in the sample
  # n the total sample size
  # p is the hypothesized value
  
  z.stat <- NULL
  cint <- NULL
  p.val <- NULL
  phat1 <- x1/n1
  qhat1 <- 1 - phat1
  phat2 <- x2/n2
  qhat2 <- 1 - phat2
  
  p_pool <- (x1 + x2)/(n1 + n2)
  q_pool <- 1 - p_pool
  
  # with continuity correction
  if(length(p) > 0) { 
    SE.phat <- sqrt(p_pool*q_pool*(1/n1 + 1/n2))
    z.stat <- (abs(phat1 - phat2) - 1/2*(1/n1 + 1/n2)) / SE.phat
    p.val <- pnorm(abs(z.stat))
    
    if(alternative=="two.sided") {
      p.val <- (1 - p.val) * 2}
    
    if(alternative=="greater") {
      p.val <- 1 - p.val
    }
  } else {
    
    # Construct a confidence interval   
    SE.phat <- sqrt(p_pool*q_pool*(1/n1 + 1/n2))
  }
  cint <- phat1 - phat2 + c(-1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat),
                   ((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat))
  
  return(list(estimate=p_pool, z.stat=z.stat, p.val=p.val, cint=cint))
}


