
# T-lookup table. Given a confidence level ci, 
# t-value 
# @param ci confidence level
# @param df degrees of freedom
t_lookup <- function (ci, df, tails = 2) {
    if(tails == 1) {
        return(-qt((100 - ci)/100, df))
    }
    if(tails == -1) {
        return(qt((100 - ci)/100, df))
    }
    return (-qt((100 - ci)/200, df))
}

t_crit <- t_lookup

# Confidence interval for the sample mean of the sampling population for t-test.
# @param n sample size 
# @param m population mean
# @param s sample standard deviation
# @param ci confidence level
# @param tails left, right or two-tailed test
ci_ttest <- function(n, m, s, ci, tails = 2) {
    df = n - 1
    res = c(round(m - t_crit(ci, df, tails)*s/sqrt(n), 2), 
            round(m + t_crit(ci, df, tails)*s/sqrt(n), 2))
    
    return (res)
}

#############################################
# This function performs a Student's t significance test. It can do a one or a
# two-tailed test.
# This is the main function. 
# @param mo - population mean in the null hypothesis
# @param xbar - sample mean
# @param s - sample standard deviation
# @param n - sample size
# @param alpha - significance level in fraction 0-1
# @param tails - left = -1, right = 1, two = 2
#
# Usage example: t_test(mo, xbar, s, n , alpha, tails)
#
t_test <- function(mo, xbar, s, n, alpha = 0.05, tails = 2) {
    se = round(s / sqrt(n), 3)
    df = n - 1
    t <- round((xbar - mo)/se, 3)
    tc = round(t_crit((1-alpha)*100, df = df, tails = tails), 3)
    sided = "two"
    if(tails == -1)
        sided = "left"
    if(tails == 1)
        sided = "right"
    
    cat(paste(
    "Testing " , sided , " sided T-test with: \nSignificance level alpha = " , alpha,
    "\nPopulation mean mo = " , mo,
    "\nSample mean xbar = " , xbar,
    "\nSample standard deviation = " , s,
    "\nSample size n = " , n,
    "\n***** Results *****",
    "\nTest statistic t =", t,
    "\nCritical value tc =", tc,
    "\nStandard error se =", se))

    cat("\n***** Conclusion *****\n")
    if(tails == -1) {
        if(t < tc) {
            cat(paste( "Reject the null hypothesis of mean =" , mo , "in left sided test with alpha =", alpha, 
                       "because t-value of" , t ,"is less than critical value" , tc))
        } else {
            cat(paste("Do not reject the null hypothesis of population mean mo =", mo))
        }    
    }
    
    
    if(tails == 1) {
        if( t > tc) {
            cat(paste( "Reject the null hypothesis of mean =" , mo , "in right sided test with alpha =", alpha, 
                       "because t-value of" , t ,"is greater than critical value" , tc))
        } else {
            cat(paste("Do not reject the null hypothesis of population mean mo =", mo)) 
        }  
    } 
    
    if(tails == 2) {
        if(t < 0 && t < -tc) 
            cat(paste( "Reject the null hypothesis of mean =" , mo , "in two-sided test with alpha =", alpha, 
                       "because t-value of" , t ,"is less than critical value" , -tc))
            if(t > 0 && t > tc)
                cat(paste( "Reject the null hypothesis of mean =" , mo , "in two-sided test with alpha =", alpha,
                           "because t-value of" , t ,"is greater than critical value" , tc))
            
            if(abs(t) <= abs(tc))
                cat(paste("Do not reject the null hypothesis of with alpha =", alpha, "population mean mo =", mo))
    
    }
    
    
    
    # plot
    par(pch=1)
    s_val = abs(tc)
    if(abs(t) > abs(tc))
        s_val = abs(t)
    x <- seq(-s_val-1, s_val+1, .1)
    plot(x+mo, dt(x, df=(n-1)), pch="*", xlab = "t-score", ylab = "Probability", 
         main = paste("t=",t, ", tc=", tc, "\nmean=", mo, ", alpha",alpha, "\nn=",n))
    lines(x+mo, dt(x, df=(n-1)),col="black")
    abline(v=t+mo)
    abline(v=tc+mo, col = "red")
    if(tails == 2)
        abline(v=-tc+mo, col = "red")
    
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Example. Here you declare the test parameters and call the function #
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# population mean
mo = 434000
# sample mean
xbar = 450000
# sample standard deviation
s = 100000
# sample size
n = 101
# significance level
alpha = 0.05
# tails, left = -1, right = 1, two = 2
tails = 1

t_test(mo, xbar, s, n , 0.05, tails)
t_test(mo, xbar, s, n , 0.10, tails)

