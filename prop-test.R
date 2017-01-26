# Z-lookup table. Given a confidence level ci, 
# t-value 
# @param ci confidence level
# @param tails -1: left, 1: right, 2: both
z_lookup <- function (ci, tails = 2) {
    if(tails == 1) {
        return(-qnorm((100 - ci)/100))
    }
    if(tails == -1) {
        return(qnorm((100 - ci)/100))
    }
    return (-qnorm((100 - ci)/200))
}

# Confidence interval for the sample mean of the sampling population for t-test.
# @param m population mean
# @param s population standard deviation, or sample standard deviation
# @param ci confidence level
# @param tails left, right or two-tailed test
# @param n sample size. If omitted, s is impled to be population standard deviation.
ci_mean <- function(m, s, ci, tails = 2, n = 1) {
    df = n - 1
    res = c(round(m - z_lookup(ci, tails)*s/sqrt(n), 2), 
            round(m + z_lookup(ci, tails)*s/sqrt(n), 2))
    
    return (res)
}


# Confidence interval for the sample proportion of the sampling population for
# test about proportion.
# @param n sample size 
# @param p population proportion
# @param ci confidence level
# @param tails left, right or two-tailed test
ci_prop_test <- function(n, p, ci, tails = 2) {
    df = n - 1
    sp = sqrt(p * (1 - p) / n)      # standard deviation of proportion
    MofE = z_lookup(ci, tails) * sp # Margin of Error
    res = c(round(p - MofE, 2), 
            round(p + MofE, 2))
    
    return (res)
}


#############################################
# This function performs a test about population proportion. It can do a one or a
# two-tailed test.
# This is the main function. 
# @param pi0 - population proportion in the null hypothesis
# @param pi - sample proportion
# @param n - sample size
# @param alpha - significance level in fraction 0-1
# @param tails - left = -1, right = 1, two = 2
#
# Usage example: t_test(mo, xbar, s, n , alpha, tails)
#
prop_test <- function(pi0, pi, n, alpha = 0.05, tails = 2) {
    se = round(sqrt((pi0 * (1 - pi0))/n), 3)
    Z = round((pi - pi0)/se, 3)
    zc = round(z_lookup((1-alpha)*100, tails = tails), 3)
    
    sided = "two"
    
    if(tails == -1)
        sided = "left"
    if(tails == 1)
        sided = "right"
    
    cat(paste(
        "Testing " , sided , " testing about proportion  with: \nSignificance level alpha = " , alpha,
        "\nPopulation proportion pi0 = " , pi0,
        "\nSample proportion pi = " , pi,
        "\nSample size n = " , n,
        "\n***** Results *****",
        "\nTest statistic Z =", Z,
        "\nCritical value zc =", zc,
        "\nStandard error se =", se))
    
    cat("\n***** Conclusion *****\n")
    if(tails == -1) {
        if(Z < -zc) {
            cat(paste( "Reject the null hypothesis of population proportion pi0 =" , pi0 , "in left sided test", 
                       "because sample proportion pi=" , pi ," is less than critical proportion = " , pi0+zc*se))
        } else {
            cat(paste("Don't reject the null hypothesis of population proportion pi0 =", pi0))
        }    
    }
    
    
    if(tails == 1) {
        if( Z > zc) {
            cat(paste( "Reject the null hypothesis of population proportion pi0 =" , pi0 , "in right sided test", 
                       "because sample proportion pi=" , pi ," is greater than critical proportion = " , pi0+zc*se))
        } else {
            cat(paste("Don't reject the null hypothesis of population proportion pi0 =", pi0)) 
        }  
    } 
    
    if(tails == 2) {
        if(Z < 0 && Z < -zc) 
            cat(paste( "Reject the null hypothesis of population proportion =" , pi0 , "in two-sided test", 
                       "because sample proportion pi=" , pi ,"is less than critical proportion" , pi0-zc*se))
        if(Z > 0 && Z > zc)
            cat(paste( "Reject the null hypothesis of population proportion =" , pi0 , "in two-sided test", 
                       "because sample proportion pi=" , pi ,"is greater than critical proportion" , pi0+zc*se))
        
        if(abs(Z) <= abs(zc))
            cat(paste("Don't reject the null hypothesis of population proportion pi0 =", pi0, "in two-sided test"))
        
    }
    
    # plot
    
    par(pch=1)
    
    x <- seq(pi0 - 3*se, pi0 + 3*se, length.out = 100)
    plot(x , dnorm(x, mean = pi0, sd = se), pch="*", xlab = "z-score", ylab = "Probability",
         main = paste("Test about Probability, α=", alpha, "\npi0 =", pi0, "; pi = ", pi))
    abline(v=pi)
    
    if(tails == -1) 
        abline(v=pi0+zc*se, col = "red")
    if(tails == 1) 
        abline(v=pi0+zc*se, col = "red")
    
    if(tails == 2) { 
        abline(v=pi0-zc*se, col = "red")
        abline(v=pi0+zc*se, col = "red")
    }

    
}

n_samp <- function(z, me, p = 0.5) {
    zc = z_lookup(z)
    n = p * (1-p) * (zc ^ 2) / (me ^ 2)
    return(n)
}

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Example. Here you declare the test parameters and call the function #
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# population proportion
pi0 = .68
# sample mean
pi = .64
# sample size
n = 900
# significance level
alpha = 0.05
# tails, left = -1, right = 1, two = 2
tails = 2

prop_test(pi0, pi, n , alpha, tails)




