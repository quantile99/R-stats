# Permutation function
# http://stackoverflow.com/questions/25692325/list-the-sample-space
# The permn function works recursively. You pass in a list of values (x) and how 
# many items you want to choose from that list (n). If you're choosing at least 
# one value, then we set up a loop whereby we select each of the elements. Then, 
# after we've chosen one value, we need to select n-1 more from the remaining 
# items. So we call the function again, this time removing the value we've just 
# selected and reducing the number of items we need to choose.

# Up to this point we've actually been ignoring the values in the set (we've 
# assumed they are all unique). But since in this case all the balls of a 
# certain color are indistinguishable, we need to collapse our results. Since 
# permn actually returns a matrix, we will collapse the rows from a vector like 
# c("G","G","R") to the string "GGR" and then just take the unique values.
#                                                                      
#   Of course not every outcome is equally likely. If we wanted to see how often
# they occur, you could do 
#    sort(prop.table(table(apply(x, 1, paste, collapse=""))))
# which would also calculate the probabilities of each of the elements in the 
# sample space
permn <- function(x, n) {
    if (n<1) return(vector(class(x)))
    do.call(rbind, lapply(1:length(x), function(i) {
        cbind(x[i], permn(x[-i], n-1))
    })
    )
}
x <- permn(samplespace, 3)
unique(sort(apply(x, 1, paste, collapse="")))

## Example
permn(c("a", "b", "c", "d"), 3)