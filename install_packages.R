install.packages("FuzzyAHP")
library(FuzzyAHP)

comparisonMatrixValues = c(1,3,9,
                           NA,1,6,
                           NA,NA,1)
comparisonMatrix = matrix(comparisonMatrixValues, nrow = 3, ncol = 3, byrow = TRUE)
comparisonMatrix = pairwiseComparisonMatrix(comparisonMatrix)
show(comparisonMatrix)
weights = calculateWeights(comparisonMatrix)
print(weights)


install.packages("RPMG")
