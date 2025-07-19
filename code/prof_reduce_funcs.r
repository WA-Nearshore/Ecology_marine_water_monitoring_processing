################################################################################
# Functions to reduce Ecology profile data so a single value for a given
# variable and profile.
# The three functions reduce the profiles in contrasting ways:
# fdepth05 = returns the single variable value at depth 0.5 m
# fdepth05to5 = returns the average variable value over depths 0.5 to 5m
# fdepthDiff = returns the difference in variable values at depth 0.5 and 5m
#
# July 2024
################################################################################

# given vectors of some variable and depth, this function returns the variable
# value with same index as the depth 0.5m within the Depth vector.
fdepth05 <- function(Var, Depth) {
  val <- Var[Depth==0.5]
  if (length(val) != 1) val <- NA 
  return(val)
}

# returns the average variable value over depths 0.5 to 5m
fdepth05to5 <- function(Var, Depth) {
  aveVar <- mean(Var[Depth>=0.5 & Depth<=5])
  if (length(aveVar) != 1) aveVar <- NA 
  return(aveVar)
}

# returns the difference in variable values between 0.5 and 5m depths
fdepthDiff <- function(Var, Depth) {
  diffVar <- Var[Depth==0.5] - Var[Depth==5]
  if (length(diffVar) != 1) diffVar <- NA 
  return(diffVar)
}
