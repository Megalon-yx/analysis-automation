# Megalonyx's Automated Analysis Tool [MAATS] for statistical data

################################################################################
#################### Don't make lines longer than this #########################
################################################################################

# Functions
ipak <- function(pkg){
  # Sourced: https://gist.github.com/stevenworthington/3178163#file-ipak-r
  # Checks to see if package is installed, corrects, and loads package.
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# Begin practice analysis problems, but code for flexibility and reuseability. 
# Don't be afraid to make functions! 

# Identify independent and dependent variables
  # Identify form of each variable
  # Identify "minimum model"
# Check distributions for each variable
  # Note skewness and kurtosis
  # Compare to Gaussian distribution
  # Normalize variables
# Check form of response variables
  # What type of model do we need?
    # Run applicable models
      # Use MANOVA/Wald
  # What type of tests do we need?