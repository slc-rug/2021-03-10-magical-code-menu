# Libraries ---- 
library(tidyverse)
library(ggplot2)



# Document Management/Sections -----------------------------------------------------
{
# Insert Section


# My new section ----------------------------------------------------------

    
    
    
    
# Normal Comment

    
    
    
    
    
    
    
# Four or more  =, - or # at the end of a comment define a section
# Example 1 ----
# Example 2 ====
# Example 3 ####

# Selection Label ---------------------------------------------------------

    # SECTION ONE ----
    y <- 11:20
    
    # SECTION TWO ====
    x <- 1:10
    # SECTION THREE ----
    testfunct2 <- function() {
        # sect in function 2 =====
        f <- 6
        testsubfunct2_1 <- function() {
            # sect in subfunction 2_1 -----
            if (a == 1) {
                # section in if ----
                g < 7
            } else {
                # section in else ----
                h = 8
            }
        } 
    }
}



# Diagnostics -------------------------------------------------------------

b <- function(x,y,z)
{
    a <- 10*x
    return(x+4)
}
b(1,2)



# Go to function definition / function defined 'out of scope'
source('Examples2.r')
d(1,2,3)

# Go to help

plot(dataThatDoesntExist)
