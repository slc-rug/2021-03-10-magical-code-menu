library(tidyverse)
library(ggplot2)
library(partitions)

# Extract Variables/Functions ----

#Reformat Code
Thresholds <-
    cbind(testGrid, LLThresholds) %>% 
    as_tibble() %>% 
    pivot_wider(
        id_cols = c(questions, concepts),
        names_prefix = "1 in ",
        names_from = thresholds,
        values_from = LLThresholds
    )


#Extract Variable
data<-read.csv("Conjoint Study - ST file.csv")

fileName <- "Conjoint Study - ST file.csv"
data<-read.csv(fileName)
gsub(".csv",".xlsx",fileName)

#Extract Function
sample(LETTERS[1:5], 1000, replace = TRUE, prob = exp(1:5)) %>% table

sampleLetters <- function(K, N) {
  sample(LETTERS[1:K], N, replace = TRUE, prob = exp(1:K)) %>% table
}
sampleLetters(10,10000)

#Real Function Example from the other day
partitions::restrictedparts(8,4) %>% as.matrix %>% t %>% as_tibble

countPermutations <- function(K=10, D=5) {
  partitions::restrictedparts(K,D) %>% as.matrix %>% t %>% as_tibble
}

### Extract Function Example ###

# Simulate a best fit line on noisy data

# Inputs
N <- 500
slope <- 3
stDev <- 2

simulateLine <- function(N=1000, slope=0, stDev=1) {
  
  # Simulation Code
  x <- rnorm(N,0,1)
  e <- rnorm(N,sd=stDev)
  y <- x*slope + e
  data1 <- data.frame(x=x,y=y)
  
  ggplot(data1,aes(x=x,y=y))+geom_point() +
      geom_point()+
      geom_smooth(method=lm,formula=y~x)

}
simulateLine()











# Cleaning Up my old bad code ----
# Before/After

#Example 1
cbind(colMeans(theta)[o],sqrt(sigs[o]),colMeans(theta)[o]-phi*sqrt(sigs[o]))
cbind(colMeans(theta)[o],
      sqrt(sigs[o]),
      colMeans(theta)[o] - phi * sqrt(sigs[o]))

#Example 2
ggsave(paste("results\\Plot - ",ind," - ",seg," - ",i,".pdf",sep=""), plot = c2, width = 10, height = 8, units = "in", dpi = 300)

fileName <- paste("results\\Plot - ", ind, " - ", seg, " - ", i, ".pdf", sep = "")
ggsave(
    fileName,
    plot = c2,
    width = 10,
    height = 8,
    units = "in",
    dpi = 300
)

#Example 3
griddata <- rawdata[rawdata$sys_RespStatus == 5 & 1 == 1, c("sys_RespNum", rawVersionLabel, fieldNames[grep("Task", fieldNames)])]

rowSlice    <- rawdata$sys_RespStatus == 5 & 1 == 1
columnSlice <- c("sys_RespNum", rawVersionLabel, fieldNames[grep("Task", fieldNames)])
griddata    <- rawdata[rowSlice, columnSlice]

#Example 4
which(!(MDList%in%as.numeric(sapply(utilityFiles,function(x){strsplit(x,"_")[[1]][1]}))))

splittingFunction <- function(x){strsplit(x,"_")[[1]][1]}
IncludeList <- as.numeric(sapply(utilityFiles,splittingFunction))
which(!(MDList %in% IncludeList))

#Example 5
designmatrix[i,((j-1)*nitemsperset+1):(j*nitemsperset)]<-sample(1:nitems,nitemsperset,replace=F,prob=exp(lambda*b))

designmatrix[i, ((j - 1) * nitemsperset + 1):(j * nitemsperset)] <-
    sample(1:nitems,
           nitemsperset,
           replace = F,
           prob = exp(lambda * b))

# Reindent Lines
cbind(rnorm(100),
  rbinom(100, 5, 1/4),
         rcauchy(100),
     rt(100, 5, 0))

randomNums <- cbind(rnorm(100),
                    rbinom(100, 5, 1/4),
                    rcauchy(100),
                    rt(100, 5, 0))
source('Cleaning Code Examples.r')
