# Marine Spatial Planning Module - Colin Attwood
# Not just an ecological problem but social and economic too - we're ignoring that for now
# You want to spread the protected areas around the country whilest conserving the largest number of species possible

ED <- read.csv("EstuaryFishSurveyData.csv")
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#Getting rid of NAs in the data set 
ED[is.na(ED)] <- 0

# Specnumber 
#this is a function in the vegan package that allows you to sus out the alpha diversity (species richness) of the eastuaries
alpha <- specnumber(ED[4:148]) 
ED$alpha <- specnumber(ED[4:148]) #created a new column with the alpha diversity in ED

### Chunk 1
# Using Colin's code to order the data in descending alpha diversity
ED <- ED[order(-ED$alpha),]
ED

# Extracting the names of the top 20 most diverse estuaries
Alphalist <- ED[1:20,1]
Alphalist

# To ensure that the graph makes sense (geographically sound), flip kmWest to create a new column - kmEast
ED$kmEast <- 2947 - ED$kmWest

# Create a column for top 20 alpha diversity and others
ED$highlight <- ifelse(rank(-ED$alpha) <=20, "top20alpha", "other")

# Create a scatter plot showing the top 20 eastuaries in terms of alpha diversity
G1 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlight)) +
  scale_color_manual(values = c("top20alpha" = "#A36DAD", "other" = "#A0D4A1")) +
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("Alpha Diversity")

G1

# Or Colin's way in separating the top 20 from the others
ED$alphalist <- 0
ED$alphalist[1:20] <- 1
# Then just color these -> Colin

# Now we're trying to see the gamma diversity - trying to see the number of different species that would be protected if we chose these
# We can use specnumber in the vegan package for this
# In essense we're sussing the top 20 estuaries by splitting them into biogeographic zones (BZ) and getthing 6 from the WC and 7 from the SC and EC
# In other word - we want to richness without the abundance
alphabz <- specnumber(ED[4:148], groups = ED$alphalist)
alphabz
# output: 0 = 121; 1 = 116


### Chunk 2
# Then order the data to both BZ and alpha
ED <- ED[order(ED$BZ,-ED$alpha),]

# Create a column for the richness with the different BZs 
ED$alphabz <- 0
ED$alphabz[1:7] <- 1 # tagging the first 6 from the East Coast
ED$alphabz[91:97] <- 1 # tagging the first 7 from the South Coast
ED$alphabz[207:212] <- 1 # tagging the first 7 from the West Coast
ED$alphabz

# Then another column for the highlighted BZ x alpha
ED$highlightBZ <- ifelse(rank(-ED$alphabz) <=20, "top20alphabz", "other")

# Create a plot for BZ x alpha
G2 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightBZ)) +
  scale_color_manual(values = c("top20alphabz" = "#A36DAD", "other" = "#A0D4A1")) +
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("AlphaBZ Diversity")
G2

# Aaaaand a list of these estuaries
ED$alphabzlist <- 0
ED$alphabzlist[1:7] <- 1
ED$alphabzlist[91:97] <- 1
ED$alphabzlist[207:212] <- 1

#Extracting the names of these estuaries
AlphaBZlistEC <- ED[1:7,1]
AlphaBZlistSC <- ED[91:97,1]
AlphaBZlistWC <- ED[207:212,1]

AlphaBZlistEC
AlphaBZlistSC
AlphaBZlistWC

# How many species are in the alphabz list
alphabzspn <- specnumber(ED[4:148], groups = ED$alphabzlist)
alphabzspn
# output : 0 = 120; 1 = 122

### Chunk 3
# We are going to figure out the mid ranges of each species
# Problem also because some species have a bi modal dbn
G3 <- ggplot(ED, aes(x = kmEast, y = Bald.glassy)) + 
  geom_point(aes(color = highlight)) +
  scale_color_manual(values = c("top20alpha" = "#A36DAD", "other" = "#A0D4A1"))
G3

# Order data by kmWest
ED <- ED[order(-ED$kmWest),]
ED
# Creating vectors for Western Limit, Eastern Limit, Range, and Centre 
WL <- array(0, c(145))
WL

EL <- array(0, c(145))
EL

Range <- array(0, c(145))
Range

Centre <- array(0, c(145))
Centre

# Now we're going to make loops
for(i in 1:145){ # this is the species loop
  for(j in 1:232){ # this is the estuary loop
    if(ED[j, i+3]>0){EL[i] <- ED[j,2]}
  }
  for(j in 232:1){ # this is the estuary loop bottom up
    if(ED[j, i+3]>0){WL[i] <- ED[j,2]}
  }
  Range[i] <- WL[i] - EL[i] # range is between the two limits
  Centre[i] <- (WL[i]+EL[i])/2
}
 
EL   
WL
Range
Centre

Centre <- as.numeric
