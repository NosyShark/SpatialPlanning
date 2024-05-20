# Marine Spatial Planning Module - Colin Attwood
# Not just an ecological problem but social and economic too - we're ignoring that for now
# You want to spread the protected areas around the country

ED <- read.csv("EstuaryFishSurveyData.csv")
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#Getting rid of NAs
ED[is.na(ED)] <- 0
# Specnumberrrrrr
alpha <- specnumber(ED[4:148])
ED$alpha <- specnumber(ED[4:148])

# Extracting alpha diversity names but using Collins code and specnumber
ED <- ED[order(-ED$alpha),]
ED
Alphalist <- ED[1:20,1]
Alphalist

# To ensure that the kmWest is flipped in our plot we want to create a new column - kmEast
ED$kmEast <- 2947 - ED$kmWest

# Column for top 20 and others
ED$highlight <- ifelse(rank(-ED$alpha) <=20, "top20alpha", "other")
# Trying to create a plot
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
alphabz <- specnumber(ED[4:148], groups = ED$alphalist)
alphabz

# Then order it to both BZ and alpha
ED <- ED[order(ED$BZ,-ED$alpha),]

ED$alphabz <- 0
ED$alphabz[1:7] <- 1 # tagging the first 6 from the East Coast
ED$alphabz[91:97] <- 1 # tagging the first 7 from the South Coast
ED$alphabz[207:212] <- 1 # tagging the first 7 from the West Coast
ED$alphabz

ED$highlightBZ <- ifelse(rank(-ED$alphabz) <=20, "top20alphabz", "other")
# Trying to create a plot
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

# How many species are in the alphabz list
alphabzspn <- specnumber(ED[4:148], groups = ED$alphabzlist)
alphabzspn

