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
ED$highlight <- ifelse(rank(-ED$alpha) <=20, "chosen", "other")

# Create a scatter plot showing the top 20 eastuaries in terms of alpha diversity
G1 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlight)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1")) +
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("Alpha Diversity")+
  theme(legend.title = element_blank())

G1

# Or Colin's way in separating the top 20 from the others
ED$alphalist <- 0
ED$alphalist[1:20] <- 1
# Then just color these -> Colin

# Now we're trying to see the gamma diversity - trying to see the number of different species that would be protected if we chose these
# We can use specnumber in the vegan package for this
# In essense we're sussing the top 20 estuaries by splitting them into biogeographic zones (BZ) and getthing 6 from the WC and 7 from the SC and EC
# In other word - we want to richness without the abundance
alphaout <- specnumber(ED[4:148], groups = ED$alphalist)
alphaout # output: 0 = 121; 1 = 116 ~ that's 80%


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
ED$highlightBZ <- ifelse(rank(-ED$alphabz) <=20, "chosen", "other")

# Create a plot for BZ x alpha
G2 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightBZ)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1")) +
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("AlphaBZ Diversity")+
  theme(legend.title = element_blank())
G2

#Extracting the names of these estuaries
AlphaBZlistEC <- ED[1:7,1]
AlphaBZlistSC <- ED[91:97,1]
AlphaBZlistWC <- ED[207:212,1]

AlphaBZlistEC
AlphaBZlistSC
AlphaBZlistWC

# How many species are in the alphabz list
alphabzout <- specnumber(ED[4:148], groups = ED$alphabzlist)
alphabzout # output : 0 = 120; 1 = 122 ~ that's 84%

### Chunk 3
# We are going to figure out the mid ranges of each species
# Problem also because some species have a bi modal dbn

G3 <- ggplot(ED, aes(x = kmEast, y = Bluespot.mullet)) + 
  geom_point(aes(color = highlightBZ)) +
  scale_color_manual(values = c("top20alphabz" = "#A36DAD", "other" = "#A0D4A1"))+
  theme(legend.title = element_blank())
G3



# Chunk 4
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

Mode <- array(0, c(145))
Mode

max <- 0

# Now we're going to make loops
for(i in 1:145){ # this is the species loop
  max <- 0
  for(j in 1:232){ # this is the estuary loop
    if(ED[j, i+3]>0){EL[i] <- ED[j,2]}
    if(ED[j, i+3]>max){
      Mode[i] <- ED[j, 2]
      max <- ED[j,i+3]
    }
  }
  for(j in 232:1){ # this is the estuary loop bottom up
    if(ED[j, i+3]>0){WL[i] <- ED[j,2]}
  }
  Range[i] <- WL[i] - EL[i] # range is between the two limits
  Centre[i] <- (WL[i]-EL[i])/2
}
 
EL   
WL
Range
Mode
Centre

hist(Centre)
hist(Mode)

# Getting the top 20 estuaries by best modes
# get names of estuaries most frequently in modes
freq <- table(Mode) # get frequencies of different estuaries 
sortfreq <- sort(freq, decreasing = TRUE) # sort for highest freqs first 
sortfreq
top20range <- c(179, 267, 308, 1448, 1034, 1036, 3, 238, 408, 452, 876, 2947, 568, 685, 830, 842, 984, 318, 564, 638) # take top 20 most frequent modes 
midranges <- ED$Estuary[match(top20range, ED$kmWest)] # match the km values to the estuary name 

# in retro I have taken the top 25 cross referenced the last 8 with the most biodiverse estuaries and found that three corresponded and included them (the last 3) into the list (I shifted 20 to 21)
midranges

# Creating a mode list
ED$modelist <- 0
# List of locations to set modelist to 1
locations <- c("St Lucia", "Mlalazi", "Matigulu/Nyoni", "Knysna", "Kariega", "Bushmans", "Kosi", "Mfolozi/Msunduzi", "Manzimtoti", "Mkomazi", "Kwelera", "Orange", "Mtentu", "Mngazana", "Ngqusi/Inxaxo", "Great Kei", "Great Fish", "Zinkwasi", "Mzamba", "Mntafufu")
# Set modelist to 1
ED <- ED %>%
  mutate(modelist = ifelse(ED$Estuary %in% locations, 1, 0))

# Highlight 
ED$highlightmode <- ifelse(rank(-ED$modelist) <=20, "chosen", "other")
# Create a graph
G4 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightmode)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("Alpha") +
  theme(legend.title = element_blank())
G4

# Check how many species are saved in this
modeout <- specnumber(ED[4:148], groups = ED$modelist)
modeout # output: 0 = 107; 1 = 128 ~ that's 88%


# Chunk 5
# Dendogram, split estuaries in each group, choose ten groups, order data by group then by diversity in group, then choose top 2 entries of each group
## Sppcomplist
# There is an estuary without fish - we need to subset the data to exclude that one
# Cannot run a dendogram with EMPTY ESTUARIES

ED <- subset(ED, alpha >0)
nrow(ED) # 218 left

ED_deco <- decostand(ED[,c(4:148)], method = "total") # "total tell us more about the proportions of the fish that occur in each estuary - instead of just focusing on raw abundance numbers because we're interested in spp comp not spp numbers
ED_deco # this has taken the numbers and reduced them to proportions

ED_dist <- vegdist(ED_deco, method = "bray") # using the proportion dataset we're working out the dissimilarity bt ea estuary by using the bray-curtis measure of dissimilarity
ED_dist # gives you the dissimilary number but now need a graph - ask R to dra a dendrogram to compare these data

ED_clust <- hclust(ED_dist, method = "average")
ED_clust # this in in preparation for the plot

plot(ED_clust, hang = -1, ylab = "Dissimilarity", xlab = "Estuaries")

# Cut a line in the dendrogram to allow for it to branch only 10 times
slice <- cutree(ED_clust, h = 0.75)
slice # estuaries sorted into dendogram slices ~ 12 branches

slice <- cutree(ED_clust, h = 0.8)
slice # cut it to 80% ~ landed up with 10 groups
hist(slice)
# Create a new variable
ED$cut80 <- slice


# Now we need to order it by cut then by alpha
ED <- ED[order(ED$cut80,-ED$alpha),]
# Tagging chosen ones
ED$clust <- 0
ED$clust[c(1, 2, 35, 36, 44, 45, 46, 47, 48, 193, 194, 197, 198, 208, 209, 210, 213, 214, 215, 218)] <- 1


# Highlight top 20 clusters
ED$highlightclust <- ifelse(rank(-ED$clust) <=20, "chosen", "other")
# Plot
G5 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightclust)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  ggtitle("Scatter Plot with Chosen Estuaries Highlighted") +
  xlab("km East") +
  ylab ("Alpha") +
  theme(legend.title = element_blank())
G5

# Check the number of spp saved
clustout <- specnumber(ED[4:148], groups = ED$clust)
clustout # output: 0 = 121; 1 = 116 ~ 80%

# note that this is good but excludes all estuaries without fish and so will be biased against plants and invertebrates
