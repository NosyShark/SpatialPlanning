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
install.packages("patchwork")
library(patchwork)

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
  xlab("km East") +
  ylab ("Alpha Diversity")+
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())

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
  xlab("km East") +
  ylab ("Alpha Diversity")+
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
G2

#Extracting the names of these estuaries
AlphaBZlistEC <- ED[1:7,1]
AlphaBZlistSC <- ED[91:97,1]
AlphaBZlistWC <- ED[207:212,1]

AlphaBZlistEC
AlphaBZlistSC
AlphaBZlistWC

# How many species are in the alphabz list
alphabzout <- specnumber(ED[4:148], groups = ED$alphabz)
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
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
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
Clustlist <- ED[c(1, 2, 35, 36, 44, 45, 46, 47, 48, 193, 194, 197, 198, 208, 209, 210, 213, 214, 215, 218),1]
Clustlist

# Highlight top 20 clusters
ED$highlightclust <- ifelse(rank(-ED$clust) <=20, "chosen", "other")
# Plot
G5 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightclust)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
G5

# Check the number of spp saved
clustout <- specnumber(ED[4:148], groups = ED$clust)
clustout # output: 0 = 121; 1 = 116 ~ 80%
# note that this is good but excludes all estuaries without fish and so will be biased against plants and invertebrates


# Chunk 6
# Now we will try to find an estuary that has the greatest complementary to other estuaries
# Order by Alpha 
# Complist variable top Est = 1, others = 0
# remove from database all spp that occur in that estuary
# Reorder by alpha and repeat
# 1. Create variable called complist

NED <- ED
NED$complist <- 0

for(j in 1:20){
  NED <- NED[order(-NED$alpha),]
  NED$complist[1] <- 1
  for(i in 1:145){
    if(NED[1, i+3] > 0 ){NED[,i+3] <- 0}
  }
  NED$alpha <- specnumber(NED[,4:148])
}

NED$complist
NED <- NED[order(-NED$complist),] 
Complist <- NED[1:20,1]
Complist

ED <- ED[order(ED$kmWest),]
NED <- NED[order(NED$kmWest),]
ED$complist <- NED$complist
compout <- specnumber(ED[4:148], groups = ED$complist)
compout # output: 0 = 109; 1 = 135 ~ that's 93%

ED$highlightcomp <- ifelse(rank(-ED$complist) <=20, "chosen", "other")
# Plot
G6 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightcomp)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
G6


# Chunk 7
# We are trying the brute force method now
# Select 20 estuaries at random from the list - tally up the spp in the 20 and record the number, reshuffle record the number if the number is higher, save the list, rinse and repeat until you find the highest number and then let's see
random_order <- sample(nrow(ED))
ED20 <- ED[order(random_order),]

maxspp <- 0 
div <- 0
randlist <- array(0, c(20))
randlist <- as.character(randlist)
for(i in 1:500000){
  div <- 0
  ED20 <- ED[sample(1:nrow(ED), 20, replace = FALSE),]
  for(i in 1:145){
    if(sum(ED20[,i+3])>0){
      div <- div+1
    }
  }
  if(div > maxspp){
      maxspp <- div
      randlist <- ED20[1:20, 1]
  }
}
maxspp/145
maxspp
randlist # <- c("Mlalazi", "Olifants", "Matigulu/Nyoni", "Groot (East)", "Klein Brak", "Manzimtoti", "Kwenxura", "Gqutywa", "Bot", "Gwaing" , "Kaaimans", "Mgwetyana", "Tyolomnqa", "Mvoti", "Mpenjati", "St Lucia", "Mapuzi", "Knysna", "Kwelera", "Zalu")
randout <- specnumber(ED[4:148], groups = ED$randlist)
randout
# Creating a mode list
ED$randlist <- 0
# List of locations to set modelist to 1
locations2 <- c("Mlalazi", "Olifants", "Matigulu/Nyoni", "Groot (East)", "Klein Brak", "Manzimtoti", "Kwenxura", "Gqutywa", "Bot", "Gwaing" , "Kaaimans", "Mgwetyana", "Tyolomnqa", "Mvoti", "Mpenjati", "St Lucia", "Mapuzi", "Knysna", "Kwelera", "Zalu")
# Set modelist to 1
ED <- ED %>%
  mutate(randlist = ifelse(ED$Estuary %in% locations2, 1, 0))


ED$highlightrand <- ifelse(rank(-ED$randlist) <=20, "chosen", "other")
# Plot
G7 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightrand)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7),
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
G7


# Chunk 8
# Run through the actual list and see how many spp it protects
# Creating a mode list
ED$DFFElist <- 0
# List of locations to set modelist to 1
locations3 <- c("Orange", "Spoeg", "Krom", "Heuningnes", "Klipdriftsfontein", "Goukou", "Swartvlei", "Goukamma", "Knysna", "Sout", "Groot (W)", "Bloukrans", "Lottering", "Elandsbos", "Storms", "Elands", "Groot (O)", "Sundays", "Mbashe", "Ku-Mpenzu", "Ku-Bhula", "Ntlonyane", "Msikaba", "Mtentu", "Mzamba", "Mpenjati", "Umhlangankulu", "Mgeni", "Mhlanga", "Mdloti", "Mlalazi", "St Lucia", "Kosi")
# Set modelist to 1
ED <- ED %>%
  mutate(DFFElist = ifelse(ED$Estuary %in% locations3, 1, 0))


ED$highlightDFFE <- ifelse(rank(-ED$DFFElist) <=20, "chosen", "other")
# Plot
G8 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightDFFE)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  theme(legend.title = element_text(size = 8),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  
        legend.text = element_text(size = 7), 
        panel.border=element_blank(), 
        panel.background=element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line())
G8


DFFEout <- specnumber(ED[4:148], groups = ED$DFFElist)
DFFEout # output: 0 = 127; 1 = 106 ~ 73%
locations3


# Chunk 9
# Compare all lists of 20 and see which combo is the best?
List <- ED[, c(1, 152, 153, 155, 158, 160, 162)]

# Calculate the frequency of each estuary being chosen
List$Frequency <- rowSums(List[, -1])

# Create a frequency table showing the estuaries and their corresponding frequencies
frequency_table <- List[, c("Estuary", "Frequency")]

# Sort the frequency table by the Frequency column in descending order (optional)
frequency_table <- frequency_table[order(-frequency_table$Frequency), ]

# Display the frequency table
print(frequency_table)

# Now with the DFFE List
List2 <- ED[, c(1, 152, 153, 155, 158, 160, 162, 164)]

# Calculate the frequency of each estuary being chosen
List2$Frequency <- rowSums(List2[, -1])

# Create a frequency table showing the estuaries and their corresponding frequencies
frequency_table2 <- List2[, c("Estuary", "Frequency")]

# Sort the frequency table by the Frequency column in descending order (optional)
frequency_table2 <- frequency_table2[order(-frequency_table2$Frequency), ]

# Display the frequency table
print(frequency_table2)


# I want to compare the DFFE list to the Complement list and add from there (also including alphalist to cross reference in case there are more than 5 overflow of complist)
List3 <- List2[, c(1, 2, 6, 8)]
List3$Frequency <- rowSums(List3[, -1])

# Create a frequency table showing the estuaries and their corresponding frequencies
frequency_table3 <- List3[, c("Estuary", "Frequency")]

# Sort the frequency table by the Frequency column in descending order (optional)
frequency_table3 <- frequency_table3[order(-frequency_table3$Frequency), ]

# Display the frequency table
print(frequency_table3)

# Separating the DFFE list from the others to see which ones the DFFE are protecting that do not appear in any of the other recommendations
# Identify the estuaries in the last list (List7)
last_list_estuaries <- List2$Estuary[List2$DFFElist == 1]

# Identify the estuaries in all the other lists (List1 to List6)
other_lists_estuaries <- List2$Estuary[rowSums(List2[, c("alphalist", "alphabz", "modelist", "clust", "randlist", "complist")]) > 0]


# Find the estuaries that are in the last list but not in any of the other lists
unique_last_list_estuaries <- setdiff(last_list_estuaries, other_lists_estuaries)

# Display the unique estuaries in the last list
print(unique_last_list_estuaries)

# And finding the common estuaries in both
# Find the estuaries that are in both the last list and any of the other lists
common_estuaries <- intersect(last_list_estuaries, other_lists_estuaries)

# Display the common estuaries
print(common_estuaries)


# Doing the same for the Complementary List and DFFE list
last_list_estuaries2 <- List3$Estuary[List3$DFFElist == 1]

# Identify the estuaries in all the other lists (List1 to List6)
other_lists_estuaries2 <- List3$Estuary[rowSums(List3[, c("alphalist", "complist")]) > 0]


# Find the estuaries that are in the last list but not in any of the other lists
unique_last_list_estuaries2 <- setdiff(last_list_estuaries2, other_lists_estuaries2)

# Display the unique estuaries in the last list
print(unique_last_list_estuaries2)

# And finding the common estuaries in both
# Find the estuaries that are in both the last list and any of the other lists
common_estuaries2 <- intersect(last_list_estuaries2, other_lists_estuaries2)

# Display the common estuaries
print(common_estuaries2)

# Working out how much we stand to protect by adding 5 more estuaries 
locations4 <- c("Orange", "Spoeg", "Krom", "Heuningnes", "Klipdriftsfontein", "Goukou", "Swartvlei", "Goukamma", "Knysna", "Sout", "Groot (W)", "Bloukrans", "Lottering", "Elandsbos", "Storms", "Elands", "Groot (O)", "Sundays", "Mbashe", "Ku-Mpenzu", "Ku-Bhula", "Ntlonyane", "Msikaba", "Mtentu", "Mzamba", "Mpenjati", "Umhlangankulu", "Mgeni", "Mhlanga", "Mdloti", "Mlalazi", "St Lucia", "Kosi", "Kwelera", "Matigulu/Nyoni", "Manzimtoti", "Tyolomnqa", "Great Fish")
ED <- ED %>%
  mutate(DFFE2list = ifelse(ED$Estuary %in% locations4, 1, 0))
DFFE2out <- specnumber(ED[4:148], groups = ED$DFFE2list)
DFFE2out # output: 0 = 117; 1 = 124 ~ 86%

locations5<- c("Orange", "Spoeg", "Krom", "Heuningnes", "Klipdriftsfontein", "Goukou", "Swartvlei", "Goukamma", "Knysna", "Sout", "Groot (W)", "Bloukrans", "Lottering", "Elandsbos", "Storms", "Elands", "Groot (O)", "Sundays", "Mbashe", "Ku-Mpenzu", "Ku-Bhula", "Ntlonyane", "Msikaba", "Mtentu", "Mzamba", "Mpenjati", "Umhlangankulu", "Mgeni", "Mhlanga", "Mdloti", "Mlalazi", "St Lucia", "Kosi", "Matigulu/Nyoni", "Mngazana", "Kariega", "Mtamvuna", "Mntafufu")
ED <- ED %>%
  mutate(DFFE3list = ifelse(ED$Estuary %in% locations5, 1, 0))
DFFE3out <- specnumber(ED[4:148], groups = ED$DFFE3list)
DFFE3out # output: 0 = 120; 1 = 117 ~81%

locations6<- c("Orange", "Spoeg", "Krom", "Heuningnes", "Klipdriftsfontein", "Goukou", "Swartvlei", "Goukamma", "Knysna", "Sout", "Groot (W)", "Bloukrans", "Lottering", "Elandsbos", "Storms", "Elands", "Groot (O)", "Sundays", "Mbashe", "Ku-Mpenzu", "Ku-Bhula", "Ntlonyane", "Msikaba", "Mtentu", "Mzamba", "Mpenjati", "Umhlangankulu", "Mgeni", "Mhlanga", "Mdloti", "Mlalazi", "St Lucia", "Kosi", "Matigulu/Nyoni", "Kwelera", "Mngazana", "Kariega", "Mntafufu")
ED <- ED %>%
  mutate(DFFE4list = ifelse(ED$Estuary %in% locations6, 1, 0))
DFFE4out <- specnumber(ED[4:148], groups = ED$DFFE4list)
DFFE4out # output: 0 = 117; 1 = 119 ~ 82%


# This was the first legend adjustments
theme(legend.title = element_blank(),
      legend.justification = c("left", "top"),
      panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
      axis.ticks.length=unit(-0.1, "cm"), 
      axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
      axis.line = element_line())

# Checking how many MPAs I have to create with complementarity to protect all spp
# Add a column to keep track of complementarity list
total_species <- colnames(ED)[4:ncol(ED)]  # Assuming species data starts at the 4th column
covered_species <- vector()  # To track species covered by the selected estuaries
selected_estuaries <- vector() 
NED2 <- ED 
NED2$complist2 <- 0

# Complementarity analysis to cover all species
while(length(covered_species) < length(total_species)) {
  # Order NED by some criteria, here assumed as alpha-diversity (number of species)
  NED2 <- NED2[order(-specnumber(NED2[, 4:ncol(NED2)])), ]
  
  # Mark the first estuary in the ordered list
  NED2$complist2[1] <- 1
  selected_estuaries <- c(selected_estuaries, NED2[1, 1])  # Assuming the first column is EstuaryID
  
  # Update the covered species list
  new_species <- which(NED2[1, 4:ncol(NED2)] > 0)
  covered_species <- unique(c(covered_species, colnames(NED2)[new_species + 3]))  # Offset by 3 for the metadata columns
  
  # Remove the species covered by the selected estuary from further consideration
  for (i in new_species) {
    NED2[, i + 3] <- 0  # Offset by 3 for the metadata columns
  }
  
  # Recalculate alpha-diversity
  NED2$alpha <- specnumber(NED2[, 4:ncol(NED2)])
}

# Output the number of estuaries needed to cover all species
num_estuaries_needed <- length(selected_estuaries)
selected_estuaries  # These are the estuaries selected to cover all species


# Assign the complist back to the original data frame
ED <- ED[order(-ED$kmWest),]
NED2 <- NED2[order(-NED2$kmWest),]
ED$complist2 <- NED2$complist2

# Check the complementarity output
compout <- specnumber(ED[, 4:148], groups = ED$complist2)
compout  # This should now cover all species

num_estuaries_needed  # Number of estuaries needed to cover all species


# Facet wrapping some shit
# Combine the plots into a single layout
#combined_plot <- (G1 + G2 + G3 + G4) / (G5 + G6 + G7 + G8)

# Display the combined plot
#print(combined_plot)
G9 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlight)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1")) +
  xlab("km East") +
  ylab ("Alpha Diversity")+
  labs(color = "Estuaries")+
  labs(title = "a. Alpha")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")),
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

G9
G10 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightBZ)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1")) +
  xlab("km East") +
  ylab ("Alpha Diversity")+
  labs(color = "Estuaries")+
  labs(title = "b. Alpha BZ")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G10
G11 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightmode)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  labs(title = "c. Mode")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G11
G12 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightclust)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  labs(title = "d. Spp Composition")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G12
G13 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightcomp)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  labs(title = "e. Complementary")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G13
G14 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightrand)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  labs(title= "f. Random")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G14
G15 <- ggplot(ED, aes(x = kmEast, y = alpha)) + 
  geom_point(aes(color = highlightDFFE)) +
  scale_color_manual(values = c("chosen" = "#A36DAD", "other" = "#A0D4A1"))+
  xlab("km East") +
  ylab ("Alpha Diversity") +
  labs(color = "Estuaries")+
  labs(title = "g. Existing MPAs")+
  theme(plot.title = element_text(size = 10),
        legend.title = element_text(size = 9),
        legend.position = "none",
        panel.border=element_blank(), panel.background=element_blank(), panel.grid = element_blank(), 
        axis.ticks.length=unit(-0.1, "cm"), 
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.line = element_line(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))
G15

empty_plot <- ggplot() + 
  theme_void() + 
  theme(
    legend.position = "none"  # No legend for the placeholder
  )


# Combine plots
combined_plot <- G9 + G10 +  G11 + G12 + G13 + G14 + G15 + 
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.justification = c(1, 0),# Adjust the justification to ensure it is at the bottom right
    legend.direction = "vertical",
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  # Add border to the legend
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm")  # Optional: Add some margin around the legend
  ) 


# Add a common title
#combined_plot <- combined_plot + plot_annotation(title = "Combined Estuary Plots with Common Legend")

# Display the combined plot
combined_plot

combined_plot2 <- (G9 | G10 | G11) / (G12 | G13 | G14 | empty_plot) +
  plot_layout(guides = "collect", widths = c(1, 1, 1, 1), heights = c(1, 1)) & 
  theme(
    legend.position = "bottomright",  # Position the legend at the bottom right
    legend.justification = "center",  # Center the legend horizontally
    legend.direction = "vertical",  # Arrange the legend items vertically
    legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  # Add border to the legend
    legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm")  # Optional: Add some margin around the legend
  )

combined_plot2


theme(
  legend.position = "bottom",  # Position the legend at the bottom
  legend.justification = c(1, 0),# Adjust the justification to ensure it is at the bottom right
  legend.direction = "vertical",
  legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),  # Add border to the legend
  legend.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm")  # Optional: Add some margin around the legend
)
