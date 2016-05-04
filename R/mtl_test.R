#################
### Calculate Mean trophic level based on diet proportions and a knowledge of
### which species are primary producers
###
### Gavin Fay, gfay@umassd.edu
### 
#################

# create some dummy diet data
# 10 species, first two are primary producers
isPP <- c(rep(1,2),rep(0,8))  
# create matrix of diet proportions: rows are predators, columns are prey
# This would eventually come from the diet output files from Atlantis
diet_prop <- matrix(0,nrow=10,ncol=10)
diet_prop[3,1:2] <- c(0.5,0.5)   #species 3 only eats primary producers
diet_prop[4,1:3] <- c(0.2,0.3,0.5)  #species 4 is an omnivore
# species 5-10 are carnivores, and also potentially cannibals
#for (isp in 5:10) diet_prop[isp,3:(isp-1)] <- rmultinom(1,100,prob=rep(1,length(3:(isp-1))))/100
for (isp in 5:10) diet_prop[isp,3:(isp)] <- rmultinom(1,100,prob=rep(1,length(3:(isp))))/100

#not tested, weird combinations of diet - there may be some where below code won't be able to solve.

#set up mean trophic level output
mtl <- rep(NA,10)
#primary prodcuers have MTL 1
mtl[isPP==1] <- 1

# Run while loop until mtl vector is filled in
while(any(is.na(mtl))) {
  print(mtl)
  # loop over species (order shouldn't matter)
  for (isp in 1:10) {
    # check if already calculated MTL for this species
    if(is.na(mtl[isp])) {
      # work out what it eats
      eaten <- which(diet_prop[isp,]>0)
      # check if it eats itself
      if (any(eaten==isp)) eaten <- eaten[-which(eaten==isp)]
      tl_temp <- mtl[eaten]
      #tl_temp <- mtl[diet_prop[isp,]>0]
      # Check whether we can calculate trophic level for this species yet
      # i.e. Must have already worked out MTL for all the prey before doing this
      if(!any(is.na(tl_temp))) {
        #Calculate Mean Trophic level for this species
        # MTL(i) = (1+ SUM(j!=i)(MTL(j)*Diet_Prop(i,j)) / (1 - Diet_Prop(i,i))
        mtl[isp] <- (1 + sum(mtl[-isp]*diet_prop[isp,-isp],na.rm=TRUE))/
          (1-diet_prop[isp,isp])
      }
    }
  }
}
print(mtl)

