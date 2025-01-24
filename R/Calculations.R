##Calculating Difference Matrix##

###Calculating Maturity Score##
Color_Ontogeny.Matrix
Color_Ontogeny.df$MaturityScore <- apply(Color_Ontogeny.df$Matrix, 1, sum)
###End

##
Sm_num <- length(Color_Ontogeny.df$Semaphoronts)
Ev_num <- length(Color_Ontogeny.df$Events)
##

###Calculating Pairwise Distances###
#Create matrix to contain the properly signed differences between semaphoronts
#DF=Color_Ontogeny.df
OSA_PairComp <- function(DF=OSA_DataFrame){
  Sm_num <- length(DF$Semaphoronts)
  Ev_num <- length(DF$Events)

Difference.Matrix = matrix (nrow = choose(Sm_num,2),
                          ncol = Ev_num+1,
                          dimnames = list( rep("Placeholder", choose(Sm_num,2)), c(DF$Events,"Distance"))
                            )
#Create vector of names for pairwise comparisons
RowNames.list = c()
for (i in 1:(Sm_num-1)){
  X <- paste(DF$Semaphoronts[i],
             DF$Semaphoronts[c((i+1):Sm_num)],
        sep = "_vs_")
  RowNames.list <- append(RowNames.list,  X)
}

#Give names to Difference Matrix
rownames(Difference.Matrix) <- RowNames.list
#Calculate the differences and fill event columns in Difference Matrix
for (i in 1:(Sm_num-1)){
  name <- DF$Semaphoronts[i]
  X <- RowNames.list[substring(RowNames.list, 1, nchar(name)) == name]
  if (i==Sm_num-1){
    Difference.Matrix[X,(1:Ev_num)] <- DF$Matrix[Sm_num,] - DF$Matrix[i,]
  }
  else{
    Difference.Matrix[X,(1:Ev_num)] <- sweep(DF$Matrix[c((i+1):Sm_num),], 2, DF$Matrix[i,]) #special apply that makes diff calculation across matrix by rows
  }
}


#Calculate absolute distances between pairs of semaphoronts
Difference.Matrix[,"Distance"] <- apply(X = abs(Difference.Matrix[,(1:Ev_num)]), 1, sum)

#Create Distance Matrix
Distance.list <- Difference.Matrix[,"Distance"]

odd_indexes<-seq(1,2*length(Distance.list),2)
even_indexes<-seq(2,2*length(Distance.list),2)

Distance.Matrix = matrix (nrow = length(Distance.list),
                            ncol = 3,
                            dimnames = list( c(1:length(Distance.list)), c("From","To","Length"))
)

Distance.Matrix[,1] <- c(unlist(strsplit(rownames(Difference.Matrix),"_vs_"))[odd_indexes])
Distance.Matrix[,2] <- c(unlist(strsplit(rownames(Difference.Matrix),"_vs_"))[even_indexes])
Distance.Matrix[,3] <- Difference.Matrix[,"Distance"]

#Create blank comparison matrix to contain absolute distance and direction of differences
Comparison.Matrix = matrix (nrow = Sm_num,
                            ncol = Sm_num,
                            dimnames = list(DF$Semaphoronts, DF$Semaphoronts)
)

Comparison.Matrix[lower.tri(Comparison.Matrix)] <- Distance.list
Comparison.Matrix[upper.tri(Comparison.Matrix)]<-""
diag(Comparison.Matrix) <- "-"

#output
out <- list(Difference.Matrix=Difference.Matrix, Distance=Distance.list, Distance.Matrix=as.data.frame(Distance.Matrix), Comparison.Matrix=as.data.frame(Comparison.Matrix))
}

###End


###Infer Necessary Hypothetical Intermediate Semaphoronts
Distance.Matrix[order(Distance.Matrix)]

Difference.Matrix[order(Distance.Matrix),]

#Split up names of semaphoront pairs into groups based on how distant they are
Distance_Groups <- split(RowNames.list, Distance.Matrix)

Difference.Matrix[Distance_Groups$`2`,]

Color_Ontogeny.df$Matrix

gsub("Sm_", "", Color_Ontogeny.df$Semaphoronts)

MxMn.RowNames.list[grep("blue", strsplit(MxMn.RowNames.list, split = "_") )]

RowNames.list == name

Difference.Matrix[Distance_Groups[[1]][1],]


##going to give it a min_pair list, so will use that
test2_obj #OSA_PairComp output
nn_test2 #next_nearest output

#Create empty matrice and give names for Max and Min hypothetical intermediate semaphoronts for all step distances
MxMn.list = c()
for (i in 1:length(Distance_Groups)){
  NAME <- paste(names(Distance_Groups)[i], "steps", sep = "_")
  X <- paste("Mx", gsub("Sm_", "", Distance_Groups[[i]]), sep = "_")
  Y <- paste("Mn", gsub("Sm_", "", Distance_Groups[[i]]), sep = "_")
  Z <- c(X,Y)[order(c(seq_along(X),seq_along(Y)))]
  MxMn.list[[NAME]] <- matrix(nrow = length(Z),
                                    ncol = Ev_num,
                                    dimnames = list( Z, Color_Ontogeny.df$Events)
                                    )
}

#Fill matrix for each set of hypothetical intermediates
for (i in 1:length(MxMn.list)){
  N <- nrow(MxMn.list[[i]])
  THING <- Color_Ontogeny.df$Matrix[unlist(strsplit(Distance_Groups[[i]], split = "_vs_")),]

  ODDS <- seq(from = 1, to = nrow(THING), by = 2)
  EVENS <- seq(from = 2, to = nrow(THING), by = 2)
  ROW_PAIRS <- cbind(ODDS,EVENS)

  X <- NULL

  for (g in 1:(N/2)){
    X <- rbind(X, apply(X = THING[ROW_PAIRS[g,],], 2, max))
    X <- rbind(X, apply(X = THING[ROW_PAIRS[g,],], 2, min))
  }

  MxMn.list[[i]][1:N,1:Ev_num] <- X
}

Combined_MxMn_Real_Matrix <- Color_Ontogeny.df$Matrix
for (i in 1:length(MxMn.list)){
  Combined_MxMn_Real_Matrix <- rbind(Combined_MxMn_Real_Matrix,MxMn.list[[i]])
}

All_Semaphoronts.df <- list()

All_Semaphoronts.df$Matrix <- Combined_MxMn_Real_Matrix[!duplicated(Combined_MxMn_Real_Matrix),]
All_Semaphoronts.df$colors <- c(Color_Ontogeny.df$colors, rep('black', nrow(Combined_MxMn_Real_Matrix) - length(Color_Ontogeny.df$colors) ) )
All_Semaphoronts.df$size <- c(Color_Ontogeny.df$size, rep(1, nrow(Combined_MxMn_Real_Matrix) - length(Color_Ontogeny.df$size) ) )

All_Semaphoronts.df$MaturityScore <- apply(All_Semaphoronts.df$Matrix, 1, sum)
All_Semaphoronts.df$order <- c(1:length(All_Semaphoronts.df$MaturityScore))

All_Semaphoronts.df$PlottingValues <- OSA_PlottingValues(All_Semaphoronts.df)

###End


###Calculating Positions on X axis to plot points
###
# Needs to find the number of semaphoronts, both real and hypothesized,
# and place the one on the most common pathway at x=1 and
# the rest spaced evenly across the rest of the x axis
###

### NEED TO UPDATE WITH ADDITION OF HYPOTHETICAL INTERMEDIATES
### AND FINDING WHICH ONE IS ON MODAL SEQUENCE

OSA_PlottingValues <- function(OSA_DataFrame){

  MS <- OSA_DataFrame$MaturityScore

  #Split maturity scores and names into a list of different vectors based on maturity scores
  MaturityScore_names <- split(names(MS), MS)
  MaturityScore <- split(MS, MS)

  #Create list to fill with X positions
  X_position <- list()

  #Calculate X positions
  for (i in 1:length(MaturityScore_names)){
    X <- MaturityScore[[i]]

    #Calculate length of duplicates
    dup.length <- length(X)

    #Determine order
    ORDER <- order(X)

    #Multiply by order and add 1 to get appropriate spread of points
    X_position[[i]] <- ((( (Sm_num + 2) / dup.length ) * (ORDER - 1) ) + 1)
  }

  #unsplit list of X positions and re-assign semaphoront names
  X_position_unsplit <- unsplit(X_position, OSA_DataFrame$MaturityScore)
  names(X_position_unsplit) <- names(OSA_DataFrame$MaturityScore)

  #Unsplit list of Maturity Scores and re-assign semaphoront names
  MaturityScore_unsplit <- unsplit(MaturityScore, OSA_DataFrame$MaturityScore)
  names(MaturityScore_unsplit) <- names(OSA_DataFrame$MaturityScore)

  #Output Values
  out <- list(X = X_position_unsplit, Y = MaturityScore_unsplit)

}

###End
