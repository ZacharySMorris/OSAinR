#Loading in and assigning critical files#


#Load in Semaphoront x Developmental Event Matrix

#Need from Nexus & Other Phylogenetic Files

  OSA_Guide_Forward.phy <- read.nexus("OSAGuide.tre")

  OSA_Guide_Reverse.phy <- read.nexus("OSAGuide_reverse.tre")

#Need from excel, numbers, csv

  Color_Ontogeny.csv <- read.csv("OSAinR.csv", header = TRUE)
  rownames(Color_Ontogeny.csv) <- paste("Sm", Color_Ontogeny.csv[,1], sep="_")
  names(Color_Ontogeny.csv) <- paste("Ev", names(Color_Ontogeny.csv), sep="_")
  Color_Ontogeny.Matrix <- as.matrix(Color_Ontogeny.csv[,-1])

  Color_Ontogeny.df <- list()
  Color_Ontogeny.df$Matrix <- Color_Ontogeny.Matrix
  Color_Ontogeny.df$Semaphoronts <- rownames(Color_Ontogeny.Matrix)
  Color_Ontogeny.df$Events <- colnames(Color_Ontogeny.Matrix)
  Color_Ontogeny.df$colors <- paste(Color_Ontogeny.csv[,1])
  Color_Ontogeny.df$size <- rep(2, length(Color_Ontogeny.df$Semaphoronts) )

  save(Color_Ontogeny.Matrix, file="data/Color_Ontogeny_Matrix.RData")
  save(Color_Ontogeny.df, file="data/Color_Ontogeny_df.RData")
  Color_Ontogeny.df


  tmp_row <- Color_Ontogeny.csv[8,]
  row.names(tmp_row) <- "Sm_black"
  tmp_row[,1] <- "black"
  Color_Ontogeny.csv[9,] <- tmp_row

#Load in metadata file (e.g., number of individuals for each Sm, age of semaphoront, others)
