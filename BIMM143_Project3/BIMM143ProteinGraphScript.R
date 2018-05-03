#' ---
#' title: "BIMM 143 Protein Analysis Script"
#' author: "Jacob Gerzenshtein"
#' date: "Spring 2018"
#' ---

#  ABOUT THE FUNCTION:
#  this function takes an input of a four letter
#  protein code from PDB, reads it, trims it down, 
#  creates a dataframe with ATOM, and finally plots
#  the protein's B factor versus the residue

#  HOW TO USE THE FUNCTION:
#  1 Find a protein of interest
#  2 Enter it into the PDB site
#  3 Find the 4 letter code associated with the desired protein 
#  4 In console, type showmewhatyouplot3("x")
#  5 Replace the x with the four letter code, making sure to keep the " "

#  OUTPUT OF FUNCTION: 
#  The output of the function is a plot of the B Factor (y) versus 
#  residue number (x). The B Factor represents the fluctuation of atoms 
#  from their average positions due to thermal/kinetic motion and is 
#  important for visualization of protein dynamics 

showmewhatyouplot3 <- function(x) {

  
  #install the package needed for protein data visualization
  install.packages("bio3d")
  
  
  #make sure the entire toolset of the package is loaded
  library(bio3d)
  
  #read the PDB file of the specified protein residue
  read <-  read.pdb(x)
  
  #trim down the db file to a specific chain and atom type
  trim <-  trim(read, chain = "A", elety = "CA")
  
  #create a dataframe for the ATOM file
  df <- trim$atom$b
  
  #create a plot of the protein residue with a specific type and axis labels 
  p <- plotb3(df, sse = trim, 
              type = "b", 
              ylab = "B Factor", 
              top = FALSE, 
              bot = FALSE)
  
}
