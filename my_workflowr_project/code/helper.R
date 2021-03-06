# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

# PCA input check function
.inputChecks <- function(pca, colour_by){

  # Test the pca input
  if (!is(pca, "prcomp")){
    stop("'pca' is not of type 'prcomp'")
  }

  # colour_by check
  if (nrow(pca$x) != length(colour_by)){
    stop("'colour_by' not of the same length as data points in 'pca'")
  }
}

# Function that plots a PCA
library(ggplot2)
my_plotPCA <- function(pca, colour_by){

  # Input checks
  .inputChecks(pca, colour_by)

  cur_df <- data.frame(PC1 = pca$x[,1],
                       PC2 = pca$x[,2],
                       colour = colour_by)

  # Calculate variance explained
  cur_var <- pca$sdev ^ 2
  cur_var_expl <- (cur_var / sum(cur_var)) * 100
  cur_var_expl <- round(cur_var_expl, digits = 2)

  # Plot the PCA
  ggplot(cur_df) +
    geom_point(aes(x = PC1, y = PC2, colour = colour)) +
    xlab(paste0("PC1 ", cur_var_expl[1], "% var explained")) +
    ylab(paste0("PC2 ", cur_var_expl[2], "% var explained"))
}
