# Load the count data
a <- read.delim("C:/Users/91957/OneDrive/Desktop/count.txt.txt", row.names = 1)

# Data preprocessing
a <- a[, -1]  # Remove the first column (if necessary)
name <- colnames(a)
name1 <- rownames(a)
sizeFactors <- colSums(a)
columnsums <- apply(a, 2, sum)

# Calculate Counts Per Million (CPM)
cpm <- apply(a, 2, function(x) (x / sum(x) * 10^6))

# Define a log transformation function
logtransform <- function(cpm) {
  cpm1 <- log2(cpm + 1)
  return(cpm1)
}

# Apply log transformation to the CPM data
cpm2 <- logtransform(cpm)
dummy <- cpm2

# Calculate Z scores
calculateZ_SCORE <- function(dummy) {
  Tmean <- apply(dummy, 1, mean)
  TSD <- apply(dummy, 1, sd)
  z_score <- dummy
  for (i in 1:nrow(dummy)) {
    z_score[i, 1:ncol(z_score)] <- (dummy[i, 1:ncol(dummy)] - Tmean[i] / TSD[i])
  }
  return(z_score)
}

# Compute Z-scores for the transformed data
zmatrix <- calculateZ_SCORE(dummy)
new_matrix <- zmatrix[!rowSums(is.na(zmatrix)),]
new_matrix1 <- new_matrix[1:100,]

# Visualize the heatmap of the transformed data
heatmap(new_matrix1, Rowv = NULL, Colv = NULL, col = heat.colors(256), margins = c(5, 10))

# Load required libraries for ComplexHeatmap and circlize
library("ComplexHeatmap")
library("circlize")

# Define colors for the heatmap
colors <- c(seq(-5, 0, length = 100), seq(0, 5, length = 100), seq(5, 15, length = 100))

# Select a subset of the matrix for visualization
new2 <- new_matrix1[1:10,]

# Create the heatmap with colorRamp2 function
Heatmap(new2, col = colorRamp2(c(-2, 0, 2), colors = c("orange", "white", "purple")))

# Task 1: Calculate the total variance for each row in cpm2
total_variance <- apply(cpm2, 1, var)

# Task 2: Sort the total variance in decreasing order and select the top 100 rows
new_variance <- sort(total_variance, decreasing = TRUE)
sel <- names(new_variance)[1:100]
cpm_new <- cpm2[sel,]

# Task 3: Calculate the Z scores for the selected rows
calculateZ_SCORE1 <- function(cpm_new) {
  Tmean1 <- apply(cpm_new, 1, mean)
  tsd1 <- apply(cpm_new, 1, sd)
  z_score1 <- cpm_new
  for (i in 1:nrow(cpm_new)) {
    z_score1[i, 1:ncol(z_score1)] <- (cpm_new[i, 1:ncol(cpm_new)] - Tmean1[i]) / tsd1[i]
  }
  return(z_score1)
}

zscore_new <- calculateZ_SCORE1(cpm_new)

# Task 4: Create the heatmap for the Z scores
Heatmap(zscore_new, col = colorRamp2(c(-2, 0, 2), colors = c("orange", "white", "purple")))

# Load annotation data
anno <- read.csv("C://Users//91957//OneDrive//Desktop//meta.csv")
names(anno) 

# Color scales for Age, Gender, and X columns
col1 <- list(call = colorRamp2(c(-2, 0, 2), colors = c("orange", "white", "purple")),
             agee = colorRamp2(c(-2, 0, 2), colors = c("orange", "white", "purple")),
             Gender = c("Female" = "red", "Male" = "blue"))

# Annotation data
ann <- data.frame(call = anno$x, Age = anno$Age, Gender = anno$Gender)

# Create heatmap annotation for Age, Gender, and X with color scales
ha <- HeatmapAnnotation(df = ann, col = col1)

# Create the heatmap with annotations and legends
pdf(file= "C://Users//91957//OneDrive//Desktop//myplot.pdf", width = 35, height = 15)
Heatmap(zscore_new, top_annotation = ha)

# Save the heatmap with annotations and legends
dev.off()
