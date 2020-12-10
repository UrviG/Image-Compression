# Load necessary libraries.
library(jpeg)
library(RCurl)


# Get image form url.
url <- "https://jpeg.org/images/jpeg-home.jpg"
img_file <- getURLContent(url, binary = TRUE)
img <- readJPEG(img_file)
writeJPEG(img, "original.jpeg")
img_dim <- dim(img)


# Extract rgb values for PCA.
r <- img[,,1]
g <- img[,,2]
b <- img[,,3]


# Convert image to data frame for clustering.
img_df <- data.frame(
  x=rep(1:img_dim[2], each=img_dim[1]),
  y=rep(img_dim[1]:1, img_dim[2]),
  r.value=as.vector(r),
  g.value=as.vector(g),
  b.value=as.vector(b)
)


# PCA on rgb values.
r_pca <- prcomp(r, center = FALSE)
g_pca <- prcomp(g, center = FALSE)
b_pca <- prcomp(b, center = FALSE)
rgb_pca <- list(r_pca, g_pca, b_pca)


# Set values for k and number of principle components.
k_values <- c(20, 10, 5, 3, 2)
pc_values <- c(100, 50, 20, 10, 5)


# Get results and write to file.
sink("results.txt")

cat("Original image size = ", file.size("original.jpeg")/1000, " kB\n") # Get size of original image.

cat("\nPCA: \n")
for(pc in pc_values) {
  pca.img <- sapply(rgb_pca, function(j) {
    compressed.img <- j$x[,1:pc] %*% t(j$rotation[,1:pc])
  }, simplify = "array")
  writeJPEG(pca.img, "pca.jpeg")
  cat("\t", pc, " PC, size = ", file.size("pca.jpeg")/1000, " kB\n")
}

cat("\nK-means: \n")
for(k in k_values) {
  kMeans <- kmeans(img_df[, c("r.value","g.value","b.value")],
                   centers = k)
  clusterColours <- kMeans$centers[kMeans$cluster,]
  dim(clusterColours) <- img_dim
  writeJPEG(clusterColours, "cluster.jpeg")
  cat("\tk = ", k, ", size = ", file.size("cluster.jpeg")/1000, "kB\n")
}

sink()