# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


cba <- function(dataset, partition1, partition2, K) {

  unique_labels_partition1=unique(partition1)
  unique_labels_partition2=unique(partition2)
  if(K<unique_labels_partition1 || K<unique_labels_partition2)
    error()
  d=as.matrix(dist(dataset))

  A = assossiation(partition1,partition2, K)
  R = constraint(A, partition1, partition2, d)
  G = mcopkmeans(dataset, R, K)

  return(G)
}
