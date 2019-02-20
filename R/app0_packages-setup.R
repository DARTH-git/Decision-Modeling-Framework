#---------------------------------------------------------------------------------#
#### Appendix 0: Install required packages if not located in R's local library ####
#---------------------------------------------------------------------------------#
if (!require(dplyr)) {
  install.packages("dplyr")
}
if (!require(truncnorm)) {
  install.packages("truncnorm")
}
if (!require(lhs)) {
  install.packages("lhs")
}
if (!require(IMIS)) {
  install.packages("IMIS")
}
if (!require(matrixStats)) {
  install.packages("matrixStats")
}
if (!require(modeest)) {
  install.packages("modeest")
}
if (!require(plotrix)) {
  install.packages("plotrix")
}
if (!require(psych)) {
  install.packages("psych")
}
if (!require(scatterplot3d)) {
  install.packages("scatterplot3d")
}
if (!require(reshape2)) {
  install.packages("reshape2")
}
if (!require(devtools)) {
  install.packages("devtools")
}
if (!require(dampack)) {
  devtools::install_github(repo = "DARTH-git/dampack") # Install package from GitHub
}