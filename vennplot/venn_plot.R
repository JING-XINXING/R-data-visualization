###############################################
# script function: drawing data sets of venn plot
# date: 2019/05/14
# author: jingxinxing
# mail: xxjing1991@163.com 
# or dulang2011@yeah.net
###############################################
## 1.Install R Packages: VennDiagram
install.packages("VennDiagram")
## 2.Library R Packages
library("VennDiagram")
## 3.Set current working directory
setwd("./")
## 4.Read test data: proteome of data.frame that size 756 * 2 (2 row and 756 column), put the test data into current working directory
venn_data <- read.table("venn_data.csv", header = T, sep = ',', stringsAsFactors = F, check.names = F) 

## 4.Determine and plot based on the size of the data set
if (length(colnames(venn_data)) == 2){
  venn_list <- list(venn_data[,1], venn_data[,2])
  names(venn_list) <- colnames(venn_data)
  venn.diagram(venn_list, filename = 'venn_two_sets_plot.tiff', imagetype = 'tiff', margin = 0.1, fill = c('red', 'blue'), alpha = 0.50, col = 'black', cex = 1, fontfamily = 'serif', cat.col = c('black', 'black'), cat.cex = 1, cat.fontfamily = 'serif', cat.default.pos = "outer")

}else if (length(colnames(venn_data)) == 3){
  venn_list <- list(venn_data[,1], venn_data[,2], venn_data[,3])
  names(venn_list) <- colnames(venn_data)
  venn.diagram(venn_list, filename = 'venn_three_sets_plot.tiff', imagetype = 'tiff', margin = 0.1, fill = c('red', 'blue', 'green'), alpha = 0.50, col = 'black', cex = 1, fontfamily = 'serif', label.col = c("darkred", "white", "darkblue", "white", "white", "white", "darkgreen"), col = "transparent", cat.cex = 1, cat.fontfamily = 'serif', cat.default.pos = "text", cat.pos = 0)
}else if (length(colnames(venn_data)) == 4){
  venn_list <- list(venn_data[,1], venn_data[,2], venn_data[,3], venn_data[,4])
  names(venn_list) <- colnames(venn_data)
  venn.diagram(venn_list, filename = "Venn_four_sets_plot.tiff", col = "black", lty = "dotted", lwd = 3, fill = c("cornflowerblue", "green", "yellow", "darkorchid1"), alpha = 0.50, label.col = c("orange", "white", "darkorchid4", "white", "white", "white", "white", "white", "darkblue", "white", "white", "white", "white", "darkgreen", "white"), cex = 2.0, fontfamily = "serif", fontface = "bold", cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"), cat.cex = 1.8, cat.fontface = "bold", cat.fontfamily = "serif")
}else if (length(colnames(venn_data)) == 5){
  venn_list <- list(venn_data[,1], venn_data[,2], venn_data[,3], venn_data[,4], venn_data[,5])
  names(venn_list) <- colnames(venn_data)
  venn.diagram(venn_list, filename = "venn_five_sets_plot.tiff", lty = "dotted", lwd = 2, col = "black", fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"), alpha = 0.60, cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"), cat.cex = 0.8, cat.fontface = "bold", margin = 0.07, cex = 0.8)
}

## 5.Display R package and environment information for drawing
sessionInfo()
