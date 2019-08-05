########################################################################################################
# Rscript function: Plotting comparison groups of Differentially expressed protein (DEP) Volcano Plot
# Author: jingxinxing
# Date: 2019/08/04
# Version: v2
# Usage: 1.进行差异蛋白筛选；得到"预处理数据.xlsx"和之前已准备的"sample_information.xlsx"两个文件就可以
#       进行火山图的绘制了；
#        2.复制绘图R脚本和投递命令shell脚本到绘图的当前目录内，与以上涉及的两个文件在同一个目录内；
#        3.修改投递命令shell脚本中的FC筛选倍数（所谓传参数），例如：FC=1.2、FC=1.5和FC=2，修改好后保存；
#        4.执行绘图命令sh runvolcano.sh 或bash runvolcano.sh即可完成火山图的绘制；
#        5.绘图脚本读取的是预处理数据中的各个比较组数据，所以有多少个比较组（如果有样品重复）就会有对应
#       的火山图被绘制和保存。
########################################################################################################
pt=proc.time()
#################////////////////////**************************\\\\\\\\\\\\\\\\\\\\\####################

## 1.设置当前的工作目录
setwd("./")

## 2.导入R包
anp <- c("openxlsx", "ggplot2", "argparse")
sapply(anp, library, character.only = TRUE)

## 3.数据读取、处理及绘图，最后保存火山图数据
# fc = 1.2 ## 用参数解析器解决这个问题
parser = ArgumentParser()
parser$add_argument("--project_path", help="/path/to/proteome/project/working_directory", required = TRUE)
parser$add_argument("--fc", help = "Fold change of comparison groups differentially expressed protein", required = TRUE)
args <- parser$parse_args()
str(args)

project_path = args$project_path
fc = args$fc

fc <- as.numeric(fc)

midatafn <- list.files(path = "./", pattern = "^预处理数据*")
print(midatafn)

samfn <- list.files(path = "./", pattern = "^sample*")
print(samfn)

sample_info_1 <- read.xlsx("sample_information.xlsx", sheet = "样品信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
sample_info_2 <- read.xlsx("sample_information.xlsx", sheet = "比较组信息", startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)

class(sample_info_1)
class(sample_info_2)
dim(sample_info_1)
dim(sample_info_2)

sample_info_1
sample_info_2

samp_numb <- sample_info_1$样品编号
print(samp_numb)

compa_groups <- 1:length(sample_info_2$比较组)
print(compa_groups)

for (i in compa_groups) {
  class(gsub("/","_",sample_info_2[i,1]))
  new_compa_groups <- gsub("/","_",sample_info_2[i,1])
  class(new_compa_groups)
  print(new_compa_groups)
  data <- read.xlsx(midatafn, sheet = new_compa_groups, startRow = 1, colNames = TRUE, rowNames = FALSE, detectDates = FALSE)
  dim(data)
  class(data)
  rownames(data) <- data[,1]
  afp_col <- c("Accession", "P-value",	"FC")
  afp_index <- match(afp_col, colnames(data))
  new_data <- data[,afp_index]
  colnames(new_data) <- c("Accession", "P_value", "FC")

  if ("P_value" %in% colnames(new_data) == TRUE) {
    new_data$threshold[new_data$P_value <= 0.05 & new_data$FC > fc] = "Up"
    new_data$threshold[new_data$P_value <= 0.05 & new_data$FC < 1/fc] = "Down"
    new_data$threshold[new_data$P_value > 0.05 & (new_data$FC >= 1/fc | new_data$FC <= fc)] = "Non"
    new_data$threshold[new_data$P_value < 0.05 & (new_data$FC >= 1/fc & new_data$FC <= fc)] = "Non"
    # new_data  <- new_data[complete.cases(new_data),]
    # new_data <- na.omit(new_data)
    # dim(new_data)

    ggplot(new_data, aes(x = log2(FC), y = -log10(P_value), colour = threshold)) +
      xlab("log2(Fold Change)") +
      ylab("-log10(P_value)") +
      geom_point() +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.margin = unit(rep(2,4), 'lines'), aspect.ratio = 1) + theme(legend.title = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_text(size=14), axis.title.y=element_text(size=14)) +
      geom_hline(aes(yintercept = -log10(0.05)), colour = "blue", size = 0.5, linetype = 2) +
      geom_vline(aes(xintercept = log2(fc)), colour = "blue", size = 0.5, linetype = 2) +
      geom_vline(aes(xintercept = log2(1/fc)), colour = "blue", size = 0.5, linetype = 2) +
      annotate("text",x=0.7, y=-log10(0.03), label="P_value=0.05") +
      annotate("text",x=log2(1/fc), y=6.2, label=paste0("FC≈",round(1/fc, 3))) +
      annotate("text",x=log2(fc), y=6.2, label=paste0("FC=",fc))
    ggsave(paste0("volcano_",i,"_",new_compa_groups,".pdf"), width = 12, height = 7.5, units = "in")
    ggsave(paste0("volcano_",i,"_",new_compa_groups,".png"), width = 12, height = 7.5, units = "in")
    write.xlsx(new_data, file = paste0("volcano_",i,"_",new_compa_groups,"_data.xlsx"))

  } else {
    print("Attention!")
    print("Your sample has not been duplicated, so the p value cannot be calculated. So rscript cannot plotting volcano plot.")

  }
}

#################////////////////////**************************\\\\\\\\\\\\\\\\\\\\\####################
print("输出数据处理和绘图所需R包名称、版本和环境变量信息：")
sessionInfo()
print("脚本运行时间为：")
proc.time()-pt
