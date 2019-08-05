#!/usr/bin/bash
date
# nohup 本地投递绘图脚本
Rscript PCA_v5.R
sleep 3
echo "PCA plot is OK"
