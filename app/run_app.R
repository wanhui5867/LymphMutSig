# =============================================================================
# 启动脚本：运行 LymphMutSig Shiny 应用
# =============================================================================

# 检查并安装必要的包
required_packages <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "readr", "markdown")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if (length(missing_packages) > 0) {
  cat("正在安装缺失的包:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

# 启动应用（端口 3841，避免与 3838 冲突）
cat("正在启动 LymphMutSig 应用...\n")
cat("启动后请在浏览器打开: http://127.0.0.1:3841\n")
cat("或在 Cursor 中: Cmd+Shift+P -> Simple Browser: Show -> 输入上述地址\n")
shiny::runApp(port = 3841, launch.browser = TRUE)
