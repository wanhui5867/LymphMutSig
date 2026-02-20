# 在 Cursor 内置浏览器中打开 LymphMutSig

1. **先启动应用**（在终端中执行）：
   ```bash
   cd "项目路径/LymphMutSig"
   Rscript -e "options(shiny.launch.browser = FALSE); shiny::runApp(port = 3841)"
   ```
   看到 `Listening on http://127.0.0.1:3841` 即表示启动成功。

2. **用 Cursor 内置浏览器打开**：
   - 按 `Cmd+Shift+P`（Mac）或 `Ctrl+Shift+P`（Windows）打开命令面板
   - 输入并选择：**Simple Browser: Show**
   - 在地址栏输入：`http://127.0.0.1:3841` 回车

即可在 Cursor 内预览应用。
