# 把 LymphMutSig 推到你的 GitHub

本地已经完成：`git init`、首次提交（分支 `main`）。

## 1. 在 GitHub 上新建仓库

1. 打开 https://github.com/new  
2. **Repository name** 填：`LymphMutSig`（或你喜欢的名字）  
3. 选 **Public**，**不要**勾选 “Add a README”  
4. 点 **Create repository**

## 2. 添加远程并推送

在终端进入项目目录后执行（把 `YOUR_GITHUB_USERNAME` 换成你的 GitHub 用户名，例如 `huiwan5867`）：

```bash
cd /Users/hui.wan/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Documents/Project/Kataegis_2023/LymphMutSig

git remote add origin https://github.com/YOUR_GITHUB_USERNAME/LymphMutSig.git
git push -u origin main
```

若仓库名不是 `LymphMutSig`，把上面两处 `LymphMutSig` 都改成你的仓库名。

推送时按提示登录 GitHub（浏览器或 Personal Access Token）。

## 3. 以后更新代码

```bash
git add .
git commit -m "简短说明"
git push
```
