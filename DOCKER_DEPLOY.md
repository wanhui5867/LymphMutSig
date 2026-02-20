# LymphMutSig – Docker 部署说明 (SciLifeLab Serve)

按 [SciLifeLab Serve Shiny 托管文档](https://serve.scilifelab.se/docs/application-hosting/shiny/) 组织，用于构建并发布 Docker 镜像。

## 目录结构

```
LymphMutSig/
├── app/
│   ├── app.R
│   ├── renv.lock    # 由 Step 1 生成
│   ├── data/
│   ├── www/
│   ├── scripts/
│   └── run_app.R
├── Dockerfile
├── init_renv.R
└── DOCKER_DEPLOY.md
```

## Step 1：生成 renv.lock

在项目根目录 `LymphMutSig/` 下执行（R 会从 `app/app.R` 等推断依赖并生成 `renv.lock`）：

```bash
cd /path/to/LymphMutSig
Rscript init_renv.R
```

或在 R 里：

```r
setwd("/path/to/LymphMutSig")
source("init_renv.R")
```

若已安装并启用 renv，也可在 R 中直接：

```r
install.packages("renv")
library(renv)
renv::snapshot(type = "implicit")
```

确保生成后 `app/renv.lock` 存在。

## Step 2：本地构建并测试镜像

安装 [Docker Desktop](https://www.docker.com/products/docker-desktop/) 并登录 Docker Hub（用户名：huiwan5867）。

在 **LymphMutSig 根目录** 执行：

```bash
cd /path/to/LymphMutSig
docker build --platform linux/amd64 -t huiwan5867/lymphmutsig:latest .
```

构建完成后本地运行测试：

```bash
docker run --rm -p 3838:3838 huiwan5867/lymphmutsig:latest
```

浏览器打开 http://localhost:3838 检查应用是否正常。

## Step 3：推送到 Docker Hub

```bash
docker push huiwan5867/lymphmutsig:latest
```

发布新版本时建议使用带日期的 tag，例如：

```bash
docker build --platform linux/amd64 -t huiwan5867/lymphmutsig:20250220 .
docker push huiwan5867/lymphmutsig:20250220
```

## Step 4–6：在 SciLifeLab Serve 上创建应用

1. 注册/登录 [SciLifeLab Serve](https://serve.scilifelab.se)。
2. 创建 Project。
3. 在项目里创建 Shiny App，填写：
   - **Name:** LymphMutSig
   - **Image:** `huiwan5867/lymphmutsig:20250220`（或你使用的 tag）
   - **Port:** 3838
   - **Source code URL:** 你的代码仓库或 Zenodo/Figshare 链接

应用将可通过 `https://<subdomain>.serve.scilifelab.se` 访问。

## 注意事项

- 每次更新代码并重新部署时，请使用**新的镜像 tag**（如日期），并在 Serve 的 app 设置里更新 Image 字段。
- 镜像需保持**公开**，Serve 会定期拉取。
- 若构建或运行报错，可先本地 `docker run` 查看日志排查。
