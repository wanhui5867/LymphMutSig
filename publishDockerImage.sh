#1. open Docker desktop 
#2. create the Dockerfile in the parent directory of shiny app

#3. create the app image using below commands
#docker buildx rm lymphmutsig
#docker buildx create --name lymphmutsig
#docker buildx use lymphmutsig
#docker buildx inspect --bootstrap
#docker buildx build --tag 20250220 -o type=image --platform=linux/amd64 .
#docker buildx build --push --tag huiwan5867/lymphmutsig:20250220 --platform=linux/amd64 .

#docker run --rm  --platform linux/amd64  -p 3838:3838 huiwan5867/lymphmutsig:20250220

docker build --platform linux/amd64 -t huiwan5867/lymphmutsig:v1 .
docker image ls
docker push huiwan5867/lymphmutsig:v1
docker run --rm -p 3838:3838 huiwan5867/lymphmutsig:v1