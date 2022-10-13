## Docker Image
### Building the Image
```bash
cd docker
docker-compose build
```

This builds an image with the name `807615458658.dkr.ecr.us-west-2.amazonaws.com/vizlab/flow-tiles:latest`. It also will tag this with the name `flow-tiles`, allowing for convenient local runs a la `docker run -it flow-tiles bash`.

### Pushing the Image to ECR
```bash
aws ecr get-login-password --region us-west-2 | docker login --username AWS --password-stdin 807615458658.dkr.ecr.us-west-2.amazonaws.com
cd docker
docker-compose push 
# line above will throw an error when it tries to push the short name
```

Image will now be available as `807615458658.dkr.ecr.us-west-2.amazonaws.com/vizlab/flow-tiles`.

## ECS Setup
* Created a "network-only" cluster named `vizlab-cluster` on ECS.
* Copied the task definition for fetch-daily-obs-task
