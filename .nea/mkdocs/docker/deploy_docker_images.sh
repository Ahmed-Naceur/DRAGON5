REGISTRY=docker.oecd-nea.org
PROJECT_PATH=dragon/5.1

VERSION='alpine'
IMAGE_NAME='python'
docker build -f Dockerfile.mkdocs -t "${REGISTRY}/${PROJECT_PATH}/${IMAGE_NAME}:${VERSION}" .
echo "${REGISTRY_TOKEN}" | docker login "${REGISTRY}" --username "${REGISTRY_USER}" --password-stdin
docker push "${REGISTRY}/${PROJECT_PATH}/${IMAGE_NAME}:${VERSION}"


