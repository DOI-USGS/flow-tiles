ARG DOCKER_BASE_IMAGE=eddelbuettel/r2u:focal
FROM ${DEVOPS_REGISTRY}${DOCKER_BASE_IMAGE}

# Disable the annoying bell on WSL2
RUN sed -i 's/^# set bell-style none$/set bell-style none/' /etc/inputrc
RUN echo 'set visualbell' >> /root/.vimrc

# Add DOI CA to local CAs so that SSL can work over VPN
COPY docker/DOIRootCA2.crt /usr/local/share/ca-certificates
RUN update-ca-certificates
ENV CURL_CA_BUNDLE /etc/ssl/certs/ca-certificates.crt

RUN install2.r \
  cowplot \
  dataRetrieval \
  geofacet \
  ggfx \
  lubridate \
  magick \
  maps \
  rsvg \
  showtext \
  svglite \
  targets \
  tidyverse \
  xml2 \
  && rm -rf /tmp/*

# Use UTC instead of local time zones
ENV TZ=Etc/UTC

# expose information about the build environment within containers
ARG DOCKER_BUILD_REPO
ENV DOCKER_BUILD_REPO ${DOCKER_BUILD_REPO}
ARG DOCKER_BUILD_COMMIT_HASH
ENV DOCKER_BUILD_COMMIT_HASH ${DOCKER_BUILD_COMMIT_HASH}

# suppress an annoying message from bspm
RUN mkdir /usr/local/lib/R/etc/
RUN echo 'options(bspm.sudo = TRUE)' >> /usr/local/lib/R/etc/Rprofile.site


# Copy repo into image, so the image contains the code it needs to run on ECS.
# Note: here / is root of build context, NOT file system.
COPY / ./
