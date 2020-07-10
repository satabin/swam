###### Adjusted from nightscape/scala-mill ######

FROM openjdk:12

# Env variables
ENV SCALA_VERSION 2.13.2
ENV MILL_VERSION 0.7.0

# Define working directory
WORKDIR /root

# Scala expects this file
# RUN touch /usr/lib/jvm/java-8-openjdk-amd64/release

# Install Scala
## Piping curl directly in tar
RUN \
  curl -fsL https://downloads.typesafe.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz | tar xfz - -C /root/ && \
  echo >> /root/.bashrc && \
  echo "export PATH=~/scala-$SCALA_VERSION/bin:$PATH" >> /root/.bashrc

# Install mill
RUN \
  curl -L -o /usr/local/bin/mill https://github.com/lihaoyi/mill/releases/download/$MILL_VERSION/$MILL_VERSION && \
  chmod +x /usr/local/bin/mill && \
  touch build.sc && \
  mill -i resolve _ && \
  rm build.sc

###### Actual Project ######

ENV REPO_PATH_DOCKER=/home/swam
ENV SWAM_OUTPUT_DOCKER=/home/out
ENV WASM_PATH_DOCKER=/home/wasm

# Create the appropriate directories
RUN mkdir $REPO_PATH_DOCKER
RUN mkdir $SWAM_OUTPUT_DOCKER
RUN mkdir $WASM_PATH_DOCKER

WORKDIR $REPO_PATH_DOCKER

# TODO: Find way of installing dependencies with Mill without copying over entire repo

ADD . $REPO_PATH_DOCKER

# As long as I cannot separate installing dependencies and compiling 
# src, this will be done in the entrypoint, so volumes can be used:
# RUN mill core.compile text.compile \
#     generator.compile cli.compile \
#     runtime.compile test.compile \
#     optin.compile wasi.compile \
#     examples.compile util.compile 
#     # Excluding benchmarks

RUN chmod +x $REPO_PATH_DOCKER/fuzzer/entrypoint_mill_server.sh
