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

ENV DOCKER_SWAM_SRC=/home/swam
ENV DOCKER_SWAM_OUTPUT=/home/out
ENV DOCKER_WASM=/home/wasm

# Create the appropriate directories
RUN mkdir $DOCKER_SWAM_SRC
RUN mkdir $DOCKER_SWAM_OUTPUT
RUN mkdir $DOCKER_WASM

WORKDIR $DOCKER_SWAM_SRC

# TODO: Find way of installing dependencies with Mill without copying over entire repo
# See: https://stackoverflow.com/questions/62834693/mill-build-tool-install-dependencies-without-compiling-source-code

ADD . $DOCKER_SWAM_SRC

# As long as I cannot separate installing dependencies and compiling 
# src, this will be done in the entrypoint, so volumes can be used:

# For maven data:
VOLUME /root/.cache/coursier/v1/https/repo1.maven.org/maven2
# For compiled sources:
VOLUME $DOCKER_SWAM_SRC/out/

RUN chmod +x $DOCKER_SWAM_SRC/entrypoint_mill_server.sh
