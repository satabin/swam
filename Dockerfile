FROM nightscape/scala-mill

ENV REPO_PATH_DOCKER=/home/swam/
ENV SWAM_OUTPUT_DOCKER=/home/out/
ENV WASM_PATH_DOCKER=/home/wasm/

# Create the appropriate directories
RUN mkdir $REPO_PATH_DOCKER
RUN mkdir $SWAM_OUTPUT
RUN mkdir $WASM_PATH_DOCKER

WORKDIR $REPO_PATH_DOCKER

# Copy over requirements first, so that they can be used from cache, 
# even if other files in the directory have been changed.
ADD ./build.sc $REPO_PATH_DOCKER/build.sc

# TODO: Test this
# Try running without copying over repo
RUN mill core.compile text.compile \
    generator.compile cli.compile \
    runtime.compile test.compile \
    optin.compile wasi.compile \
    examples.compile util.compile \
    benchmarks.compile

ADD . $REPO_PATH_DOCKER

RUN mill core.compile text.compile \
    generator.compile cli.compile \
    runtime.compile test.compile \
    optin.compile wasi.compile \
    examples.compile util.compile \
    benchmarks.compile

RUN chmod +x $REPO_PATH_DOCKER/entrypoint*
RUN chmod +x $REPO_PATH_DOCKER/fuzzer/entrypoint_mill_server.sh
