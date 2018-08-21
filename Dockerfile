FROM fpco/stack-build:lts-9.17 as build

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/hex
ARG BINARY_PATH
WORKDIR /opt/hex
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-9.17/8.0.2/bin .
CMD ["/opt/hex/hex-exe"]
