FROM matsubara0507/ubuntu-for-haskell:git
ENV LANG C.UTF-8
RUN apt-get update && apt-get install -y rsync

ARG local_bin_path
WORKDIR /work
COPY ${local_bin_path} /usr/local/bin
ENTRYPOINT ["site"]
