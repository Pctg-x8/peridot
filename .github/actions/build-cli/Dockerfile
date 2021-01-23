FROM rust:latest

ADD builder.sh /
RUN chmod +x /builder.sh

ENTRYPOINT ["bash", "-o", "pipefail", "-c", "/builder.sh 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"]
