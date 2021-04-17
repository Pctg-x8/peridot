FROM rust:latest

ADD entrypoint.sh /
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["bash", "-o", "pipefail", "-c", "/entrypoint.sh 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"]
