FROM alpine:latest

RUN apk --update add curl git jq
ADD runner.sh /
RUN chmod +x /runner.sh

ENTRYPOINT ["/runner.sh"]
