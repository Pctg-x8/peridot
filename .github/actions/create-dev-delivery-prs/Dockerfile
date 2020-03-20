FROM alpine:latest

RUN apk --update add curl git
ADD runner.sh /
RUN chmod +x /runner.sh

ENTRYPOINT ["/runner.sh"]
