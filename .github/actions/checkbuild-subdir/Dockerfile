FROM rust:latest

ADD tester.sh /
ADD checker.sh /
RUN chmod +x /tester.sh
RUN chmod +x /checker.sh

ENTRYPOINT ["/tester.sh"]
