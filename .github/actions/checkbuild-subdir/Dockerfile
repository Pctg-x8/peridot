FROM rust:latest

ADD tester.sh /
ADD checker.sh /
ADD find-test-code.sh /
RUN chmod +x /tester.sh
RUN chmod +x /checker.sh
RUN chmod +x /find-test-code.sh

ENTRYPOINT ["/tester.sh"]
