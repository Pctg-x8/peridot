FROM rust:latest

ADD tester.sh /
ADD checker.sh /
ADD find-test-code.sh /
RUN chmod +x /tester.sh
RUN chmod +x /checker.sh
RUN chmod +x /find-test-code.sh

ENTRYPOINT ["sh", "-c", "/tester.sh 2>&1 | tee $GITHUB_WORKSPACE/.buildlog"]
