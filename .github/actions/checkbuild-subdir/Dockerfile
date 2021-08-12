FROM rust:latest

# あとでlatest自動取得とかしたいけど面倒だから一旦これでいいや
RUN curl -o /cargo-json-gha-translator -L https://github.com/Pctg-x8/cargo-json-gha-translator/releases/download/v0.1.2/cargo-json-gha-translator && chmod +x /cargo-json-gha-translator

ADD tester.sh /
ADD checker.sh /
ADD find-test-code.sh /
RUN chmod +x /tester.sh
RUN chmod +x /checker.sh
RUN chmod +x /find-test-code.sh

ENTRYPOINT ["/tester.sh"]
