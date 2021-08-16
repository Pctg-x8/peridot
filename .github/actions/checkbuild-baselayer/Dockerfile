FROM rust:latest

# あとでlatest自動取得とかしたいけど面倒だから一旦これでいいや
RUN curl -o /cargo-json-gha-translator -L https://github.com/Pctg-x8/cargo-json-gha-translator/releases/download/v0.1.3/cargo-json-gha-translator && chmod +x /cargo-json-gha-translator

ADD entrypoint.sh /
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]
