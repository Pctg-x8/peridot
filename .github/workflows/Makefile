
TARGET_WORKFLOWS = integrity-test weekly-integrity-test auto-delivery-dev docs-cd sdk-build
CONVERTER = dhall-to-yaml-ng

.SUFFIXES: .dhall .yml

default: all

.PHONY: all
all: $(addsuffix .yml,$(TARGET_WORKFLOWS))

.dhall.yml:
	$(CONVERTER) --file $^ > $@

.PHONY: clean
clean:
	rm -f $(addsuffix .yml,$(TARGET_WORKFLOWS))
