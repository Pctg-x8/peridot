
TARGET_WORKFLOWS = integrity-test weekly-integrity-test auto-delivery-dev docs-cd sdk-build
CONVERTER = dhall-to-yaml
DIFF = colordiff -u

.SUFFIXES: .dhall .yml .ymls

default: all

.PHONY: all
all: $(addsuffix .yml,$(TARGET_WORKFLOWS))

test-sync: $(addsuffix .ymls,$(TARGET_WORKFLOWS))

integrity-test.yml: integrity-test.dhall integrity-test/Common.dhall
	$(CONVERTER) --file integrity-test.dhall --output integrity-test.yml

weekly-integrity-test.yml: weekly-integrity-test.dhall integrity-test/Common.dhall
	$(CONVERTER) --file weekly-integrity-test.dhall --output weekly-integrity-test.yml

.dhall.yml:
	$(CONVERTER) --file $^ --output $@

.dhall.ymls:
	@echo ">> testing $^..."
	@$(CONVERTER) --file $^ --output $@
	@$(DIFF) $(patsubst %.ymls,%.yml,$@) $@ || (echo "!! Workflow $^ is not synchronized! !!"; rm -f $@; exit 1)
	@rm -f $@

.PHONY: clean
clean:
	rm -f $(addsuffix .yml,$(TARGET_WORKFLOWS))
