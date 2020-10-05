
TARGET_WORKFLOWS = integrity-test weekly-integrity-test auto-delivery-dev docs-cd sdk-build
FORCE_DEPS = $(wildcard ./schemas/*.dhall) $(wildcard ./integrity-test/*.dhall)
CONVERTER = dhall-to-yaml-ng
DIFF = colordiff -u

.SUFFIXES: .dhall .yml .ymls

default: all

.PHONY: all
all: $(addsuffix .yml,$(TARGET_WORKFLOWS))

test-sync: $(addsuffix .ymls,$(TARGET_WORKFLOWS))

.dhall.yml: $(FORCE_DEPS)
	$(CONVERTER) --file $^ > $@

.dhall.ymls:
	@echo ">> testing $^..."
	@$(CONVERTER) --file $^ > $@
	@$(DIFF) $(patsubst %.ymls,%.yml,$@) $@ || (echo "!! Workflow $^ is not synchronized! !!"; rm -f $@; exit 1)
	@rm -f $@

.PHONY: clean
clean:
	rm -f $(addsuffix .yml,$(TARGET_WORKFLOWS))
