SMLNJ_HOME := $(dir $(shell which sml))
SMLNJ_HEAP_SUFFIX := $(shell echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null)

TIGER_CM := tiger.cm
TIGER_HEAP := tiger.$(SMLNJ_HEAP_SUFFIX)

all: $(TIGER_HEAP)

$(TIGER_HEAP): $(TIGER_CM)
	ml-build $(TIGER_CM) Main.main

.PHONY: clean
clean:
	rm -f $(TIGER_HEAP)
	rm -Rf .cm

.PHONY: install
install: $(TIGER_HEAP)
	cp $(TIGER_HEAP) $(SMLNJ_HOME).heap
	(cd $(SMLNJ_HOME) ; ln -s .run-sml tiger)

.PHONY: uninstall
uninstall:
	rm -f $(SMLNJ_HOME).heap/$(TIGER_HEAP)
	rm -f $(SMLNJ_HOME)tiger
