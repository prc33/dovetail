BENCHMARKS = fib

RUNTIME	= work.c atomics.ll trampoline.ll slow_queue.c fast_cell.c slow_cell.c main.ll
BIN	= bin
BUILD	= build
BOEHM	= ~/boehm

CLANG	= clang -S -I $(BOEHM)/include/ -O0 -emit-llvm
LINK	= llvm-link-3.2
OPT	= opt-3.2 -instcombine -std-compile-opts -std-link-opts -O3
LLC	= llc-3.2 -O3
ASM	= gcc -g
LIBS	= -lpthread $(BOEHM)/lib/libgc.so
JCAMC	= $(BIN)/jcamc

RUNTIME_LLVM = $(addprefix runtime/,$(filter %.ll,$(RUNTIME)))
RUNTIME_C = $(filter %.c,$(RUNTIME))
RUNTIME_CO = $(addprefix $(BUILD)/runtime/,$(RUNTIME_C:.c=.ll))

BM_EXEC = $(addprefix $(BIN)/,$(BENCHMARKS))
BM_BASE = $(addsuffix _base,$(BM_EXEC))

$(BIN):
	mkdir $(BIN)

$(BUILD):
	mkdir $(BUILD)
	mkdir $(BUILD)/runtime
	mkdir $(BUILD)/benchmarks

all: $(BM_EXEC) $(BM_BASE)

runtime: $(BUILD)/runtime/runtime.bc

clean:
	rm -Rf $(BUILD)
	rm -Rf $(BIN)

$(JCAMC): | $(BUILD) $(BIN)
	ocamlbuild -I compiler -use-ocamlfind dovetail.byte -package llvm -build-dir $(BUILD)
	ln -s -f ../$(BUILD)/compiler/dovetail.byte $(JCAMC)

$(BUILD)/runtime/%.ll: runtime/%.c | $(BUILD)
	$(CLANG) $< -o $@

$(BUILD)/runtime/runtime.bc: $(RUNTIME_LLVM) $(RUNTIME_CO) | $(BUILD)
	$(LINK) $(RUNTIME_LLVM) $(RUNTIME_CO) -o $@

$(BUILD)/benchmarks/%.bc: benchmarks/%.jcam $(JCAMC) | $(BUILD)
	$(JCAMC) $< $@

$(BUILD)/benchmarks/%.linked.bc: $(BUILD)/benchmarks/%.bc $(BUILD)/runtime/runtime.bc | $(BIN)
	$(LINK) $< $(BUILD)/runtime/runtime.bc -o $@

$(BUILD)/%.opt.ll: $(BUILD)/%.linked.bc
	$(OPT) $< -S -o $@

$(BUILD)/%.s: $(BUILD)/%.opt.ll
	$(LLC) $< -o $@	
	
$(BIN)/%: $(BUILD)/benchmarks/%.s | $(BIN)
	$(ASM) $< $(LIBS) -o $@

$(BIN)/%_base: benchmarks/%.c | $(BIN)
	gcc -O3 $< -o $@
