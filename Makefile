BENCHMARKS = fib quicksort

RUNTIME	= work.c atomics.ll trampoline.ll slow_queue.c slow_cell.c fast_queue.c fast_cell.c fast_mem.c main.ll arrays.c
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

.SECONDARY:

all: benchmarks runtime compiler

$(BIN):
	mkdir $(BIN)

$(BUILD):
	mkdir $(BUILD)
	mkdir $(BUILD)/runtime
	mkdir $(BUILD)/benchmarks

benchmarks: $(BM_EXEC) $(BM_BASE)

compiler: $(JCAMC)

runtime: $(BUILD)/runtime/runtime.bc

clean:
	rm -Rf $(BUILD)
	rm -Rf $(BIN)

$(JCAMC): compiler/*.ml | $(BUILD) $(BIN)
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
	gcc -std=c99 -O3 $< -o $@
