BENCHMARKS = fib quicksort

RUNTIME	= work.c atomics.ll trampoline.ll slow_queue.c slow_cell.c fast_queue.c fast_cell.c fast_mem.c main.ll arrays.c
BIN	= bin
BUILD	= build
BOEHM	= ~/boehm
WOOL	= ~/Desktop/wool-0.1.5alpha

CLANG	= clang -S -I $(BOEHM)/include/ -O0 -emit-llvm
LINK	= llvm-link-3.2
OPT	= opt-3.2 -instcombine -std-compile-opts -std-link-opts -O3
LLC	= llc-3.2 -O3
ASM	= gcc -g
LIBS	= -lpthread $(BOEHM)/lib/libgc.so
JCAMC	= $(BIN)/jcamc
CC	= gcc -std=c99 -O3 -Wall
WOOLC	= gcc -std=gnu99 -O3 -Wall -DWOOL -I $(WOOL) $(WOOL)/wool.o -lpthread

RUNTIME_LLVM = $(addprefix runtime/,$(filter %.ll,$(RUNTIME)))
RUNTIME_C = $(filter %.c,$(RUNTIME))
RUNTIME_CO = $(addprefix $(BUILD)/runtime/,$(RUNTIME_C:.c=.ll))

BM_ALL  = $(BENCHMARKS) $(addsuffix _base,$(BENCHMARKS)) $(addsuffix _wool,$(BENCHMARKS))
BM_EXEC = $(addprefix $(BIN)/,$(BM_ALL))

all: benchmarks runtime compiler

benchmarks: $(BM_EXEC)

compiler: $(JCAMC)

runtime: $(BUILD)/runtime/runtime.bc

clean:
	rm -Rf $(BUILD)
	rm -Rf $(BIN)

$(BIN):
	mkdir $(BIN)

$(BUILD):
	mkdir $(BUILD)
	mkdir $(BUILD)/runtime
	mkdir $(BUILD)/benchmarks

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

$(BUILD)/%.fixed.ll: $(BUILD)/%.opt.ll
	./fix-selects.sh $< $@

$(BUILD)/%.s: $(BUILD)/%.fixed.ll
	$(LLC) $< -o $@	
	
$(BIN)/%: $(BUILD)/benchmarks/%.s | $(BIN)
	$(ASM) $< $(LIBS) -o $@

$(BIN)/%_base: benchmarks/%.c | $(BIN)
	$(CC) $< -o $@

$(BIN)/%_wool: benchmarks/%.c | $(BIN)
	$(WOOLC) $< -o $@
