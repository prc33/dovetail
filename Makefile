BENCHMARKS = fib quicksort nqueens locks barrier rwlock blackscholes queue

RUNTIME	= work.c atomics.ll slow_queue.c slow_cell.c fast_queue.c fast_cell.c fast_mem.c util.ll arrays.c util_c.c benchmarks.c
BIN	= bin
BUILD	= build
BOEHM	= ~/boehm
WOOL	= ~/wool-0.1.5alpha

# This is just the default
DOVETAIL_DISABLEFAST=0

CLANG	= clang -g -S -I $(BOEHM)/include/ -O0 -emit-llvm -DDOVETAIL_DISABLEFAST=$(DOVETAIL_DISABLEFAST)
LINK	= llvm-link
OPT	= opt -instcombine -std-compile-opts -std-link-opts -O3
LLC	= llc -O3
ASM	= gcc
LIBS	= $(BOEHM)/lib/libgc.a -lpthread -lm
JCAMC	= $(BIN)/jcamc
CFA	= $(BIN)/analyse
CC	= gcc -std=c99 -O3 -Wall
CPP	= g++ -std=c++0x -O3 -Wall -pthread
WOOLC	= gcc -std=gnu99 -O3 -Wall -DWOOL -I $(WOOL) $(WOOL)/wool.o
JAVAC	= javac -d $(BIN)

RUNTIME_LLVM = $(addprefix runtime/,$(filter %.ll,$(RUNTIME)))
RUNTIME_C = $(filter %.c,$(RUNTIME))
RUNTIME_CO = $(addprefix $(BUILD)/runtime/,$(RUNTIME_C:.c=.ll))

BM_ALL  = $(BENCHMARKS) $(addsuffix _base,$(BENCHMARKS)) $(addsuffix _wool,$(BENCHMARKS)) $(addsuffix .class,$(BENCHMARKS)) fib_inline fib_inline2
BM_EXEC = $(addprefix $(BIN)/,$(BM_ALL))

all: benchmarks runtime compiler

benchmarks: $(BM_EXEC)

compiler: $(JCAMC)

runtime: $(BUILD)/runtime/runtime.bc

clean:
	rm -Rf $(BUILD)
	rm -Rf $(BIN)

clean-benchmarks:
	rm -Rf $(BUILD)/benchmarks
	rm -f $(BM_EXEC)
	mkdir $(BUILD)/benchmarks

$(BIN):
	mkdir $(BIN)

$(BUILD):
	mkdir $(BUILD)
	mkdir $(BUILD)/runtime
	mkdir $(BUILD)/benchmarks

$(JCAMC): compiler/*.ml | $(BUILD) $(BIN)
	ocamlbuild -I compiler -use-ocamlfind dovetail.native -package llvm -build-dir $(BUILD)
	ln -s -f ../$(BUILD)/compiler/dovetail.native $(JCAMC)

$(CFA): compiler/*.ml | $(BUILD) $(BIN)
	ocamlbuild -I compiler -use-ocamlfind analyse.native -package llvm -build-dir $(BUILD)
	ln -s -f ../$(BUILD)/compiler/analyse.native $(CFA)

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
	$(CC) $< $(LIBS) -o $@

$(BIN)/%_base: benchmarks/%.cpp | $(BIN)
	$(CPP) $< $(LIBS) -o $@

$(BIN)/%_wool: benchmarks/%.c | $(BIN)
	$(WOOLC) $< $(LIBS) -o $@

$(BIN)/%_wool: benchmarks/%.cpp | $(BIN)
	touch $@

$(BIN)/%.class: benchmarks/%.java | $(BIN)
	$(JAVAC) $<
