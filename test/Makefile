CC = gcc clang
CPP = g++ clang++
TEST = string lipsum

comp = $(firstword $(subst -, ,$*))
testFile = $(lastword $(subst -, ,$*))

allTests = $(subst $(_),_,$(TEST))

_ = $() $()
, := ,

test: $(foreach test,$(TEST),$(foreach cc,$(CC),test-$(cc)-$(test)) $(foreach cpp,$(CPP),test-$(cpp)-$(test))) test-all
.PHONY: test

test-all: $(foreach cc,$(CC),test-$(cc)-all) $(foreach cpp,$(CPP),test-$(cpp)-all)
.PHONY: test-all

.SECONDEXPANSION:

.PHONY: test-%
$(foreach test,$(TEST),$(foreach cc,$(CC),test-$(cc)-$(test))): test-%: main.c $$(testFile).h $$(testFile).o
	$(comp) -D FORMAT=\"%s\" -D TEST=$(testFile) -include $(testFile).h main.c $(testFile).o -o $(comp).out && \
	./$(comp).out | diff $(testFile).txt -
$(foreach test,$(TEST),$(foreach cpp,$(CPP),test-$(cpp)-$(test))): test-%: main.cpp $$(testFile).h $$(testFile).o
	$(comp) -D TEST=$(testFile) -include $(testFile).h main.cpp $(testFile).o -o $(comp).out && \
	./$(comp).out | diff $(testFile).txt -

$(foreach cc,$(CC),test-$(cc)-all): test-%-all: main.c $$(allTests).h $$(allTests).o
	$(comp) -D FORMAT=\"$(subst $(_),,$(patsubst %,\%s,$(TEST)))\" -D TEST="$(subst $(_),$(,),$(TEST))" -include $(allTests).h main.c $(allTests).o -o $(comp)-all.out && \
	bash -c "diff <(./$(comp)-all.out) <(cat $(foreach test,$(TEST),$(test).txt))"
$(foreach cpp,$(CPP),test-$(cpp)-all): test-%-all: main.cpp $$(allTests).h $$(allTests).o
	$(comp) -D TEST="$(subst $(_),<<,$(TEST))" -include $(allTests).h main.cpp $(allTests).o -o $(comp)-all.out && \
	bash -c "diff <(./$(comp)-all.out) <(cat $(foreach test,$(TEST),$(test).txt))"

$(foreach test,$(TEST),$(test).o): %.o: %.txt ../stringLinker
	../stringLinker $<
$(foreach test,$(TEST),$(test).h): %.h: %.txt ../stringLinker
	../stringLinker $<

$(allTests).o $(allTests).h: $$(foreach test,$$(TEST),$$(test).txt)
	../stringLinker $(foreach test,$(TEST),$(test).txt)

.PHONY: clean
clean:
	rm *.asm *.h *.o *.out

