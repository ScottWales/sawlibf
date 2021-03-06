# Copyright 2012 ARC Centre of Excellence for Climate System Science
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

all:
.PHONY: all check clean doc
.SUFFIXES:

LD=$(FC)
MKDIR=mkdir -p

FFLAGS+=-warn all -warn errors -check all
FFLAGS+=-module include
FFLAGS+=-g -traceback
FFLAGS+=-fpp -Iinclude

CFLAGS+=-Wall -Werror
CFLAGS+=-MMD -MP

ALL_SRC:=$(shell find src -type f -not -iname '.*' 2> /dev/null)
BIN_SRC:=$(shell find src/bin -type f -not -iname '.*' 2> /dev/null)
TEST_SRC:=$(shell find src/test -type f -not -iname '.*' 2> /dev/null)

BIN:=$(patsubst src/bin/%,bin/%,$(basename $(BIN_SRC)))
TEST:=$(patsubst src/test/%,test/%,$(basename $(TEST_SRC)))
LIBS:=$(patsubst src/lib/%,lib/lib%.a,$(shell find src/lib -mindepth 1 -maxdepth 1 -type d 2> /dev/null))

F90_SRC=$(filter %.f90, $(ALL_SRC))
C_SRC=$(filter %.c, $(ALL_SRC))

all:$(BIN) $(TEST) $(LIBS)
check:$(TEST)
	failed=0;for test in $^; do ./$$test || failed=$$(($$failed+1)); done;\
	    if [ $$failed -gt 0 ]; then echo "$$failed tests failed"; exit 1; fi
clean:
	$(RM) -r build bin test doc lib
doc:$(ALL_SRC)

build/%.o:src/%.f90
	@$(MKDIR) $(dir $@)
	$(FC) $(FFLAGS) -c -o $@ $<
build/%.o:src/%.c
	@$(MKDIR) $(dir $@)
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

bin/%:build/bin/%.o $(LIBS)
	@$(MKDIR) $(dir $@)
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)
test/%:build/test/%.o $(LIBS)
	@$(MKDIR) $(dir $@)
	$(LD) $(LDFLAGS) -o $@ $^ $(LDLIBS)

libsrc=$(patsubst src/lib/%,build/lib/%.o,$(basename $(filter src/lib/$(1)/%,$(ALL_SRC))))
.SECONDEXPANSION:
lib/lib%.a:$$(call libsrc,%)
	@$(MKDIR) $(dir $@)
	$(AR) cr  $@ $^

build/%.d:src/%.f90 module_dependencies
	@$(MKDIR) $(dir $@)
	./module_dependencies $< > $@
-include $(patsubst src/%.f90,build/%.d,$(F90_SRC))
-include $(patsubst src/%.c,build/%.d,$(C_SRC))
