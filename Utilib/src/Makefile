#---------------------------------------------------------------------------
#
#  Makefile for building the Utilib library
#  Author : A. Hebert (2018-5-10) and C. Bienvenue (2025-11-04)
#
#---------------------------------------------------------------------------
#
ARCH = $(shell uname -m)
ifneq (,$(filter $(ARCH),aarch64 arm64))
  nbit =
else
  ifneq (,$(filter $(ARCH),i386 i686))
    nbit = -m32
  else
    nbit = -m64
  endif
endif

DIRNAME = $(shell uname -sm | sed 's/[ ]/_/')
OS = $(shell uname -s | cut -d"_" -f1)
opt = -O -g
.DEFAULT_GOAL := all
ifeq ($(openmp),1)
  COMP = -fopenmp
else
  COMP =
endif

ifeq ($(intel),1)
  fcompiler = ifort
  ccompiler = icc
else
  ifeq ($(nvidia),1)
    fcompiler = nvfortran
    ccompiler = nvc
  else
    ifeq ($(llvm),1)
      fcompiler = flang-new
      ccompiler = clang
    else
      fcompiler = gfortran
      ccompiler = gcc
    endif
  endif
endif

ifeq ($(OS),AIX)
  PY := python
else
  PY := $(shell command -v python3 2>/dev/null)
  ifeq ($(PY),)
    PY := $(shell command -v python 2>/dev/null)
  endif
endif

ifeq ($(OS),Darwin)
  ifeq ($(openmp),1)
    ccompiler = gcc-14
  endif
  F90 = $(fcompiler)
  C = $(ccompiler)
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
else
ifeq ($(OS),Linux)
  F90 = $(fcompiler)
  C = $(ccompiler)
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
else
ifeq ($(OS),CYGWIN)
  F90 = $(fcompiler)
  C = $(ccompiler)
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
else
ifeq ($(OS),SunOS)
  fcompiler =
  F90 = f90
  C = cc
  CFLAGS = $(nbit)
  FFLAGS = $(nbit) -s -ftrap=%none
  FFLAG77 = $(nbit) -s -ftrap=%none
else
ifeq ($(OS),AIX)
  fcompiler =
  opt = -O4
  DIRNAME = AIX
  F90 = xlf90
  C = cc
  CFLAGS = -qstrict
  FFLAGS = -qstrict -qmaxmem=-1 -qsuffix=f=f90
  FFLAG77 = -qstrict -qmaxmem=-1 -qxlf77=leadzero -qfixed
else
  $(error $(OS) is not a valid OS)
endif
endif
endif
endif
endif
ifeq ($(fcompiler),gfortran)
  ifneq (,$(filter $(ARCH),i386 i686 x86_64))
    summary =
  else
    summary = -ffpe-summary=none
  endif
  ifeq ($(OS),Darwin)
    summary = -ffpe-summary=none
  endif
  FFLAGS += -Wall $(summary)
  FFLAG77 += -Wall -frecord-marker=4 $(summary)
endif

ifeq ($(intel),1)
  FFLAGS = -fPIC
  FFLAG77 = -fPIC
  lib = ../lib/$(DIRNAME)_intel
  lib_module = ../lib/$(DIRNAME)_intel/modules
else
  ifeq ($(nvidia),1)
    lib = ../lib/$(DIRNAME)_nvidia
    lib_module = ../lib/$(DIRNAME)_nvidia/modules
  else
    ifeq ($(llvm),1)
      lib = ../lib/$(DIRNAME)_llvm
      lib_module = ../lib/$(DIRNAME)_llvm/modules
      FFLAGS += -mmlir -fdynamic-heap-array
      ifeq ($(OS),Darwin)
        LFLAGS += -lclang_rt.osx
        ifeq ($(openmp),1)
          LFLAGS += -lomp
        endif
      endif
    else
      lib = ../lib/$(DIRNAME)
      lib_module = ../lib/$(DIRNAME)/modules
    endif
  endif
endif

SRCC = $(shell ls *.c 2>/dev/null)
SRC77 = $(shell ls *.f 2>/dev/null)
SRCF77 = $(shell ls *.F 2>/dev/null)
SRC90 = $(shell $(PY) ../../script/make_depend.py *.f90 *.F90 2>/dev/null)
OBJC = $(SRCC:.c=.o)
OBJ90 = $(patsubst %.f90,%.o,$(patsubst %.F90,%.o,$(SRC90)))
OBJ77 = $(SRC77:.f=.o)
OBJF77 = $(SRCF77:.F=.o)

ifneq ($(filter Darwin Linux,$(OS)),)
# Auto-generated dependency file capturing Fortran module and include deps
DEPS_MK := .utilib_deps.mk
-include $(DEPS_MK)
$(DEPS_MK): $(SRC90) $(SRC77) $(SRCF77)
ifeq ($(PY),)
	@echo "# Dependencies not generated: no Python available" > $(DEPS_MK)
else
	@echo "# Generating Fortran dependencies into $(DEPS_MK)"; \
	$(PY) ../../script/make_depend.py --make-deps $(SRC90) $(SRC77) $(SRCF77) > $(DEPS_MK) || { rm -f $(DEPS_MK); exit 1; }
endif
else
  ifneq ($(findstring -j,$(MAKEFLAGS)),)
    .NOTPARALLEL:
    $(info Parallel build is not supported on $(OS). Forcing sequential build.)
  endif
endif

all : libUtilib.a
	@echo 'Utilib: fflags=' $(FFLAGS)
ifeq ($(openmp),1)
	@echo 'Utilib: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'Utilib: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'Utilib: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'Utilib: llvm is defined'
endif
	@echo "Utilib: python=" $(PY)
%.o : %.c 
	$(C) $(CFLAGS) $(FLAGS) $(opt) $(COMP) -c $< -o $@
%.o : %.f90 
	$(F90) $(FFLAGS) $(opt) $(COMP) $(INCLUDE) -c $< -o $@
%.o : %.F90 
	$(PREPRO) -P -W -traditional $(FLAGS) $< $(@:.o=.pp.f90)
	$(F90) $(FFLAGS) $(opt) $(COMP) $(INCLUDE) -c $(@:.o=.pp.f90) -o $@
	/bin/rm -f $(@:.o=.pp.f90)
%.o : %.f 
	$(F90) $(FFLAG77) $(opt) $(COMP) $(INCLUDE) -c $< -o $@
%.o : %.F 
	$(PREPRO) -P -W -traditional $(FLAGS) $(FMPI) $< $(@:.o=.pp.f)
	$(F90) $(FFLAG77) $(opt) $(COMP) $(INCLUDE) -c $(@:.o=.pp.f) -o $@
	/bin/rm -f $(@:.o=.pp.f)
all: $(lib_module)
$(lib_module)/:
	mkdir -p $(lib_module)/
$(lib)/: $(lib_module)/
	mkdir -p $(lib)/
ifneq ($(filter Darwin Linux,$(OS)),)
libUtilib.a: $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77) $(lib)/
	ar rcs $@ $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77)
	cp $@ $(lib)/$@
	cp *.mod $(lib_module)
else
libUtilib.a: $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77) $(lib)/
	ar r $@ $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77)
	cp $@ $(lib)/$@
	cp *.mod $(lib_module)
endif
clean:
	/bin/rm -f *.o *.mod *.a $(DEPS_MK) *.pp.f *.pp.f90 ._*
