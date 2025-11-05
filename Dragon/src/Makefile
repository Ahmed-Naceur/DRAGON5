#---------------------------------------------------------------------------
#
#  Makefile for building the Dragon library and load module
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
PREPRO = cpp
.DEFAULT_GOAL := all
ifeq ($(openmp),1)
  COMP = -fopenmp
  PREPRO = cpp -D_OPENMP
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
  FLAGS = -DLinux -DUnix
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
  LFLAGS = $(nbit)
else
ifeq ($(OS),Linux)
  F90 = $(fcompiler)
  C = $(ccompiler)
  FLAGS = -DLinux -DUnix
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
  LFLAGS = $(nbit)
else
ifeq ($(OS),CYGWIN)
  F90 = $(fcompiler)
  C = $(ccompiler)
  FLAGS = -DLinux -DUnix
  CFLAGS = -Wall $(nbit) -fPIC
  FFLAGS = $(nbit) -fPIC
  FFLAG77 = $(nbit) -fPIC
  LFLAGS = $(nbit)
else
ifeq ($(OS),SunOS)
  fcompiler =
  MAKE = gmake
  F90 = f90
  C = cc
  PREPRO = /usr/lib/cpp
  FLAGS = -DSunOS -DUnix
  CFLAGS = $(nbit)
  FFLAGS = $(nbit) -s -ftrap=%none
  FFLAG77 = $(nbit) -s -ftrap=%none
  LFLAGS = $(nbit)
else
ifeq ($(OS),AIX)
  fcompiler =
  opt = -O4
  MAKE = gmake
  DIRNAME = AIX
  F90 = xlf90
  C = cc
  FLAGS = -DAIX -DUnix
  CFLAGS = -qstrict
  FFLAGS = -qstrict -qmaxmem=-1 -qsuffix=f=f90
  FFLAG77 = -qstrict -qmaxmem=-1 -qxlf77=leadzero -qfixed
  LFLAGS = -qstrict -bmaxdata:0x80000000 -qipa
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
  libUtl = ../../Utilib/lib/$(DIRNAME)_intel
  libGan = ../../Ganlib/lib/$(DIRNAME)_intel
  libTri = ../../Trivac/lib/$(DIRNAME)_intel
  bin = ../bin/$(DIRNAME)_intel
  lib_module = ../lib/$(DIRNAME)_intel/modules
  INCLUDE = -I../../Ganlib/lib/$(DIRNAME)_intel/modules/
else
  ifeq ($(nvidia),1)
    lib = ../lib/$(DIRNAME)_nvidia
    libUtl = ../../Utilib/lib/$(DIRNAME)_nvidia
    libGan = ../../Ganlib/lib/$(DIRNAME)_nvidia
    libTri = ../../Trivac/lib/$(DIRNAME)_nvidia
    bin = ../bin/$(DIRNAME)_nvidia
    lib_module = ../lib/$(DIRNAME)_nvidia/modules
    INCLUDE = -I../../Ganlib/lib/$(DIRNAME)_nvidia/modules/
  else
    ifeq ($(llvm),1)
      lib = ../lib/$(DIRNAME)_llvm
      libUtl = ../../Utilib/lib/$(DIRNAME)_llvm
      libGan = ../../Ganlib/lib/$(DIRNAME)_llvm
      libTri = ../../Trivac/lib/$(DIRNAME)_llvm
      bin = ../bin/$(DIRNAME)_llvm
      lib_module = ../lib/$(DIRNAME)_llvm/modules
      INCLUDE = -I../../Ganlib/lib/$(DIRNAME)_llvm/modules/
      FFLAGS += -mmlir -fdynamic-heap-array
      ifeq ($(OS),Darwin)
        LFLAGS += -lclang_rt.osx
      endif
    else
      lib = ../lib/$(DIRNAME)
      libUtl = ../../Utilib/lib/$(DIRNAME)
      libGan = ../../Ganlib/lib/$(DIRNAME)
      libTri = ../../Trivac/lib/$(DIRNAME)
      bin = ../bin/$(DIRNAME)
      lib_module = ../lib/$(DIRNAME)/modules
      INCLUDE = -I../../Ganlib/lib/$(DIRNAME)/modules/
    endif
endif
endif

ifeq ($(hdf5),1)
  FLAGS += -DHDF5_LIB -I${HDF5_INC}
  FFLAGS += -I${HDF5_INC}
  LFLAGS += -L${HDF5_API} -lhdf5
endif

FFLAGS += -cpp

SRCC = $(shell ls *.c 2>/dev/null)
SRC77 = $(filter-out %.pp.f,$(shell ls *.f 2>/dev/null))
SRCF77 = $(shell ls *.F 2>/dev/null)
ifeq ($(PY),)
  $(warning No Python interpreter found; Fortran dependency ordering may be suboptimal)
  SRC90_RAW = $(shell ls *.f90 *.F90 2>/dev/null)
  SRC90 = $(filter-out %.pp.f90,$(SRC90_RAW))
else
  SRC90_RAW = $(shell $(PY) ../../script/make_depend.py *.f90 *.F90)
  SRC90 = $(filter-out %.pp.f90,$(SRC90_RAW))
endif
OBJC = $(SRCC:.c=.o)
OBJ90 = $(patsubst %.f90,%.o,$(patsubst %.F90,%.o,$(SRC90)))
OBJ77 = $(SRC77:.f=.o)
OBJF77 = $(SRCF77:.F=.o)

# Auto-generated dependency file capturing Fortran module and include deps
DEPS_MK := .dragon_deps.mk
-include $(DEPS_MK)
$(DEPS_MK): $(SRC90) $(SRC77) $(SRCF77)
ifeq ($(PY),)
	@echo "# Dependencies not generated: no Python available" > $(DEPS_MK)
else
	@echo "# Generating Fortran dependencies into $(DEPS_MK)"; \
	$(PY) ../../script/make_depend.py --make-deps $(SRC90) $(SRC77) $(SRCF77) > $(DEPS_MK) || { rm -f $(DEPS_MK); exit 1; }
endif

all : sub-make Dragon
ifeq ($(openmp),1)
	@echo 'Dragon: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'Dragon: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'Dragon: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'Dragon: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'Dragon: hdf5 is defined'
endif
sub-make:
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Utilib/src
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Ganlib/src
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Trivac/src
%.o : %.c
	$(C) $(CFLAGS) $(opt) $(COMP) -c $< -o $@
%.o : %.f90
	$(F90) $(FFLAGS) $(opt) $(COMP) $(INCLUDE) -c $< -o $@
%.o : %.F90
	# Use Fortran compiler's integrated preprocessor instead of cpp -traditional
	$(F90) $(FFLAGS) $(opt) $(COMP) $(INCLUDE) $(FLAGS) -c $< -o $@
%.o : %.f
	$(F90) $(FFLAG77) $(opt) $(COMP) $(INCLUDE) -c $< -o $@
%.o : %.F
	# Use Fortran compiler's integrated preprocessor only for .F (not for .f)
	$(F90) $(FFLAG77) -cpp $(opt) $(COMP) $(INCLUDE) $(FLAGS) -c $< -o $@
$(lib_module)/:
	mkdir -p $(lib_module)/
$(lib)/: $(lib_module)/
	mkdir -p $(lib)/
libDragon.a: $(DEPS_MK) $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77) $(lib)/
	ar rcs $@ $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77)
	cp $@ $(lib)/$@
	if ls *.mod 1> /dev/null 2>&1; then cp *.mod $(lib_module); fi
$(bin)/:
	mkdir -p $(bin)/
Dragon: libDragon.a DRAGON.o $(bin)/ sub-make
	$(F90) $(opt) $(COMP) DRAGON.o $(lib)/libDragon.a $(libTri)/libTrivac.a \
	$(libUtl)/libUtilib.a $(libGan)/libGanlib.a $(LFLAGS) -o Dragon
	cp $@ $(bin)/$@
clean:
	$(MAKE) -C ../../Trivac/src clean
	/bin/rm -f *.o *.mod *.a sub-make temp.* Dragon $(DEPS_MK) *.pp.f *.pp.f90
