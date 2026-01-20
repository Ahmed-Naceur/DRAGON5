#---------------------------------------------------------------------------
#
#  Makefile for building the Ganlib library and load module
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
PREPRO = cpp
ifeq ($(openmp),1)
  COMP = -fopenmp
  PREPRO = cpp -DOPENMP
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

ifeq ($(mpi),1)
  FMPI = -DMPI
  fcompiler = mpif90
  ccompiler = mpicc
else
  FMPI =
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
  DIRNAME = AIX
  F90 = xlf90
  C = cc
  FLAGS = -DF90 -DAIX -DUnix
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
  FFLAGS += -Wall -Wno-maybe-uninitialized $(summary)
  FFLAG77 += -Wall -frecord-marker=4 $(summary)
endif

ifeq ($(intel),1)
  FFLAGS = -fPIC
  FFLAG77 = -fPIC
  lib = ../lib/$(DIRNAME)_intel
  bin = ../bin/$(DIRNAME)_intel
  lib_module = ../lib/$(DIRNAME)_intel/modules
else
  ifeq ($(nvidia),1)
    lib = ../lib/$(DIRNAME)_nvidia
    bin = ../bin/$(DIRNAME)_nvidia
    lib_module = ../lib/$(DIRNAME)_nvidia/modules
  else
    ifeq ($(llvm),1)
      lib = ../lib/$(DIRNAME)_llvm
      bin = ../bin/$(DIRNAME)_llvm
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
      bin = ../bin/$(DIRNAME)
      lib_module = ../lib/$(DIRNAME)/modules
    endif
  endif
endif

ifeq ($(hdf5),1)
  FLAGS += -DHDF5_LIB -I${HDF5_INC}
  FFLAGS += -I${HDF5_INC}
  LFLAGS += -L${HDF5_API} -lhdf5
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
DEPS_MK := .ganlib_deps.mk
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

all : sub-make Ganlib
ifeq ($(mpi),1)
	@echo 'Ganlib: mpi is defined'
endif
ifeq ($(openmp),1)
	@echo 'Ganlib: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'Ganlib: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'Ganlib: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'Ganlib: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'Ganlib: hdf5 is defined'
endif
	@echo 'Ganlib: python=' $(PY)
sub-make:
%.o : %.c
	$(C) $(CFLAGS) $(FLAGS) $(opt) $(COMP) -c $< -o $@
%.o : %.f90
	$(F90) $(FFLAGS) $(opt) $(COMP) -c $< -o $@
%.o : %.F90
	$(PREPRO) -P -W -traditional $(FLAGS) $< $(@:.o=.pp.f90)
	$(F90) $(FFLAGS) $(opt) $(COMP) -c $(@:.o=.pp.f90) -o $@
	/bin/rm -f $(@:.o=.pp.f90)
%.o : %.f
	$(F90) $(FFLAG77) $(opt) $(COMP) -c $< -o $@
%.o : %.F
	$(PREPRO) -P -W -traditional $(FLAGS) $(FMPI) $< $(@:.o=.pp.f)
	$(F90) $(FFLAG77) $(opt) $(COMP) -c $(@:.o=.pp.f) -o $@
	/bin/rm -f $(@:.o=.pp.f)
$(lib_module)/:
	mkdir -p $(lib_module)/
ifneq ($(filter Darwin Linux,$(OS)),)
libGanlib.a: $(DEPS_MK) $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77) $(lib_module)/
	ar rcs $@ $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77)
	cp $@ $(lib)/$@
	if ls *.mod 1> /dev/null 2>&1; then cp *.mod $(lib_module); fi
else
libGanlib.a: $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77) $(lib_module)/
	ar r $@ $(OBJC) $(OBJ90) $(OBJ77) $(OBJF77)
	cp $@ $(lib)/$@
	if ls *.mod 1> /dev/null 2>&1; then cp *.mod $(lib_module); fi
endif
$(bin)/:
	mkdir -p $(bin)/
Ganlib: libGanlib.a GANMAIN.o $(bin)/
	$(F90) $(opt) $(COMP) GANMAIN.o $(lib)/libGanlib.a $(LFLAGS) -o Ganlib
	cp $@ $(bin)/$@
clean:
	/bin/rm -f *.o *.mod *.a sub-make temp.* Ganlib* $(DEPS_MK) *.pp.f *.pp.f90 ._*
