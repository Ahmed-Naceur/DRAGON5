#---------------------------------------------------------------------------
#
#  Makefile for building the PyGan extension modules
#  Author : A. Hebert (2020-7-3)
#
#---------------------------------------------------------------------------
#
OS = $(shell uname -s | cut -d"_" -f1)
ifeq ($(OS),AIX)
  DIRNAME = AIX
else
  DIRNAME = $(shell uname -sm | sed 's/[ ]/_/')
endif
pylib = $(shell python3 ../../script/find_pylib.py)
PYTHONPATH = $(pylib)/python
$(info set PYTHONPATH="$(PYTHONPATH)")
export PYTHONPATH

# Build defaults and phony targets
.DEFAULT_GOAL := all

ifeq ($(intel),1)
  ifeq ($(INTELTOOLS),)
    $(error INTELTOOLS is not set)
  endif
  COMPILER = INTELTOOLS
  lib = ../lib/$(DIRNAME)_intel
  libGan = ../../Ganlib/lib/$(DIRNAME)_intel
else
  ifeq ($(nvidia),1)
    ifeq ($(NVTOOLS),)
      $(error NVTOOLS is not set)
    endif
    COMPILER = NVTOOLS
    lib = ../lib/$(DIRNAME)_nvidia
    libGan = ../../Ganlib/lib/$(DIRNAME)_nvidia
  else
    ifeq ($(llvm),1)
      ifeq ($(LLVMTOOLS),)
        $(error LLVMTOOLS is not set)
      endif
      COMPILER = LLVMTOOLS
      lib = ../lib/$(DIRNAME)_llvm
      libGan = ../../Ganlib/lib/$(DIRNAME)_llvm
    else
      ifeq ($(FORTRANPATH),)
        $(error FORTRANPATH is not set)
      endif
      COMPILER = FORTRANPATH
      lib = ../lib/$(DIRNAME)
      libGan = ../../Ganlib/lib/$(DIRNAME)
    endif
  endif
endif
export COMPILER
#	export FORTRANPATH

all: donjon
checkPython: ; @which python3 > /dev/null
ganlib: sub-make-ganlib pygan-ganlib
ifeq ($(openmp),1)
	@echo 'pygan_ganlib: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'pygan_ganlib: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'pygan_ganlib: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'pygan_ganlib: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'pygan_ganlib: hdf5 is defined'
endif
trivac: sub-make-trivac pygan-trivac
ifeq ($(openmp),1)
	@echo 'pygan_trivac: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'pygan_trivac: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'pygan_trivac: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'pygan_trivac: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'pygan_trivac: hdf5 is defined'
endif
dragon: sub-make-dragon pygan-dragon
ifeq ($(openmp),1)
	@echo 'pygan_dragon: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'pygan_dragon: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'pygan_dragon: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'pygan_dragon: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'pygan_dragon: hdf5 is defined'
endif
donjon: sub-make-donjon pygan-donjon
ifeq ($(openmp),1)
	@echo 'pygan_donjon: openmp is defined'
endif
ifeq ($(intel),1)
	@echo 'pygan_donjon: intel is defined'
endif
ifeq ($(nvidia),1)
	@echo 'pygan_donjon: nvidia is defined'
endif
ifeq ($(llvm),1)
	@echo 'pygan_donjon: llvm is defined'
endif
ifeq ($(hdf5),1)
	@echo 'pygan_donjon: hdf5 is defined'
endif
sub-make-ganlib:
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Ganlib/src
sub-make-trivac: sub-make-ganlib
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) -C ../../Utilib/src
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Trivac/src
sub-make-dragon: sub-make-trivac
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Dragon/src
sub-make-donjon: sub-make-dragon
	$(MAKE) openmp=$(openmp) intel=$(intel) nvidia=$(nvidia) llvm=$(llvm) hdf5=$(hdf5) -C ../../Donjon/src
libGanlib.a: $(lib)/ sub-make-ganlib
	cp $(libGan)/libGanlib.a .
	ar -d libGanlib.a xabort_c.o
	mv libGanlib.a $(lib)
$(lib)/:
	mkdir -p $(lib)/
pygan-ganlib: libGanlib.a sub-make-ganlib checkPython
	mkdir -p $(DIRNAME)
	cp *.[ch] $(DIRNAME)
	cp *.py $(DIRNAME)
	cd $(DIRNAME); python3 setup_lcm.py install --home=.
	cd $(DIRNAME); python3 setup_lifo.py install --home=.
ifeq ($(openmp),1)
	export CODE_EMBEDDED=GANLIB_OMP; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
else
	export CODE_EMBEDDED=GANLIB; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
endif
	cp -R $(DIRNAME)/$(pylib)/* $(lib)/
	/bin/rm -rf $(DIRNAME)
pygan-trivac: libGanlib.a sub-make-trivac checkPython
	mkdir -p $(DIRNAME)
	cp *.[ch] $(DIRNAME)
	cp *.py $(DIRNAME)
	cd $(DIRNAME); python3 setup_lcm.py install --home=.
	cd $(DIRNAME); python3 setup_lifo.py install --home=.
ifeq ($(openmp),1)
	export CODE_EMBEDDED=TRIVAC_OMP; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
else
	export CODE_EMBEDDED=TRIVAC; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
endif
	cp -R $(DIRNAME)/$(pylib)/* $(lib)/
	/bin/rm -rf $(DIRNAME)
pygan-dragon: libGanlib.a sub-make-dragon checkPython
	mkdir -p $(DIRNAME)
	cp *.[ch] $(DIRNAME)
	cp *.py $(DIRNAME)
	cd $(DIRNAME); python3 setup_lcm.py install --home=.
	cd $(DIRNAME); python3 setup_lifo.py install --home=.
ifeq ($(openmp),1)
	export CODE_EMBEDDED=DRAGON_OMP; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
else
	export CODE_EMBEDDED=DRAGON; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
endif
	cp -R $(DIRNAME)/$(pylib)/* $(lib)/
	/bin/rm -rf $(DIRNAME)
pygan-donjon: libGanlib.a sub-make-donjon checkPython
	mkdir -p $(DIRNAME)
	cp *.[ch] $(DIRNAME)
	cp *.py $(DIRNAME)
	cd $(DIRNAME); python3 setup_lcm.py install --home=.
	cd $(DIRNAME); python3 setup_lifo.py install --home=.
ifeq ($(openmp),1)
	export CODE_EMBEDDED=DONJON_OMP; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
else
	export CODE_EMBEDDED=DONJON; cd $(DIRNAME); python3 setup_cle2000.py install --home=.
endif
	cp -R $(DIRNAME)/$(pylib)/* $(lib)/
	/bin/rm -rf $(DIRNAME)
	@echo 'makefile PYTHONPATH=' $(PYTHONPATH)
clean:
	@echo 'clean PyGan'
	$(MAKE) -C ../../Dragon/src clean
	$(MAKE) -C ../../Donjon/src clean
	/bin/rm -r -f ../lib
	/bin/rm -f sub-make*
