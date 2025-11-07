#
# python3 setup_cle2000.py install --home=.
#
from sys import version_info
if version_info[0] == 3 and version_info[1] >= 12:
  from setuptools import setup, Extension
elif version_info[0] > 3:
  from setuptools import setup, Extension
else:
  from distutils.core import setup, Extension
import sys
import os

def _detect_gfortran_dir():
  import subprocess, os
  try:
    out = subprocess.check_output(['gfortran', '-print-file-name=libgfortran.dylib'], text=True).strip()
    if out and out != 'libgfortran.dylib':
      d = os.path.dirname(out)
      if os.path.isdir(d):
        return d
  except Exception:
    pass
  return None

def _has_any(paths):
  for p in paths:
    if any(os.path.exists(pfx) for pfx in _expand_suffixes(p)):
      return True
  return False

def _expand_suffixes(base):
  # Consider common library suffixes across UNIX
  # Static is last resort; we prefer shared for Python extension link
  exts = ['.dylib', '.so', '.a']
  return [base + ext for ext in exts]

def _detect_flang_dir():
  """Best-effort detection of the flang/Fortran runtime library directory on UNIX.
  Priority order:
  1) FLANGLIB or FLANG_LIBDIR (explicit path)
  2) From LLVMTOOLS (../lib or ../lib64)
  3) llvm-config --libdir
  4) flang-new location heuristic (bin -> lib or sibling llvm lib)
  5) Homebrew (macOS): brew --prefix flang, then /opt/homebrew/opt/flang/lib
  6) Common system locations (/usr/lib, /usr/lib64, /usr/local/lib, /opt/local/lib)
  Returns the first directory that appears to contain flang/Fortran runtime libs.
  """
  import subprocess
  candidates = []

  # 1) Explicit overrides
  for env_name in ('FLANGLIB', 'FLANG_LIBDIR'):
    d = os.environ.get(env_name)
    if d and os.path.isdir(d):
      candidates.append(d)

  # 2) Derive from LLVMTOOLS (often a bin path)
  t = os.environ.get('LLVMTOOLS')
  if t:
    for sub in ('..',):
      for libname in ('lib', 'lib64'):
        cand = os.path.abspath(os.path.join(t, sub, libname))
        if os.path.isdir(cand):
          candidates.append(cand)

  # 3) llvm-config --libdir
  try:
    libdir = subprocess.check_output(['llvm-config', '--libdir'], text=True).strip()
    if os.path.isdir(libdir):
      candidates.append(libdir)
  except Exception:
    pass

  # 4) Locate flang-new executable and probe nearby
  try:
    fl = subprocess.check_output(['which', 'flang-new'], text=True).strip()
    if fl and os.path.isabs(fl):
      base = os.path.dirname(os.path.dirname(fl))  # prefix
      for libname in ('lib', 'lib64'):
        cand = os.path.join(base, libname)
        if os.path.isdir(cand):
          candidates.append(cand)
  except Exception:
    pass

  # 5) macOS Homebrew
  if sys.platform == 'darwin':
    try:
      prefix = subprocess.check_output(['brew', '--prefix', 'flang'], text=True).strip()
      cand = os.path.join(prefix, 'lib')
      if os.path.isdir(cand):
        candidates.append(cand)
    except Exception:
      pass
    for cand in ('/opt/homebrew/opt/flang/lib', '/usr/local/opt/flang/lib'):
      if os.path.isdir(cand):
        candidates.append(cand)

  # 6) Fallback common UNIX lib dirs
  for cand in ('/usr/lib', '/usr/lib64', '/usr/local/lib', '/opt/local/lib'):
    if os.path.isdir(cand):
      candidates.append(cand)

  # Validate candidates by presence of known Fortran runtime libs
  def looks_valid(d):
    probes = [
      os.path.join(d, 'libFortranRuntime'),
      os.path.join(d, 'libFortranDecimal'),
      os.path.join(d, 'libflang_rt.runtime'),
      os.path.join(d, 'libflang'),
    ]
    return _has_any(probes)
  
  seen = set()
  for d in candidates:
    if not d or d in seen:
      continue
    seen.add(d)
    if looks_valid(d):
      return d
  return None

def _compute_flang_link_args(libdir):
  """Return a list of extra link args appropriate for the platform and runtime present.

  We prefer the new LLVM flang runtime (FortranRuntime/FortranDecimal). If not present,
  fall back to classic flang libraries when detected. On macOS, add clang runtime only when needed.
  """
  args = []
  def haslib(name):
    if not libdir:
      return False
    return _has_any([os.path.join(libdir, name)])

  if haslib('libFortranRuntime') and haslib('libFortranDecimal'):
    args += ['-lFortranRuntime', '-lFortranDecimal']
  elif haslib('libflang_rt.runtime') and haslib('libFortranDecimal'):
    args += ['-lflang_rt.runtime', '-lFortranDecimal']
  elif haslib('libflang'):
    args += ['-lflang']
    if haslib('libflangrti'):
      args += ['-lflangrti']
  else:
    args += []

  if sys.platform == 'darwin':
    args += ['-lclang_rt.osx']
  return args

def _compute_openmp_link_args(compiler):
  """Return OpenMP runtime link args based on the compiler suite.

  - LLVMTOOLS -> libomp (and Homebrew paths on macOS)
  - default (gfortran) -> libgomp
  """
  args = []
  if compiler == "LLVMTOOLS":
    # LLVM OpenMP
    if sys.platform == 'darwin':
      # Add Homebrew libomp search and rpath paths for both arm64 and x86_64
      for p in ('/opt/homebrew/opt/libomp/lib', '/usr/local/opt/libomp/lib'):
        args += ['-L'+p, '-Wl,-rpath,'+p]
    args += ['-lomp']
  else:
    # GCC toolchain
    args += ['-lgomp']
  return args

def main():
  import os
  from sysconfig import get_config_var
  mach = os.path.basename(os.getcwd())
  Code = os.environ.get("CODE_EMBEDDED", None) # Code selection
  Compiler = os.environ.get("COMPILER", None) # Compiler selection
  # Directory containing Fortran runtime libraries
  if Compiler == "LLVMTOOLS":
    FortranLib = _detect_flang_dir()
  else:
    FortranLib = _detect_gfortran_dir()
  HDF5Lib = os.environ.get("HDF5_API", None) # directory with libhdf5
  pylib = os.path.basename(get_config_var("LIBDIR")) # get lib or lib64
  print("install Cle2000 binding to", Code, "on directory",mach, "pylib=",pylib, "Compiler=",Compiler)
  if Compiler == "NVTOOLS":
    libdir="../../lib/"+mach+"_nvidia"
    libUtl="../../../Utilib/lib/"+mach+"_nvidia"
    libTri="../../../Trivac/lib/"+mach+"_nvidia"
    libDra="../../../Dragon/lib/"+mach+"_nvidia"
    libDon="../../../Donjon/lib/"+mach+"_nvidia"
    extralink=["-lnvcpumath","-lnvf","-lnvc"]
  elif Compiler == "LLVMTOOLS":
    libdir="../../lib/"+mach+"_llvm"
    libUtl="../../../Utilib/lib/"+mach+"_llvm"
    libTri="../../../Trivac/lib/"+mach+"_llvm"
    libDra="../../../Dragon/lib/"+mach+"_llvm"
    libDon="../../../Donjon/lib/"+mach+"_llvm"
    extralink=_compute_flang_link_args(FortranLib)
  elif Compiler == "INTELTOOLS":
    libdir="../../lib/"+mach+"_intel"
    libUtl="../../../Utilib/lib/"+mach+"_intel"
    libTri="../../../Trivac/lib/"+mach+"_intel"
    libDra="../../../Dragon/lib/"+mach+"_intel"
    libDon="../../../Donjon/lib/"+mach+"_intel"
    extralink=[ ]
  else:
    libdir="../../lib/"+mach
    libUtl="../../../Utilib/lib/"+mach
    libTri="../../../Trivac/lib/"+mach
    libDra="../../../Dragon/lib/"+mach
    libDon="../../../Donjon/lib/"+mach
    extralink=["-lgfortran", ]
  print("debug Compiler=",Compiler,"libdir=",libdir,"Code=",Code)
  if FortranLib:
    print("debug FortranLib=", FortranLib)
  if HDF5Lib:
    print("debug HDF5Lib=", HDF5Lib)

  # Build helper lists with None filtered out
  def _dirs(*args):
    return [d for d in args if d]

  if Code == "GANLIB":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with GANLIB",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     extra_link_args = extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Ganlib","hdf5"] ) ])
  elif Code == "TRIVAC":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with TRIVAC",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__trivac__', None)],
                     extra_link_args = extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Trivac","Utilib","Ganlib","hdf5"] ) ])
  elif Code == "DRAGON":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with DRAGON",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__dragon__', None)],
                     extra_link_args = extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri,libDra),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Dragon","Trivac","Utilib","Ganlib","hdf5"] ) ])
  elif Code == "DONJON":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with DONJON",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__donjon__', None)],
                     extra_link_args = extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri,libDra,libDon),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Donjon","Dragon","Trivac","Utilib","Ganlib","hdf5"] ) ])
  elif Code == "GANLIB_OMP":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with GANLIB_OMP",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     extra_link_args = _compute_openmp_link_args(Compiler)+extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Ganlib","hdf5"] ) ])
  elif Code == "TRIVAC_OMP":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with TRIVAC_OMP",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__trivac__', None)],
                     extra_link_args = _compute_openmp_link_args(Compiler)+extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Trivac","Utilib","Ganlib","hdf5"] ) ])
  elif Code == "DRAGON_OMP":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with DRAGON_OMP",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__dragon__', None)],
                     extra_link_args = _compute_openmp_link_args(Compiler)+extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri,libDra),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Dragon","Trivac","Utilib","Ganlib","hdf5"] ) ])
  elif Code == "DONJON_OMP":
    setup (name="Cle2000",
       version="5.0",
       description="Python bindings for Cle-2000 with DONJON_OMP",
       author="Alain Hebert",
       author_email="alain.hebert@polymtl.ca",
       license="LGPL",
       ext_modules=[Extension('cle2000',sources=['cle2000module.c'],
                     define_macros=[('__donjon__', None)],
                     extra_link_args = _compute_openmp_link_args(Compiler)+extralink,
                     include_dirs=["../../../Ganlib/src"],
                     library_dirs=_dirs(libdir,FortranLib,HDF5Lib,libUtl,libTri,libDra,libDon),
                     runtime_library_dirs=_dirs(FortranLib,HDF5Lib),
                     libraries=["Donjon","Dragon","Trivac","Utilib","Ganlib","hdf5"] ) ])
  else:
    raise ValueError(Code+" is not implemented for setup.py bindings")
if __name__ == "__main__":
  main()
