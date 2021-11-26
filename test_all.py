#!/usr/bin/env python3

import collections

Compiler = collections.namedtuple("Compiler", "f90 cpp lflags cflags cflags_devel f90_cflags cpp_cflags")

compilers = {
   "gnu": Compiler(
      f90='gfortran', cpp='g++', cflags='-g -fbacktrace -Wall -Wextra -cpp',
      lflags = "",
      # Some of these are not specific for Fortran:
      f90_cflags="""
      -std=f2008 -pedantic -Werror
      -Wunused-parameter
      -Wno-unused-function
      -Wno-maybe-uninitialized -Wno-unused-dummy-argument -Wno-error=return-type
      -Werror=shadow -Werror=intrinsic-shadow -Wuninitialized -Wunreachable-code -Wconversion
      -Waliasing -Wampersand -Wc-binding-type -Wcharacter-truncation
      -Wfunction-elimination -Wimplicit-interface -Wimplicit-procedure -Wintrinsic-shadow -Wintrinsics-std -Wline-truncation -Wno-tabs
      -Wreal-q-constant -Wsurprising
      -Wno-conversion
      -Wno-implicit-interface -Wno-strict-overflow
      """.replace('\n',''),
      cflags_devel="-O0 -fcheck=all -fbounds-check -Warray-bounds -Wstrict-overflow=5 -Wunderflow -ffpe-trap=invalid,zero,overflow",
      cpp_cflags="",
   ),
   "intel": Compiler(
      f90='ifort', cpp='icpc', cflags='-g -traceback -cpp',
      lflags = "",
      f90_cflags="",
      cflags_devel="-O0",
      cpp_cflags="",
   )
}
compilers["llvm"] = Compiler(**{
   **compilers["intel"]._asdict(),
   "f90": 'ifx', # flang is buggier than ifx
   "cpp": 'clang++'
})

for vendor, c in compilers.items():
   prog = f"test_{vendor}"
   print(f"{c.f90} {c.cflags} {c.f90_cflags} fhash_modules.f90 fhash_test.f90 -o {prog} && ", end='')
   # Insist on valgind, because this is a container implementation with raw pointers inside:
   print(f"valgrind --quiet ./{prog} &&", end='')
print("echo ALL TESTS PASSED")
