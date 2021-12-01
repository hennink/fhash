#!/usr/bin/env python3

import argparse
import collections
from distutils import util

Compiler = collections.namedtuple("Compiler", "vendor f90 cpp cflags f90_cflags cpp_cflags cflags_devel cflags_optim")
compilers = [
   Compiler(
      vendor = "gnu",
      f90='gfortran', cpp='g++',
      cflags='-g -Wall -Wextra',
      # Some of these are not specific for Fortran:
      f90_cflags="""
      -cpp -std=f2008 -pedantic -Werror -fbacktrace
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
      cpp_cflags="-std=c++11",
      cflags_devel="-O0 -fcheck=all -fbounds-check -Warray-bounds -Wstrict-overflow=5 -Wunderflow -ffpe-trap=invalid,zero,overflow",
      cflags_optim="-O3",
   ),
   Compiler(
      vendor = "intel",
      f90='ifort', cpp='icpc',
      cflags='-g -traceback',
      f90_cflags="-cpp",
      cpp_cflags="-std=c++11",
      cflags_devel="-O0",
      cflags_optim="-Ofast",
   ),
   Compiler(
      vendor = "llvm",
      f90='ifx', cpp='clang++',  # flang is buggier than ifx
      cflags='-g -traceback',
      f90_cflags="-cpp",
      cpp_cflags="-std=c++11",
      cflags_devel="-O0",
      cflags_optim="-Ofast",
   ),
]

Task = collections.namedtuple("Task", 'name f90_files optim')
tasks = [
   Task("test", "fhash_modules.f90 fhash_test.f90", optim=False),
   Task("benchmark_f90", "benchmark.f90", optim=True),
   Task("benchmark_stl", "benchmark.cc", optim=True),
]

def exec_task(task : Task, c : Compiler):
   if all(f.endswith('.f90') for f in task.f90_files.split()):
      compiler = f"{c.f90} {c.f90_cflags}"
   elif all(f.endswith('.cc') for f in task.f90_files.split()):
      compiler = f"{c.cpp} {c.cpp_cflags}"
   else:
      raise NotImplementedError("Cannot deterine language of these files: {task.f90_files}")

   if task.optim:
      optim_cflags = c.cflags_optim
   else:
      optim_cflags = c.cflags_devel

   prog = f"{task.name}_{c.f90}"
   call = f"./{prog}"
   if args.valgrind and not task.optim:
      call = f"valgrind --quiet --leak-check=full {call}"

   return f"""
      {compiler} {c.cflags} {optim_cflags} {task.f90_files} -o {prog}
      {call}
   """

parser = argparse.ArgumentParser(description="output Bash code that runs tests and/or benchmarks for the fhash library")
all_vendors = [c.vendor for c in compilers]
all_tasks = [t.name for t in tasks]
parser.add_argument("--compilers", "-c", nargs='+', type=str.lower, choices=all_vendors, default=all_vendors)
parser.add_argument("--tasks", "-t", nargs='+', type=str.lower, choices=all_tasks, default=all_tasks)
parser.add_argument("--verbose", "-v", type=util.strtobool, default=True)
parser.add_argument("--valgrind", "-g", type=util.strtobool, default=True, help="run tasks under valgrind (unless they require optimization)")
args = parser.parse_args()

bash_commands = "\n".join(
   f"({exec_task(t, c)})"
   for t in tasks if t.name in args.tasks
   for c in compilers if c.vendor in args.compilers
)
test_prog = f"""(
   set -e
   {"set -x" if args.verbose else ""}
   {bash_commands}
   echo 'done'
)"""
print(test_prog)
