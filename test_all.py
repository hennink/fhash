#!/usr/bin/env python3

import argparse
import collections
from distutils import util

Compiler = collections.namedtuple("Compiler", "vendor f90 cpp cflags cflags_devel cflags_optim f90_cflags cpp_cflags")

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
      cflags_devel="-O0",
      cpp_cflags="-std=c++11",
      cflags_optim="-Ofast",
   ),
   Compiler(
      vendor = "llvm",
      f90='ifx', cpp='clang++',  # flang is buggier than ifx
      cflags='-g -traceback',
      f90_cflags="-cpp",
      cflags_devel="-O0",
      cpp_cflags="-std=c++11",
      cflags_optim="-Ofast",
   ),
]

Task = collections.namedtuple("Task", 'f90_files optim')
tasks = {
   "test": Task("fhash_modules.f90 fhash_test.f90", optim=False),
   "benchmark_f90": Task("benchmark.f90", optim=True),
   "benchmark_stl": Task("benchmark.cc", optim=True),
}

def exec_task(taskname, task : Task, c : Compiler):
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

   prog = f"{taskname}_{c.f90}"
   call = f"./{prog}"
   if args.valgrind and not task.optim:
      call = f"valgrind --quiet --leak-check=full {call}"

   return f"""
      {compiler} {c.cflags} {optim_cflags} {task.f90_files} -o {prog}
      {call}
   """

parser = argparse.ArgumentParser(description="output Bash code that runs tests and/or benchmarks for the fhash library")
all_vendors = [c.vendor for c in compilers]
parser.add_argument("--compilers", "-c", nargs='+', type=str.lower, choices=all_vendors, default=all_vendors)
parser.add_argument("--tasks", "-t", nargs='+', type=str.lower, choices=tasks, default=tasks)
parser.add_argument("--verbose", "-v", type=util.strtobool, default=False)
parser.add_argument("--valgrind", "-g", type=util.strtobool, default=True, help="run tasks under valgrind (unless they require optimization)")
args = parser.parse_args()

all_tasks = "\n".join(
   f"({exec_task(t, tasks[t], c)})"
   for t in args.tasks
   for c in compilers if c.vendor in args.compilers
)
test_prog = f"""(
   set -e
   {"set -x" if args.verbose else ""}
   {all_tasks}
   echo 'done'
)"""
print(test_prog)
