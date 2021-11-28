#!/usr/bin/env python3

import argparse
import collections

Compiler = collections.namedtuple("Compiler", "f90 cpp cflags cflags_devel cflags_optim f90_cflags cpp_cflags")

compilers = {
   "gnu": Compiler(
      f90='gfortran', cpp='g++',
      cflags='-g -fbacktrace -Wall -Wextra -cpp',
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
      cpp_cflags="",
      cflags_devel="-O0 -fcheck=all -fbounds-check -Warray-bounds -Wstrict-overflow=5 -Wunderflow -ffpe-trap=invalid,zero,overflow",
      cflags_optim="-O3",
   ),
   "intel": Compiler(
      f90='ifort', cpp='icpc', cflags='-g -traceback -cpp',
      f90_cflags="",
      cflags_devel="-O0",
      cpp_cflags="",
      cflags_optim="-Ofast",
   )
}
compilers["llvm"] = Compiler(**{
   **compilers["intel"]._asdict(),
   "f90": 'ifx', # flang is buggier than ifx
   "cpp": 'clang++'
})

Task = collections.namedtuple("Task", 'f90_files optim')
tasks = {
   "test": Task("fhash_modules.f90 fhash_test.f90", optim=False),
   "benchmark_f90": Task("benchmark.f90", optim=True),
}

def exec_task(taskname, task : Task, c : Compiler):
   prog = f"{taskname}_{c.f90}"
   # Insist on valgind, because this is a container implementation with raw pointers inside:
   cflags = f"{c.cflags} {c.f90_cflags}"
   if task.optim:
      cflags += " " + c.cflags_optim
      prefix = ""
   else:
      cflags += " " + c.cflags_devel
      prefix = "valgrind --quiet"

   return f"""
      {c.f90} {cflags} {task.f90_files} -o {prog}
      {prefix} ./{prog}
   """

parser = argparse.ArgumentParser(description="output Bash code that runs tests and/or benchmarks for the fhash library")
parser.add_argument("--compilers", "-c", nargs='+', type=str, choices=compilers, default=compilers)
parser.add_argument("--tasks", "-t", nargs='+', type=str, choices=tasks, default=tasks)
args = parser.parse_args()

all_tasks = "\n".join(
   f"({exec_task(t, tasks[t], compilers[c])})"
   for t in args.tasks
   for c in args.compilers
)
test_prog = f"""(
   set -e
   {all_tasks}
   echo 'done'
)"""
print(test_prog)
