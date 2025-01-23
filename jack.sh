#!/bin/bash
set -xe

./mill chisel[].runMain chisel3.UnitTests -o tests.fir -f formalIsKoenig "$@"
firtool tests.fir -o tests.mlir --ir-hw --enable-layers=Verification,Verification.Assert,Verification.Assume,Verification.Cover --mlir-print-debuginfo
circt-test tests.mlir -l
circt-test tests.mlir
