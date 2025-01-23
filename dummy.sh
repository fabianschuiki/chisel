#!/bin/bash
set -xe

# ./mill chisel[].runMain circt.stage.ChiselMain --module "choozle.Top()" --target chirrtl
./mill chisel[].runMain choozle.Dummy
firtool Top.fir --ir-hw -o Top.mlir --enable-layers=Verification,Verification.Assert,Verification.Assume,Verification.Cover --mlir-print-debuginfo
circt-test Top.mlir -l
circt-test Top.mlir
