#!/bin/bash
set -xe

./mill chisel[].runMain chisel3.UnitTests -o AllUnitTests.fir -v "$@"
firtool AllUnitTests.fir --ir-hw -o AllUnitTests.mlir --enable-layers=Verification,Verification.Assert,Verification.Assume,Verification.Cover --mlir-print-debuginfo
circt-test AllUnitTests.mlir -l
circt-test AllUnitTests.mlir
