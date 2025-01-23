package formalIsKoenig

import chisel3._
import chisel3.test._
import chisel3.ltl._
import chisel3.util.Cat
import chisel3.experimental.hierarchy._

// Run this with something like:
// ./mill chisel[].runMain chisel3.UnitTests -o tests.fir -f formalIsKoenig
// firtool tests.fir -o tests.mlir --ir-hw --enable-layers=Verification,Verification.Assert,Verification.Assume,Verification.Cover --mlir-print-debuginfo
// circt-test tests.mlir -l
// circt-test tests.mlir

//===----------------------------------------------------------------------===//
// Minimal Example
//===----------------------------------------------------------------------===//

// Extending `UnitTest` causes this to be picked up by `chisel3.UnitTests`.
class MinimalTest extends RawModule with UnitTest {
  // Mark this module as a formal test. This emits a `formal` declaration in
  // FIRRTL, which `firtool` will lower to a `verif.formal` test which
  // `circt-test` will automatically pick up.
  FormalTest(this)

  // Inputs on this module are treated as symbolic values that can change in
  // every clock cycle.
  val a = IO(Input(UInt(42.W)))
  AssertProperty(a << 1 === a +& a)
  AssumeProperty(a >= 1.U)
  AssertProperty(a +& a >= 2.U)
}

//===----------------------------------------------------------------------===//
// Simple Half Adder
//===----------------------------------------------------------------------===//

class SimpleHalfAdder extends RawModule {
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  val s = IO(Output(Bool()))
  val c = IO(Output(Bool()))
  s := a ^ b
  c := a & b
}

// Unit tests that just instantiate some DUT are pretty straightforward. If you
// mark this with `FormalTest(this)`, `circt-test` will pick it up for you.
class SimpleHalfAdderTest extends RawModule with UnitTest {
  FormalTest(this)
  val a = IO(Input(Bool()))
  val b = IO(Input(Bool()))
  val dut = Module(new SimpleHalfAdder)
  dut.a := a
  dut.b := b
  AssertProperty(Cat(dut.c, dut.s) === a +& b)
}

//===----------------------------------------------------------------------===//
// Inline Tests
//===----------------------------------------------------------------------===//

// This isn't upstream anywhere, but you can cook up convenience wrappers to
// define formal tests inline in a module pretty easily. This is nice if you
// need access to a module's parameters.
trait Testable extends RawModule {
  protected def formalTest(gen: => Unit): Unit = {
    formalTest("Test")(gen)
  }
  protected def formalTest(suffix: String)(gen: => Unit): Unit = {
    afterModuleBuilt {
      val that = this
      Definition(new RawModule {
        gen
        FormalTest(this, MapTestParam(Map()), that.desiredName + suffix)
      })
    }
  }
}

// Same half adder as above, but with an inline formal test.
@instantiable
class HalfAdder extends RawModule with Testable {
  @public val a = IO(Input(Bool()))
  @public val b = IO(Input(Bool()))
  @public val s = IO(Output(Bool()))
  @public val c = IO(Output(Bool()))

  s := a ^ b
  c := a & b

  // This will produce a formal unit test for the half adder whenever it is
  // instantiated anywhere in the design.
  formalTest {
    val a = IO(Input(Bool()))
    val b = IO(Input(Bool()))
    val dut = Instance(this.toDefinition)
    dut.a := a
    dut.b := b
    AssertProperty(Cat(dut.c, dut.s) === a +& b)
  }
}

// Full adder built from two of the above half adders, with an inline formal
// test. Since this instantiates the half adder internally, instantiating the
// full adder will produce a formal unit test for both the half adder and the
// full adder in the output. Since this runs through D/I, you get a single copy
// of the tests.
@instantiable
class FullAdder extends RawModule with Testable {
  @public val a = IO(Input(Bool()))
  @public val b = IO(Input(Bool()))
  @public val ci = IO(Input(Bool()))
  @public val s = IO(Output(Bool()))
  @public val co = IO(Output(Bool()))

  val ha1 = Instantiate(new HalfAdder)
  val ha2 = Instantiate(new HalfAdder)
  ha1.a := a
  ha1.b := b
  ha2.a := ha1.s
  ha2.b := ci
  s := ha2.s
  co := ha1.c | ha2.c

  // This will produce a formal unit test for the full adder whenever it is
  // instantiated anywhere in the design.
  formalTest {
    val a = IO(Input(Bool()))
    val b = IO(Input(Bool()))
    val ci = IO(Input(Bool()))
    val dut = Instance(this.toDefinition)
    dut.a := a
    dut.b := b
    dut.ci := ci
    AssertProperty(Cat(dut.co, dut.s) === a +& b +& ci)
  }
}

// Since the half and full adders have their tests inline, you can create a new
// definition for them in a separate `UnitTest` to run them as a unit test. You
// could also mark the modules as `UnitTest` directly, since they don't have any
// parameters.
class TestHalfAndFullAdder extends UnitTest {
  Definition(new FullAdder)
}

//===----------------------------------------------------------------------===//
// Parametrized Modules
//===----------------------------------------------------------------------===//

// A simple adder with configurable bit width, which instantiates a chain of the
// above full adders to implement its functionality. If you use this adder in a
// design, you'd automatically get the unit tests for all parametrizations that
// were used.
@instantiable
class Adder(width: Int) extends RawModule with Testable {
  @public val a = IO(Input(UInt(width.W)))
  @public val b = IO(Input(UInt(width.W)))
  @public val z = IO(Output(UInt(width.W)))

  // Create a ripple carry chain of full adders to compute the result.
  var carry = 0.U
  if (width > 0) {
    z := VecInit(a.asBools.zip(b.asBools).map { case (digitA, digitB) =>
      val fa = Instantiate(new FullAdder)
      fa.a := digitA
      fa.b := digitB
      fa.ci := carry
      carry = fa.co
      fa.s
    }).asUInt
  } else {
    z := 0.U
  }

  // Check this specific adder computes `z = a + b`.
  formalTest(f"Check_W${width}") {
    val a = IO(Input(UInt(width.W)))
    val b = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := a
    dut.b := b
    AssertProperty(dut.z === dut.a + dut.b)
  }

  // Check that `a == 0` implies `z = b`.
  formalTest(f"NeutralA_W${width}") {
    val x = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := 0.U
    dut.b := x
    AssertProperty(dut.z === x)
  }

  // Check that `b == 0` implies `z = a`.
  formalTest(f"NeutralB_W${width}") {
    val x = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := x
    dut.b := 0.U
    AssertProperty(dut.z === x)
  }
}

// Create a few definitions of interesting adder parametrizations for runs with
// `chisel3.UnitTests`. This just makes sure that event if you have no design
// you still get the interesting corner cases of the adder tested as a unit
// test.
class InterestingAdders extends UnitTest {
  Definition(new Adder(0))
  Definition(new Adder(1))
  Definition(new Adder(8))
}
