package choozle

import chisel3._
import chisel3.test._
import chisel3.util.Cat
import chisel3.ltl._
import chisel3.experimental.hierarchy._
import circt.stage.ChiselStage

object Dummy {
  def main(args: Array[String]): Unit = {
    os.write.over(os.pwd / "Top.fir", ChiselStage.emitCHIRRTL(new Top))
  }
}

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

class Top extends RawModule {
  // Definition(new HalfAdder)
  // Definition(new FullAdder)

  // val adder1 = Instantiate(new Adder(8))
  // adder1.a := 0.U
  // adder1.b := 0.U
  // val adder2 = Instantiate(new Adder(13))
  // adder2.a := 0.U
  // adder2.b := 0.U
  // val adder3 = Instantiate(new Adder(13))
  // adder3.a := 0.U
  // adder3.b := 0.U
  // val adder4 = Instantiate(new Adder(24))
  // adder4.a := 0.U
  // adder4.b := 0.U

  Definition(new ContractDebug)
}

@instantiable
class HalfAdder extends RawModule with Testable {
  @public val a = IO(Input(Bool()))
  @public val b = IO(Input(Bool()))
  @public val s = IO(Output(Bool()))
  @public val c = IO(Output(Bool()))

  s := a ^ b
  c := a & b

  formalTest {
    val a = IO(Input(Bool()))
    val b = IO(Input(Bool()))
    val dut = Instance(this.toDefinition)
    dut.a := a
    dut.b := b
    AssertProperty(Cat(dut.c, dut.s) === a +& b)
  }
}

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

@instantiable
class Adder(width: Int) extends RawModule with Testable {
  @public val a = IO(Input(UInt(width.W)))
  @public val b = IO(Input(UInt(width.W)))
  @public val z = IO(Output(UInt(width.W)))

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

  formalTest(f"Check_${width}") {
    val a = IO(Input(UInt(width.W)))
    val b = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := a
    dut.b := b
    AssertProperty(dut.z === dut.a + dut.b)
  }

  formalTest(f"NeutralA_${width}") {
    val x = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := 0.U
    dut.b := x
    AssertProperty(dut.z === x)
  }

  formalTest(f"NeutralB_${width}") {
    val x = IO(Input(UInt(width.W)))
    val dut = Instance(this.toDefinition)
    dut.a := x
    dut.b := 0.U
    AssertProperty(dut.z === x)
  }
}

class AdderChecks extends UnitTest {
  Definition(new Adder(0))
  Definition(new Adder(1))
  Definition(new Adder(8))
  Definition(new Adder(24))
}

@instantiable
class ContractDebug extends RawModule with Public {
  @public val a = IO(Input(Bool()))
  @public val b = IO(Input(Bool()))
  @public val s = IO(Output(Bool()))
  @public val c = IO(Output(Bool()))

  val impl = Cat(a & b, a ^ b)
  // val Tuple1(sum) = FormalContract(Tuple1(impl)) { case Tuple1(value) =>

  val x = FormalContract(impl) { impl =>
    println("Hello from 1")
    val inner = ~impl
    val inner2 = ~inner
    EnsureProperty(inner2 === a +& b)
  }

  val (y1, y2) = FormalContract(impl, a) { case (y1, y2) =>
    val inner1 = ~y1
    val inner2 = ~y2
    println("Hello from 2")
    EnsureProperty(y1 === a +& b)
  }

  val q0 = ~x
  val q1 = ~y1
  val q2 = ~y2

  s := x(0)
  c := x(1)
}
