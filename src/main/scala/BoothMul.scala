package choozle.booth

import chisel3._
import chisel3.test._
import chisel3.util.Cat
import chisel3.ltl._
import chisel3.experimental.hierarchy._
import circt.stage.ChiselStage
import choozle.Testable

@instantiable
class BoothEncoder extends RawModule with Testable with UnitTest with Public {
  @public val lhs = IO(Input(UInt(16.W)))
  @public val rhs = IO(Input(UInt(16.W)))
  // @public val s = IO(Output(Bool()))
  // @public val c = IO(Output(Bool()))

  val lhsNeg = -lhs
  val rhsExt = Cat(rhs, false.B)
  val numTerms = 16 / 2
  val terms = Wire(Vec(numTerms, UInt(16.W)))
  var termsSummed = 0.U
  for (i <- 0 until numTerms) {
    val bits = rhsExt(i * 2 + 1, i * 2)
    terms(i) := 0.U
    when(bits === 0b01.U) {
      terms(i) := lhs << (i * 2)
    }.elsewhen(bits === 0b10.U) {
      terms(i) := lhsNeg << (i * 2)
    }
    if (i > 0)
      AssertProperty(terms(i)(i * 2 - 1, 0) === 0.U)
    val partialSum = FormalContract(termsSummed + terms(i)) { sum =>
      EnsureProperty(sum === lhs * rhs(i * 2 + 1, 0))
    }
    termsSummed = partialSum
  }

  formalTest {
    val lhs = IO(Input(Bool()))
    val rhs = IO(Input(Bool()))
    val dut = Instance(this.toDefinition)
    dut.lhs := lhs
    dut.rhs := rhs
  }
}
