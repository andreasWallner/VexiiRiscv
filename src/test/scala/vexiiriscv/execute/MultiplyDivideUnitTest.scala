package vexiiriscv.execute;

import spinal.core._
import spinal.core.sim._

import org.scalatest.funsuite.AnyFunSuite;

class MultiplyDivideUnitTest extends AnyFunSuite {
  val dut = SpinalSimConfig().withFstWave.compile(MultiplyDivideUnit(4))
  test("random") {
    dut.doSim("random", seed=1) { dut =>
      dut.clockDomain.forkStimulus(10)

      for(ab <- 0 until 16; bb <- 0 until 16; (as, bs) <- Seq((false, false), (true, true), (true, false))) {
        dut.io.run #= false
        dut.io.a.signed #= as
        dut.io.b.signed #= bs
        dut.io.a.value.randomize()
        dut.io.b.value.randomize()

        dut.clockDomain.waitSampling()
        dut.io.run #= true
        dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)

        val a = dut.io.a.value.toVariableBigInt(dut.io.a.signed.toBoolean)
        val b = dut.io.b.value.toVariableBigInt(dut.io.b.signed.toBoolean)
        val expected = a * b
        val actual = dut.io.result.toVariableBigInt(dut.io.a.signed.toBoolean || dut.io.b.signed.toBoolean)
        val ass = if(as) "S" else "U"
        val bss = if(bs) "S" else "U"
        println(f"$ass $a%03x ($a, ${dut.io.a.value.toBigInt.toString(2)}) * $bss $b%03x ($b, ${dut.io.b.value.toBigInt.toString(2)}) = $expected%05x ($expected) ?= $actual%05x ($actual)")
        assert(actual == expected)
        dut.io.run #= false
      }
    }

    implicit class BitsSignedPimper(b: Bits) {
      def toVariableBigInt(signed: Boolean): BigInt = if (signed)
        this.toSignedBigInt
      else
        b.toBigInt

      def toSignedBigInt: BigInt = {
        val bin = b.toBigInt
        val negative = bin.testBit(widthOf(b) - 1)
        if (negative)
          bin - (BigInt(1) << widthOf(b))
        else
          bin
      }
    }
  }
}
