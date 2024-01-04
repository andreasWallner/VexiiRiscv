package vexiiriscv.execute

import scala.collection.mutable
import spinal.core._
import spinal.lib._
import spinal.lib.misc.pipeline._
import vexiiriscv.riscv.{IntRegFile, RS1, RS2, Riscv, Rvi}
import Riscv._
import RsUnsignedPlugin._
import vexiiriscv.misc.{AdderAggregator, MulSpliter}

import scala.collection.mutable.ArrayBuffer

object IterativeMulDivPlugin extends AreaObject {
  val HIGH = Payload(Bool())
  val RESULT_IS_SIGNED = Payload(Bool())
}

class IterativeMulDivPlugin(val layer: LaneLayer,
                            val srcAt: Int = 0,
                            val mulAt: Int = 0,
                            val sumAt: Int = 1,
                            val writebackAt: Int = 2) extends ExecutionUnitElementSimple(layer) {
  import MulPlugin._

  lazy val ifp = host.find[IntFormatPlugin](_.laneName == layer.laneName)

  val logic = during setup new Logic {
    val formatBus = ifp.access(writebackAt)
    implicit val _ = ifp -> formatBus
    add(Rvi.MUL).decode(HIGH -> False).rsUnsigned(true, true)
    add(Rvi.MULH).decode(HIGH -> True).rsUnsigned(true, true)
    add(Rvi.MULHSU).decode(HIGH -> True).rsUnsigned(true, false)
    add(Rvi.MULHU).decode(HIGH -> True).rsUnsigned(false, false)
    if (XLEN.get == 64) {
      add(Rvi.MULW).decode(HIGH -> False).rsUnsigned(true, true)
      for (op <- List(Rvi.MULW)) {
        ifp.signExtend(formatBus, layer(op), 32)
      }
    }

    uopRetainer.release()

    val product = Reg(UInt(XLEN bits))
    val multiplicand = Reg(UInt(XLEN bits))
    val multiplier = Reg(UInt(XLEN bits))

    val mul = new eu.Execute(mulAt) {

    }
    val writeback = new eu.Execute(writebackAt) {

    }
  }
}

// TODO load narrow
case class MultiplyDivideUnit(width: Int) extends Component {
  val io = new Bundle {
    val run = in port Bool()
    val a = in port new Bundle {
      val value = Bits(width bit)
      val signed = Bool()
    }
    val b = in port new Bundle {
      val value = Bits(width bit)
      val signed = Bool()
    }

    val result = out port Bits(2*width bit)
    val done = out port Bool()
  }
  assert(!(!io.a.signed && io.b.signed), "a unsigned & b signed is not supported")

  val unsigned = !io.a.signed && !io.b.signed
  val mem = Reg(UInt(2 * width + 1 bit))
  val cnt = Reg(UInt(log2Up(width + 1) bit)) init width

  val sum = mem.takeHigh(width).asUInt +^ io.a.value.asUInt
  val overflow = sum.msb

  val extended_sum = io.a.signed.mux(f=sum, t=sum.takeLow(width).asSInt.resize(width+1).asUInt)
  val extended_keep = io.a.signed.mux(f=False ## mem.takeHigh(width), t=mem.msb ## mem.takeHigh(width)).asUInt

  val unshifted = mem(0).mux(t=extended_sum, f=extended_keep) @@ mem.takeLow(width + 1).asUInt

  val multiplicator = mem.takeLow(width+1)

  val busy = Reg(Bool()) init False
  val willBeDone = False
  when(busy) {
    mem := unshifted >> 1
    cnt := cnt - 1
    when(cnt === 0) {
      willBeDone := True
    }
  } elsewhen(io.run) {
    busy := True
    mem.clearAll()
    mem(width downto 0) := ((io.b.value.msb & io.b.signed & !io.a.signed) ## io.b.value).asUInt
    cnt := width
  }

  busy.clearWhen(io.done)
  io.result := mem.takeLow(2*width)
  io.done := RegNext(willBeDone)
}
