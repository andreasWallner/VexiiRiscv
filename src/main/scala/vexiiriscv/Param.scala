package vexiiriscv

import spinal.core._
import spinal.lib.misc.plugin.Hostable
import vexiiriscv._
import vexiiriscv.execute._
import vexiiriscv.misc._
import vexiiriscv.prediction.{LearnCmd, LearnPlugin}
import vexiiriscv.riscv.IntRegFile
import vexiiriscv.test.WhiteboxerPlugin

import scala.collection.mutable.ArrayBuffer

class ParamSimple(){
  var xlen = 32
  var rvc = false
  var hartCount = 1
  var withMmu = false
  var resetVector = 0x80000000l
  var decoders = 2
  var lanes = 2
  var regFileSync = false
  var ioRange    : UInt => Bool = a => a(31 downto 28) === 0x1
  var fetchRange : UInt => Bool = a => a(31 downto 28) =/= 0x1
  var withGShare = true
  var withBtb = true
  var withRas = true

  def plugins() = {
    val plugins = ArrayBuffer[Hostable]()

    plugins += new riscv.RiscvPlugin(xlen, rvc, hartCount)
    withMmu match {
      case false => plugins += new memory.StaticTranslationPlugin(32, ioRange, fetchRange)
      case true =>
    }

    plugins += new misc.PipelineBuilderPlugin()
    plugins += new schedule.ReschedulePlugin()

    plugins += new LearnPlugin()
    if(withRas) assert(withBtb)
    if(withGShare) assert(withBtb)
    if(withBtb) {
      plugins += new prediction.BtbPlugin(
        sets = 512 / decoders,
        ways = decoders,
        rasDepth = if(withRas) 4 else 0,
        hashWidth = 16,
        readAt = 0,
        hitAt = 1,
        jumpAt = 1
      )
      plugins += new prediction.DecodePredictionPlugin(
        decodeAt = 1,
        jumpAt = 1
      )
    }
    if(withGShare) {
      plugins += new prediction.GSharePlugin (
        memBytes = 4 KiB,
        historyWidth = 12,
        readAt = 0
      )
      plugins += new prediction.HistoryPlugin()
    }


    plugins += new fetch.PcPlugin(resetVector)
    plugins += new fetch.FetchPipelinePlugin()
    plugins += new fetch.CachelessPlugin(
      forkAt = 0,
      joinAt = 1, //You can for instance allow the external memory to have more latency by changing this
      wordWidth = 32*decoders
    )

    plugins += new decode.DecodePipelinePlugin()
    plugins += new decode.AlignerPlugin(
      fetchAt = 1,
      lanes = decoders
    )
    plugins += new decode.DecoderPlugin(
      decodeAt = 1
    )
    plugins += new schedule.DispatchPlugin(
      dispatchAt = 1
    )

    plugins += new regfile.RegFilePlugin(
      spec = riscv.IntRegFile,
      physicalDepth = 32,
      preferedWritePortForInit = "lane0",
      syncRead = regFileSync
    )

    val allowBypassFrom = 0 //100 => disabled
    def newExecuteLanePlugin(name : String) = new execute.ExecuteLanePlugin(
      name,
      rfReadAt = 0,
      decodeAt = regFileSync.toInt,
      executeAt = regFileSync.toInt + 1,
      withBypasses = allowBypassFrom<100
    )

    plugins += new execute.ExecutePipelinePlugin()

    val lane0 = newExecuteLanePlugin("lane0")
    val early0 = new LaneLayer("early0", lane0, priority = 0)
    val late0 = new LaneLayer("late0", lane0, priority = -5)
    plugins += lane0


    plugins += new RedoPlugin("lane0")
    plugins += new SrcPlugin(early0, executeAt = 0)
    plugins += new IntAluPlugin(early0, formatAt = 0)
    plugins += new BarrelShifterPlugin(early0, formatAt = 0)
    plugins += new IntFormatPlugin("lane0")
    plugins += new BranchPlugin(layer=early0, aluAt=0, jumpAt=0, wbAt=0)
    plugins += new LsuCachelessPlugin(
      layer     = early0,
      withSpeculativeLoadFlush = true,
      addressAt = 0,
      forkAt    = 0,
      joinAt    = 1,
      wbAt      = 2,
      translationStorageParameter = null,
      translationPortParameter = null
    )
    plugins += new RsUnsignedPlugin("lane0")
    plugins += new MulPlugin(early0)
    plugins += new DivPlugin(early0)
    plugins += new CsrAccessPlugin(early0, writeBackKey =  if(lanes == 1) "lane0" else "lane1")
    plugins += new PrivilegedPlugin(PrivilegedConfig.full)

    plugins += new SrcPlugin(late0, executeAt=2)
    plugins += new IntAluPlugin(late0, aluAt=2, formatAt=2)
    plugins += new BarrelShifterPlugin(late0, shiftAt=2, formatAt=2)
    plugins += new BranchPlugin(late0, aluAt = 2, jumpAt = 2, wbAt = 2)

    plugins += new WriteBackPlugin("lane0", IntRegFile, writeAt = 2, allowBypassFrom = allowBypassFrom)


    if(lanes >= 2) {
      val lane1 = newExecuteLanePlugin("lane1")
      val early1 = new LaneLayer("early1", lane1, priority = 10)
      val late1 = new LaneLayer("late1", lane1, priority = -3)
      plugins += lane1

      plugins += new SrcPlugin(early1, executeAt = 0)
      plugins += new IntAluPlugin(early1, formatAt = 0)
      plugins += new BarrelShifterPlugin(early1, formatAt = 0)
      plugins += new IntFormatPlugin("lane1")
      plugins += new BranchPlugin(early1, aluAt = 0, jumpAt = 0, wbAt = 0)

      plugins += new SrcPlugin(late1, executeAt = 2)
      plugins += new IntAluPlugin(late1, aluAt = 2, formatAt = 2)
      plugins += new BarrelShifterPlugin(late1, shiftAt = 2, formatAt = 2)
      plugins += new BranchPlugin(late1, aluAt = 2, jumpAt = 2, wbAt = 2)

      plugins += new WriteBackPlugin("lane1", IntRegFile, writeAt = 2, allowBypassFrom = allowBypassFrom)
    }


    plugins += new WhiteboxerPlugin()

    plugins
  }
}

/*
jump at 0 :
1l btb gshare ras => 1.64 dhrystone 3.26 coremark 1.04 embench
       + late alu => 1.72 dhrystone 3.54 coremark 1.10
2l btb gshare ras => 1.92 dhrystone 3.93 coremark 1.34 embench
       + late alu => 2.09 dhrystone 4.39 coremark
    + 4b late alu => 2.24 dhrystone 4.55 coremark 1.47 embench



jump at 1
1l btb gshare ras => 1.64 dhrystone 3.21 coremark 1.03 embench
2l btb gshare ras => 1.91 dhrystone 3.83 coremark 1.32 embench

 coremark 2l => Branch : 523630 47815   9.1%

btb ras gshare at 1 :
vexii_1i ->
Artix 7 -> 90 Mhz 1619 LUT 1038 FF
Artix 7 -> 166 Mhz 2423 LUT 1091 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2658 LUT 1314 FF
Artix 7 -> 134 Mhz 3136 LUT 1370 FF

btb ras gshare at 2 :
vexii_1i ->
Artix 7 -> 90 Mhz 1554 LUT 1045 FF
Artix 7 -> 196 Mhz 1705 LUT 1045 FF
vexii_2i -> embench 1.21
Artix 7 -> 90 Mhz 2808 LUT 1349 FF
Artix 7 -> 137 Mhz 2949 LUT 1352 FF

  -mtune=sifive-7-series
1.51
3.11

1.51
3.20

dual issue, btb, no gshare =>

1.74 Dhrystone/MHz
3.66 Coremark/MHz
aha-mont64           1.41
crc32                1.00
cubic                0.60
edn                  1.08
huffbench            1.46
matmult-int          1.01
md5sum               1.82
minver               0.81
nbody                1.01
nettle-aes           1.53
nettle-sha256        1.61
nsichneu             0.79
picojpeg             1.14
primecount           1.46
qrduino              1.41
sglib-combined       1.21
slre                 1.54
st                   1.21
statemate            1.96
tarfind              2.05
ud                   1.15
wikisort             1.78
---------           -----
Geometric mean       1.26


vexii_1i ->
Artix 7 -> 90 Mhz 1466 LUT 907 FF
Artix 7 -> 189 Mhz 1946 LUT 960 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2596 LUT 1153 FF
Artix 7 -> 135 Mhz 3136 LUT 1207 FF

vexii_1i ->
Artix 7 -> 90 Mhz 1054 LUT 737 FF
Artix 7 -> 193 Mhz 1498 LUT 789 FF
vexii_2i ->
Artix 7 -> 90 Mhz 2271 LUT 980 FF
Artix 7 -> 133 Mhz 2777 LUT 1033 FF




lane micro op spec
- For each impl
  - readRsFrom
  - rd bypass from (executeId)
  - completion at (probe)
  - mayFlushUpTo
  - dontFlushFrom
 */

