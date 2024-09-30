package xijiang.bridge.parameter

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import zhujiang.HasZJParams
import zhujiang.chi._
import zhujiang._
import _root_.xijiang.bridge.parameter

case object BridgeParamKey extends Field[BridgeParam]



case class BridgeParam(
                       axiDataBits: Int = 256,
                       chiBeatBits:  Int = 256,
                       axiIdBits:   Int = 4,
                       blockBits:  Int = 512,
                       addrBits:   Int = 48,
                       axiBrustPolicy: String = "INCR",
                       lcreditNum: Int = 16,
)




abstract class BridgeBundle(implicit val p: Parameters) extends Bundle with BridgeTrait
abstract class BridgeModule(implicit val p: Parameters) extends Module with BridgeTrait

trait BridgeTrait {
    implicit val p : Parameters
    val Param = p(BridgeParamKey)
    val nrBeat = Param.blockBits / Param.chiBeatBits
    val fakeMemBits = 64
    val chiBeatBits = Param.chiBeatBits
    val axiBeatBits = Param.axiDataBits
    val nrLcredit   = Param.lcreditNum
    val offsetBits  = 6
    val beatNumBits = log2Ceil(nrBeat) 


    def toDataID(x: UInt): UInt = {
        require(nrBeat == 1 | nrBeat == 2 | nrBeat == 4)
        if (nrBeat == 1) { "b00".U }
        else if (nrBeat == 2) { Mux(x === 0.U, "b00".U, "b10".U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }

    def toBeatNum(x: UInt): UInt = {
        if (nrBeat == 1) { assert(x === "b00".U); 0.U }
        else if (nrBeat == 2) { assert(x === "b00".U | x === "b10".U); Mux(x === "b00".U, 0.U, 1.U) }
        else if (nrBeat == 4) { x }
        else { 0.U }
    }
}

object AXI4Params {
    def apply()(implicit p : Parameters) = {
        val Param = p(BridgeParamKey)
        AXI4BundleParameters(addrBits = Param.addrBits, dataBits = Param.axiDataBits, idBits = Param.axiIdBits)
    }
}