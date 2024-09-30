package xijiang.bridge.parameter

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import zhujiang.HasZJParams
import zhujiang.chi._
import zhujiang._

case object BridgeParamKey extends Field[BridgeParam]



case class BridgeParam(
                       axiDataBits: Int = 256,
                       chiBeatBytes:  Int = 256,
                       axiIdBits:   Int = 4,
                       blockBits:  Int = 512,
                       addrBits:   Int = 48,
                       axiBrustPolicy: String = "INCR",
                       lcreditNum: Int = 16,
)




// abstract class BridgeBundle(implicit val p: Parameters) extends Bundle 
// abstract class BridgeModule(implicit val p: Parameters) extends Module 

object AXI4Params {
    def apply()(implicit p : Parameters) = {
        val Param = p(BridgeParamKey)
        AXI4BundleParameters(addrBits = Param.addrBits, dataBits = Param.axiDataBits, idBits = Param.axiIdBits)
    }
}