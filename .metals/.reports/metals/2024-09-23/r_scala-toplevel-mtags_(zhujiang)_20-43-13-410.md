error id: file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:[326..330) in Input.VirtualFile("file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala", "package xijiang.bridge

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import zhujiang.HasZJParams
import zhujiang.chi._
import zhujiang._

case object BridgeParamKey extends Field[BridgeParam]
case object 


case class BridgeParam(
                       axiDataBits: Int = 256,
                       chiBeatBytes:  Int = 256,
                       axiIdBits:   Int = 8,
                       blockBits:  Int = 512,
                       axiBrustPolicy: String = "INCR",
)


trait HasBridgeParam extends HasZJParams {
    val p : Parameters
    val bridgeParam = p(BridgeParamKey)
    val axiAddrBits = raw
    val chiNrBeat   = bridgeParam.blockBits / bridgeParam.chiBeatBytes
}


abstract class BridgeBundle(implicit val p: Parameters) extends Bundle with HasBridgeParam
abstract class BridgeModule(implicit val p: Parameters) extends Module with HasBridgeParam

object AXI4Params {
    def apply()(implicit p: Parameters) = {
        val bridgeParam = p(BridgeParamKey)
        AXI4BundleParameters(addrBits = 48, dataBits = bridgeParam.axiDataBits, idBits = bridgeParam.axiIdBits)
    }
}")
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:16: error: expected identifier; obtained case
case class BridgeParam(
^
#### Short summary: 

expected identifier; obtained case