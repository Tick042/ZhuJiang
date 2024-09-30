error id: file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:[852..852) in Input.VirtualFile("file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala", "package xijiang.bridge

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.axi4._
import zhujiang.HasZJParams
import zhujiang.chi._
import zhujiang._


// object AXI4Params{
//     def apply() = {
//         AXI4BundleParameters(addrBits = 48, dataBits = 256, idBits = 8)
//     }
// }


case class BridgeParam(
                       axiDataBits: Int = 256,
                       chiBeatBytes:  Int = 256,
                       axiIdBits:   Int = 8,
                       blockBits:  Int = 512,
                       axiBrustPolicy: String = "INCR",
)

trait HasParseZJParam extends HasZJParams 

trait 


// class BridgeBundle(implicit val p: Parameters) extends ZJBundle 
// class BridgeModule(implicit val p: Parameters) extends ZJModule ")
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:34: error: expected identifier; obtained eof
// class BridgeModule(implicit val p: Parameters) extends ZJModule 
                                                                   ^
#### Short summary: 

expected identifier; obtained eof