error id: file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:[409..414) in Input.VirtualFile("file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala", "package xijiang.bridge

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


case class 



class BridgeBundle(implicit val p: Parameters) extends ZJBundle 
class BridgeModule(implicit val p: Parameters) extends ZJModule ")
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala
file://<WORKSPACE>/src/main/scala/xijiang/bridge/BridgeParameters.scala:24: error: expected identifier; obtained class
class BridgeBundle(implicit val p: Parameters) extends ZJBundle 
^
#### Short summary: 

expected identifier; obtained class