package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._

class WriteHandle(implicit p: Parameters) extends BridgeModule{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_aw = Flipped(Decoupled(new AXI4BundleAW(AXI4Params())))
    val axi_w  = Flipped(Decoupled(new AXI4BundleW(AXI4Params())))
    val axi_b  = Decoupled(new AXI4BundleB(AXI4Params()))

    // CHI Interface
    val chi_txreq = CHIChannelIO(new ReqFlit)
    val chi_rxrsp = Flipped(CHIChannelIO(new RespFlit))
    val chi_txdat = CHIChannelIO(new DataFlit)
  })
  
}