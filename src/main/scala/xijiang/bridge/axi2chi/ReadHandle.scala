package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._

class ReadHandle(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_ar = Flipped(Decoupled(new AXI4BundleAR(AXI4Params())))
    val axi_r  = Decoupled(new AXI4BundleR(AXI4Params()))

    // CHI Interface
    val chi_txreq = CHIChannelIO(new ReqFlit)
    val chi_rxrsp = Flipped(CHIChannelIO(new RespFlit))
    val chi_rxdat = Flipped(CHIChannelIO(new DataFlit))
  })
  
}