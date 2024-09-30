package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.amba.axi4._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge._
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._
import xijiang.bridge.Utils.GenerateVerilog
import _root_.circt.stage.FirtoolOption
import chisel3.stage.ChiselGeneratorAnnotation
import _root_.circt.stage._

class AXI2CHI(implicit p: Parameters) extends Module{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_aw = Flipped(Decoupled(new AXI4BundleAW(AXI4Params())))
    val axi_w  = Flipped(Decoupled(new AXI4BundleW(AXI4Params())))
    val axi_b  = Decoupled(new AXI4BundleB(AXI4Params()))
    val axi_r  = Decoupled(new AXI4BundleR(AXI4Params()))
    val axi_ar = Flipped(Decoupled(new AXI4BundleAR(AXI4Params())))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_rxdat = Flipped(Decoupled(new DataFlit))
    val chi_txdat = Decoupled(new DataFlit)
  })

  io.axi_ar <> DontCare
  io.axi_aw <> DontCare
  io.axi_b  <> DontCare
  io.axi_r  <> DontCare
  io.axi_w  <> DontCare

  io.chi_rxdat <> DontCare
  io.chi_rxrsp <> DontCare
  io.chi_txdat <> DontCare
  io.chi_txreq <> DontCare
  
}

//-----------------------------------------------------------------------------//
//-------------------------------GenerateVerilog-------------------------------//
//-----------------------------------------------------------------------------//



// object AXI2CHI extends App {
//     val config = new Config((_, _, _) => {
//         case ZJParametersKey      => ZJParameters()
//         case BridgeParamKey  => BridgeParam()
//     })

//     GenerateVerilog(args, () => new AXI2CHI()(config), name = "AXI2CHI", split = false)
// }

object AXI2CHI extends App {
  private val config = new Config((_,_,_) => {
    case ZJParametersKey => ZJParameters()
    case BridgeParamKey  => BridgeParam()
  })
  private val gen = () => new AXI2CHI()(config)
  (new ChiselStage).execute(
    Array("--target", "verilog") ++ args,
    Seq(
      FirtoolOption("-O=debug"),
    ) ++ Seq(ChiselGeneratorAnnotation(gen))
  )
}