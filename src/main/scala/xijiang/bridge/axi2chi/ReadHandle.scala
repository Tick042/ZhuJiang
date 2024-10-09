package xijiang.bridge.axi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import xijiang.bridge.parameter._
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._


class AREntry(implicit p: Parameters) extends BridgeBundle {
    val state           = Bool()
    val addr            = UInt(addrBits.W)
    val burst           = Bool() //TODO
    val len             = UInt(8.W)
    val size            = UInt(3.W)
    val arid            = UInt(axiIdBits.W)
    val nid             = UInt(nidBits.W)
    val sendNum         = UInt(sendNumBits.W)
    
}

  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- Module Define --------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//


class ReadHandle(implicit p: Parameters) extends BridgeModule{
  val io = IO(new Bundle {
    // AXI4 Interface
    val axi_ar = Flipped(Decoupled(new ARFlit(axiParams)))
    val axi_r  = Decoupled(new RFlit(axiParams))

    // CHI Interface
    val chi_txreq = Decoupled(new ReqFlit)
    val chi_rxrsp = Flipped(Decoupled(new RespFlit))
    val chi_rxdat = Flipped(Decoupled(new DataFlit))
  })
  io.axi_ar <> DontCare
  io.axi_r  <> DontCare
  io.chi_txreq <> DontCare
  io.chi_rxrsp <> DontCare
  io.chi_rxdat <> DontCare

  io.axi_ar.ready := true.B


  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  val arEntrys        = RegInit(VecInit(Seq.fill(nrEntrys){0.U.asTypeOf(new AREntry)}))
  val arFreeEntrys    = arEntrys.map(_.state === false.B)

  
  
}