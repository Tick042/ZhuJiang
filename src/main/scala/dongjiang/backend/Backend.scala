package dongjiang.backend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import dongjiang.directory.DirEntry
import xijiang.Node
import xs.utils.debug.{DomainInfo, HardwareAssertion}
import dongjiang.directory.{DirEntry, DirMsg}
import dongjiang.frontend.decode.Operations

class Backend(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    // CHI TX
    val txReqVec      = Vec(nrIcn, Decoupled(new ReqFlit(true)))
    val txSnpVec      = Vec(nrIcn, Decoupled(new SnoopFlit()))
    val txRspVec      = Vec(nrIcn, Decoupled(new RespFlit()))
    // CHI RX
    val rxRspVec      = Vec(nrIcn, Flipped(Decoupled(new RespFlit())))
    val rxDat         = Flipped(Valid(new DataFlit())) // Dont use rxDat.Data/BE in Backend
    // CHI RESP From Frontend
    val fastResp      = Flipped(Decoupled(new RespFlit()))
    // Update PoS Message
    val updPosTagVec  = Vec(djparam.nrDirBank, Valid(new Addr with HasPosIndex))
    val cleanPosVec   = Vec(djparam.nrDirBank, Valid(new DJBundle with HasPosIndex {
      val isSnp       = Bool()
    }))
    // Write Directory
    val writeDir      = new DJBundle {
      val llc         = Decoupled(new DirEntry("llc") with HasPosIndex)
      val sf          = Decoupled(new DirEntry("sf")  with HasPosIndex)
    }
    // Write Directory Resp
    val respDir       = Flipped(Valid(new DJBundle {
      val llc         = new DirEntry("llc")
      val sf          = new DirEntry("sf")
    }))
    // Clean Signal to Directory
    val unlockVec2    = Vec(djparam.nrDirBank, Vec(2, Valid(new PosIndex())))
    // Multicore Req running in LAN
    val multicore     = Bool()
    // From Frontend
    val commitVec     = Vec(djparam.nrDirBank, Flipped(Valid(new DJBundle with HasLLCTxnID {
      val chi         = new ChiTask
      val dir         = new DirMsg()
      val ops         = new Operations()
      val alrDeqDB    = Bool()
    })))
    // From Frontend
    val cmTaskVec     = Vec(djparam.nrDirBank, Flipped(Decoupled(new DJBundle with HasLLCTxnID {
      val chi         = new ChiTask with HasAddr
      val ops         = new Operations()
      val alrDeqDB    = Bool()
    })))
  })
  io <> DontCare

}