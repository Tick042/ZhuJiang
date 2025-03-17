package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang._
import dongjiang.bundle._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.SnpOpcode._
import zhujiang.chi._


class ChiTask(implicit p: Parameters) extends DJBundle with HasAddr with HasNodeId with HasChiChannel
  with HasChiOp with HasChiOrderAndExpCompAck with HasChiSnpField with HasChiSize  {
  // REQ
  val txnID       = UInt(ChiTxnIdBits.W)
  val memAttr     = new MemAttr()
  // SNP
  val fwdNID      = UInt(nodeIdBits.W)
  val fwdTxnID    = UInt(ChiFwdTxnIdBits.W)
  val retToSrc    = Bool()
}

class RespTask()