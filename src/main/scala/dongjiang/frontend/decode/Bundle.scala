package dongjiang.frontend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.chi._
import dongjiang.bundle._
import xs.utils.ParallelLookUp
import math.max
import dongjiang.bundle.ChiChannel._

class ReqInst extends Bundle {
  val channel = UInt(ChiChannel.width.W)
  val toLAN   = Bool() // true -> LAN; false -> BBN; only use in REQ
  val opcode  = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
}

class StateInst extends Bundle {
  val valid       = Bool()
  val srcState    = UInt(ChiState.width.W)
  val othState    = UInt(ChiState.width.W)
  val llcState    = UInt(ChiState.width.W)
  val expCompAck  = Bool()
}


trait HasOperations { this: Bundle =>
  val snoop     = Bool()
  val read      = Bool()
  val dataless  = Bool()
  val wriOrAtm  = Bool() // Write or Atomic
}

class Operations extends Bundle with HasOperations

object SnpTgt {
  val width = 2
  val NONE  = "b00".U
  val ALL   = "b01".U
  val ONE   = "b10".U
  val OTH   = "b11".U
}

trait HasCodes { this: Bundle with HasOperations =>
  // Snoop
  val retToSrc    = Bool()
  val snpTgt      = UInt(SnpTgt.width.W)

  // Common
  val opcode      = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val needDB      = Bool()
  val expCompAck  = Bool()
  val canNest     = Bool()
  val nothing     = Bool()
}

class Code extends Bundle with HasOperations with HasCodes

object Decode {
  def decode(inst: UInt, table: Seq[(UInt, UInt)]): Code = {
    ParallelLookUp(
      inst,
      table
    ).asTypeOf(new Code)
  }
}


object Inst {
  def isReq             : UInt = { val temp = WireInit(0.U.asTypeOf(new ReqInst()));   temp.channel     := REQ;     temp.asUInt }
  def isSnp             : UInt = { val temp = WireInit(0.U.asTypeOf(new ReqInst()));   temp.channel     := SNP;     temp.asUInt }
  def toLAN             : UInt = { val temp = WireInit(0.U.asTypeOf(new ReqInst()));   temp.toLAN       := true.B;  temp.asUInt }
  def toBBN             : UInt = { val temp = WireInit(0.U.asTypeOf(new ReqInst()));   temp.toLAN       := false.B; temp.asUInt }
  def opIs    (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new ReqInst()));   temp.opcode      := x;       temp.asUInt }

  def srcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.srcState    := x;       temp.valid := true.B; temp.asUInt }
  def othIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.othState    := x;       temp.asUInt }
  def llcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.llcState    := x;       temp.asUInt }
  def reqExpCompAck     : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.expCompAck  := true.B;  temp.asUInt }
}



object Code {
  def snpAll            : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.ALL;  temp.asUInt }
  def snpOne            : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.ONE;  temp.asUInt }
  def snpOth            : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.OTH;  temp.asUInt }
  def read              : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.read       := true.B;  temp.asUInt }
  def dataless          : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.dataless   := true.B;  temp.asUInt }
  def wriOrAtm          : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.wriOrAtm   := true.B;  temp.asUInt }

  def retToSrc          : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.retToSrc   := true.B;  temp.asUInt }
  def opcode  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.opcode     := true.B;  temp.asUInt }
  def needDB            : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.needDB     := true.B;  temp.asUInt }
  def expCompAck        : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.expCompAck := true.B;  temp.asUInt }
  def canNest           : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.canNest    := true.B;  temp.asUInt }
  def nothing           : UInt = { val temp = WireInit(0.U.asTypeOf(new Code())); temp.nothing    := true.B;  temp.asUInt }
  def error             : UInt = { val temp = WireInit(0.U.asTypeOf(new Code()));                             temp.asUInt }
}

