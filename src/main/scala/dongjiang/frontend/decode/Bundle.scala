package dongjiang.frontend.decode

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import zhujiang.chi._
import dongjiang.bundle._
import xs.utils.ParallelLookUp
import math.max
import dongjiang.bundle.ChiChannel._

class Insts extends Bundle {
  val channel       = UInt(ChiChannel.width.W)
  val toLAN         = Bool() // true -> LAN; false -> BBN; only use in REQ
  val opcode        = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val srcState      = UInt(ChiState.width.W)
  val othState      = UInt(ChiState.width.W)
  val llcState      = UInt(ChiState.width.W)
  val expCompAck    = Bool()
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

trait HasDecode { this: Bundle with HasOperations =>
  // Snoop
  val retToSrc    = Bool()
  val snpTgt      = UInt(SnpTgt.width.W)

  // Common
  val opcode      = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val needDB      = Bool()
  val expCompAck  = Bool()
  val canBeNest   = Bool()
  val nothing     = Bool()
}

class Decode extends Bundle with HasOperations with HasDecode {
  def decode(insts: Insts, table: Seq[(UInt, UInt)]): Decode = {
    this := ParallelLookUp(
      insts.asUInt,
      table
    ).asTypeOf(new Decode)
    this
  }
}


object Inst {
  def chnlIs  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.channel     := x;       temp.asUInt }
  def isReq             : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.channel     := REQ;     temp.asUInt }
  def isSnp             : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.channel     := REQ;     temp.asUInt }
  def toLAN             : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.toLAN       := true.B;  temp.asUInt }
  def toBBN             : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.toLAN       := false.B; temp.asUInt }
  def opIs    (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.opcode      := x;       temp.asUInt }
  def srcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.srcState    := x;       temp.asUInt }
  def othIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.othState    := x;       temp.asUInt }
  def llcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.llcState    := x;       temp.asUInt }
  def reqExpCompAck     : UInt = { val temp = WireInit(0.U.asTypeOf(new Insts())); temp.expCompAck  := true.B;  temp.asUInt }
}



object Code {
  def snpAll            : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.ALL;  temp.asUInt }
  def snpOne            : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.ONE;  temp.asUInt }
  def snpOth            : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.snoop      := true.B;  temp.snpTgt := SnpTgt.OTH;  temp.asUInt }
  def read              : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.read       := true.B;  temp.asUInt }
  def dataless          : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.dataless   := true.B;  temp.asUInt }
  def wriOrAtm          : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.wriOrAtm   := true.B;  temp.asUInt }

  def retToSrc          : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.retToSrc   := true.B;  temp.asUInt }
  def opcode  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.opcode     := true.B;  temp.asUInt }
  def needDB            : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.needDB     := true.B;  temp.asUInt }
  def expCompAck        : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.expCompAck := true.B;  temp.asUInt }
  def canBeNest         : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.canBeNest  := true.B;  temp.asUInt }
  def nothing           : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode())); temp.nothing    := true.B;  temp.asUInt }
  def error             : UInt = { val temp = WireInit(0.U.asTypeOf(new Decode()));                             temp.asUInt }
}

