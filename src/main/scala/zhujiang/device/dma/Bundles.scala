package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import chisel3.util.RRArbiter
import xs.utils.ResetRRArbiter
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._
import xijiang.{Node, NodeType}
import freechips.rocketchip.amba.ahb.AHBImpMaster.bundle
import zhujiang.chi.ReqOpcode.AtomicLoadUMIN

object DmaDef {
    def RREncoder(in: Seq[Bool]): UInt = {
        val arb = Module(new ResetRRArbiter(UInt(log2Ceil(in.size).W), in.size))
        arb.io.in.zipWithIndex.foreach {
            case(a, i) =>
                a.valid := in(i)
                a.bits  := i.U
        }
        arb.io.out.ready := true.B
        arb.io.out.bits
    }
}


object BurstMode {
  val width        = 2
  val Fix          = "b00".U
  val Incr         = "b01".U
  val Wrap         = "b10".U
  val Reserve      = "b11".U
}
class AllocChiReq(implicit p: Parameters) extends ZJBundle {
  val addr         = UInt(raw.W)
  val sendDataCnts = UInt(4.W)
  val double       = Bool()  //alloc databuffer 
  val arId         = UInt(zjParams.dmaParams.idBits.W)
  val last         = Bool()
}

class AXIREntry(implicit p: Parameters) extends ZJBundle {
    val addr            = UInt(raw.W)
    val burst           = UInt(BurstMode.width.W)
    val num             = UInt(8.W)
    val cnt             = UInt(8.W)
    val arid            = UInt(8.W)
    val byteMask        = UInt(9.W)
    val size            = UInt(3.W)

    def byteComp(len:UInt) = {
      val maxShift = 1 << 3
      val tail = ((BigInt(1) << maxShift) - 1).U
      (Cat(len, tail) << log2Ceil(bew).U) >> maxShift
    }

    def axiREntryInit[T <: ARFlit](ar : T): AXIREntry = {
       this.addr   := ar.addr & (~31.U(raw.W))
       this.cnt    := 0.U
       this.num    := (ar.len) << ar.size >> log2Ceil(dw / 8)
       this.byteMask := byteComp(ar.len)
       this.burst    := ar.burst
       this.arid     := ar.id
       this.size     := ar.size
       this
    }
}

class RdDBPtr(chiEntrySize: Int) extends Bundle {
  val set    = UInt(log2Ceil(chiEntrySize).W)
  val poi    = UInt(1.W)
  def PtrAdd[T <: CHIREntry](c: T): RdDBPtr = {
    this.poi := Mux(c.double & this.poi === 0.U, 1.U, 0.U)
    this.set := Mux(!c.double | c.double & this.poi === 1.U, this.set + 1.U, this.set)
    this
  }
}

class readRdDataBuffer(bufferSize: Int)(implicit p: Parameters) extends ZJBundle {
  val set      = UInt(log2Ceil(bufferSize).W)
  val id       = UInt(zjParams.dmaParams.idBits.W)
  val resp     = UInt(2.W)
  val sendCnts = UInt(4.W)
  val last     = Bool()

  def setBdl[T <: CHIREntry, R <: RdDBPtr](c: T, i: R): readRdDataBuffer = {
    this.id       := c.arId
    this.resp     := 0.U
    this.set      := Mux(i.poi === 1.U, c.dbSite2, c.dbSite1)
    this.sendCnts := c.sendDataCnts
    this.last     := Mux(c.last & c.double & i.poi === 1.U | c.last & !c.double & i.poi === 0.U, true.B, false.B)
    this
  }
}
class respDataBuffer(bufferSize: Int)(implicit p: Parameters) extends ZJBundle {
  val data = UInt(dw.W)
  val id       = UInt(zjParams.dmaParams.idBits.W)
  val resp     = UInt(2.W)
  val sendCnts = UInt(4.W)
  val last     = Bool()
}

class CHIREntry(implicit p : Parameters) extends ZJBundle {
  val arId           = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
  val double         = Bool()
  val sendDataCnts   = UInt(4.W)
  val cnt            = UInt(4.W)
  val addr           = UInt(raw.W)
  val dbSite1        = UInt(log2Ceil(zjParams.dmaParams.bufferSize).W)
  val dbSite2        = UInt(log2Ceil(zjParams.dmaParams.bufferSize).W)
  val last           = Bool()
  val haveAllocDB    = Bool()
  val haveSendReq    = Bool()
  val haveWrDB       = Bool()

  def chiREntryInit[T <: AllocChiReq](b: T): CHIREntry = {
    this.haveAllocDB      := false.B
    this.haveSendReq      := false.B
    this.haveWrDB         := false.B
    this.cnt              := 0.U
    this.dbSite1          := 0.U
    this.dbSite2          := 0.U
    this.last             := b.last
    this.sendDataCnts     := b.sendDataCnts
    this.double           := b.double
    this.arId             := b.arId
    this.addr             := b.addr
    this
  }
  def chiRAllocDB[T <: ChiDataBufferCtrlEntry](b: T): CHIREntry = {
    this.haveAllocDB := true.B
    this.dbSite1     := b.buf(0)
    this.dbSite2     := b.buf(1)
    this
  }
}

class DmaReqFlit(implicit p : Parameters) extends ReqFlit {
  def RReqInit[T <: CHIREntry](c : T, i : UInt): ReqFlit = {
    this          := 0.U.asTypeOf(this)
    this.Addr     := c.addr
    this.Opcode   := Mux(c.addr(raw - 1), ReqOpcode.ReadNoSnp, ReqOpcode.ReadOnce)
    this.SrcID    := 1.U
    this.Order    := "b11".U
    this.TxnID    := i
    this.Size     := Mux(c.double, 6.U, 5.U)
    this
  }
}

class AXIWEntry(implicit p: Parameters) extends ZJBundle {
    val addr            = UInt(raw.W)
    val unalign         = Bool()
    val burst           = UInt(BurstMode.width.W)
    val awid            = UInt(8.W)
    val byteMask        = UInt(9.W)
}

class CHIWEntry(implicit p: Parameters) extends ZJBundle {
  val areid        = UInt(log2Ceil(zjParams.dmaParams.axiEntrySize).W)
  val dbid         = UInt(12.W)
  val last         = Bool()
  val full         = Bool()
  val sendReqOrder = UInt(log2Ceil(zjParams.dmaParams.chiEntrySize).W)
  val tgtID        = UInt(niw.W)
  val sendFull     = Bool()
  val mmioReq      = Bool()
  val sendAckOrder = UInt(log2Ceil(zjParams.dmaParams.chiEntrySize).W)
  val haveRecComp  = Bool()
}
