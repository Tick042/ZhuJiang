package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import org.chipsalliance.cde.config.Parameters
import zhujiang._
import zhujiang.chi._
import zhujiang.axi._
import xs.utils.sram._
import xijiang._
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
import freechips.rocketchip.tilelink.TLMessages.d

class AxiREntrys(implicit p: Parameters) extends ZJModule with HasCircularQueuePtrHelper {
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)

  private class CirQAxiEntryPtr extends CircularQueuePtr[CirQAxiEntryPtr](dmaParams.axiEntrySize)
  private object CirQAxiEntryPtr {
  def apply(f: Bool, v: UInt): CirQAxiEntryPtr = {
        val ptr = Wire(new CirQAxiEntryPtr)
        ptr.flag := f
        ptr.value := v
        ptr
    }
  }
  val io = IO(new Bundle {
    val ar    = Flipped(Decoupled(new ARFlit(axiParams)))
    val alloc = Decoupled(new AllocChiReq)
  })

/* 
 * Reg Define
 */
  private val uEntrys    = Reg(Vec(dmaParams.axiEntrySize, new AXIREntry))
  private val uHeadPtr   = RegInit(CirQAxiEntryPtr(f = false.B, v = 0.U))
  private val uTailPtr   = RegInit(CirQAxiEntryPtr(f = false.B, v = 0.U))

/* 
 * Logic
 */
  when(io.ar.fire){
    uEntrys(uHeadPtr.value).axiREntryInit(io.ar.bits)
    uHeadPtr := uHeadPtr + 1.U
  }
  private val uTail = WireInit(uEntrys(uTailPtr.value))
  when(io.alloc.fire){
    assert(uHeadPtr =/= uTailPtr)
    uTailPtr := Mux(io.alloc.bits.last, uTailPtr + 1.U, uTailPtr)
    uEntrys(uTailPtr.value).cnt  := Mux(io.alloc.bits.double, uEntrys(uTailPtr.value).cnt + 2.U, uEntrys(uTailPtr.value).cnt + 1.U)
    uEntrys(uTailPtr.value).addr := Mux(uTail.burst === BurstMode.Incr, 
                                      Mux(io.alloc.bits.double, uTail.addr + (dw/4).U, uTail.addr + (dw/8).U),
                                        Mux(uTail.burst === BurstMode.Wrap, (uTail.addr + (dw/8).U) & uTail.byteMask | ~(~uTail.addr | uTail.byteMask), uTail.addr))
  }
  private val allocReq   = WireInit(0.U.asTypeOf(new AllocChiReq))
  allocReq.addr         := uEntrys(uTailPtr.value).addr
  allocReq.double       := Mux(!uEntrys(uTailPtr.value).addr(5) & uEntrys(uTailPtr.value).burst === BurstMode.Incr & uEntrys(uTailPtr.value).cnt =/= uEntrys(uTailPtr.value).num, true.B, false.B)
  allocReq.last         := Mux(allocReq.double, uEntrys(uTailPtr.value).cnt + 1.U === uEntrys(uTailPtr.value).num, uEntrys(uTailPtr.value).cnt === uEntrys(uTailPtr.value).num)
  allocReq.sendDataCnts := 1.U << log2Ceil(dw / 8) >> uEntrys(uTailPtr.value).size
  allocReq.arId         := uEntrys(uTailPtr.value).arid

/* 
 * IO Interface
 */
  io.ar.ready         := !isFull(uHeadPtr, uTailPtr)
  io.alloc.valid      := uHeadPtr =/= uTailPtr
  io.alloc.bits       := allocReq

/* 
 * 
 */
  when(io.ar.fire && io.ar.bits.burst === BurstMode.Wrap){
    assert(PopCount(io.ar.bits.len + 1.U) === 1.U, "AxiMaster error!, The Wrap of burst mode can not send this len!")
  }

}



class ChiREntrys(implicit p: Parameters) extends ZJModule with HasCircularQueuePtrHelper {
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)

  private class CirQChiEntryPtr extends CircularQueuePtr[CirQChiEntryPtr](dmaParams.chiEntrySize)
  private object CirQChiEntryPtr {
  def apply(f: Bool, v: UInt): CirQChiEntryPtr = {
        val ptr = Wire(new CirQChiEntryPtr)
        ptr.flag := f
        ptr.value := v
        ptr
    }
  }

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(new AllocChiReq))
    val reqDB = Decoupled(new ChiDataBufferAllocReq(dmaParams.chiEntrySize))
    val respDB = Input(Valid(new ChiDataBufferCtrlEntry(dmaParams.bufferSize)))
    val chiReq = Decoupled(new ReqFlit)
    val chiRsp = Flipped(Decoupled(new RespFlit))
    val chiDat = Flipped(Decoupled(new DataFlit))
    val wrDB   = Decoupled(new writeRdDataBuffer(dmaParams.bufferSize))
    val rdDB   = Decoupled(new readRdDataBuffer(dmaParams.bufferSize))
  })

/* 
 * Reg/Wire Define
 */
  private val dEntrys  = Reg(Vec(dmaParams.chiEntrySize, new CHIREntry))
  private val dHeadPtr = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val dTailPtr = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))

  private val allocDBPtr = RegInit(0.U(log2Ceil(dmaParams.chiEntrySize).W))
  private val sendReqPtr = RegInit(0.U(log2Ceil(dmaParams.chiEntrySize).W))
  private val readDBPtr  = RegInit(0.U.asTypeOf(new RdDBPtr(dmaParams.chiEntrySize)))
  
  private val chiReqBdl  = WireInit(0.U.asTypeOf(new DmaReqFlit))
  private val readDBBdl  = WireInit(0.U.asTypeOf(io.rdDB.bits))
  private val dataTxnid  = io.chiDat.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0)
  
/* 
 * Pointers Logic
 */
  when(io.alloc.fire){
    dEntrys(dHeadPtr.value).chiREntryInit(io.alloc.bits)
    dHeadPtr := dHeadPtr + 1.U
  }

  when(io.respDB.valid){
    dEntrys(allocDBPtr).chiRAllocDB(io.respDB.bits)
    allocDBPtr := allocDBPtr + 1.U
  }

/* 
 * Send CHI Req Logic
 */
  dEntrys(sendReqPtr).haveSendReq := Mux(io.chiReq.fire, true.B, dEntrys(sendReqPtr).haveSendReq)
  chiReqBdl.RReqInit(dEntrys(sendReqPtr), sendReqPtr)

/* 
 * Receive CHI Resp Logic
 */
  when(io.chiRsp.fire){
    assert(io.chiRsp.bits.Opcode === RspOpcode.ReadReceipt)
    assert(io.chiRsp.bits.TxnID === sendReqPtr)
    sendReqPtr := sendReqPtr + 1.U
  }

/* 
 * Receive CHI Data Logic 
 */
  when(io.chiDat.fire){
    assert(io.chiDat.bits.Opcode === DatOpcode.CompData)
  }

  def fromDCT(x: UInt): Bool = {
  require(x.getWidth == niw)
  val fromCC = WireInit(false.B)
  if(zjParams.localRing.filter(_.nodeType == NodeType.CC).nonEmpty){
    fromCC := zjParams.localRing.filter(_.nodeType == NodeType.CC).map(_.nodeId.asUInt >> nodeAidBits === x >> nodeAidBits).reduce(_ | _)
  }
  else {
    fromCC := false.B
  }
  fromCC
  }
  io.wrDB.bits.data := io.chiDat.bits.Data
  io.wrDB.bits.set  := Mux(dEntrys(dataTxnid).double && io.chiDat.bits.DataID === 2.U, dEntrys(dataTxnid).dbSite2, dEntrys(dataTxnid).dbSite1)
  io.wrDB.valid     := Mux(dEntrys(dataTxnid).double, io.chiDat.valid, 
                            Mux(fromDCT(io.chiDat.bits.SrcID), Mux(dEntrys(dataTxnid).addr(5), io.chiDat.bits.DataID === 2.U && io.chiDat.valid, 
                              io.chiDat.bits.DataID === 0.U & io.chiDat.valid), io.chiDat.valid))
  dEntrys(dataTxnid).haveWrDB := Mux(io.wrDB.valid && io.chiDat.bits.DataID === 2.U && dEntrys(dataTxnid).double, true.B, 
                                              Mux(io.wrDB.valid, true.B, dEntrys(dataTxnid).haveWrDB))

/* 
 * Read DataBuffer Logic 
 */
  readDBBdl.setBdl(dEntrys(readDBPtr.set), readDBPtr)
  when(io.rdDB.fire){
    readDBPtr.PtrAdd(dEntrys(readDBPtr.set))
  }

 
/* 
 * IO Interface
 */
  io.alloc.ready       := !isFull(dHeadPtr, dTailPtr)
  io.reqDB.valid       := allocDBPtr =/= dHeadPtr.value
  io.reqDB.bits.double := dEntrys(allocDBPtr).double
  io.chiReq.valid      := sendReqPtr =/= dHeadPtr.value && !dEntrys(sendReqPtr).haveSendReq && dEntrys(sendReqPtr).haveAllocDB
  io.chiReq.bits       := chiReqBdl
  io.chiRsp.ready      := true.B
  io.chiDat.ready      := true.B
  io.rdDB.valid        := dEntrys(readDBPtr.set).haveWrDB & readDBPtr.set =/= dHeadPtr.value
  io.rdDB.bits         := readDBBdl
}