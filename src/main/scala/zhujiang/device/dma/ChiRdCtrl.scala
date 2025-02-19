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

class ChiRdCtrl(implicit p: Parameters) extends ZJModule with HasCircularQueuePtrHelper {
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
    val axiAr  = Flipped(Decoupled(new ARFlit(axiParams)))
    val reqDB  = Decoupled(Bool())
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
  // AxiRd to ChiRd Entrys
  private val chiEntrys = Reg(Vec(dmaParams.chiEntrySize, new CHIREntry))
  // Pointer
  private val headPtr   = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val tailPtr   = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val reqDBPtr  = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val txReqPtr  = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val rxRctPtr  = RegInit(CirQChiEntryPtr(f = false.B, v = 0.U))
  private val txDatPtr  = RegInit(0.U.asTypeOf(new ChiDBPtr(dmaParams.chiEntrySize)))
  //Wire define
  //TODO:delete wire
  private val rcvIsRct   = WireInit(io.chiRsp.fire & io.chiRsp.bits.Opcode === RspOpcode.ReadReceipt)
  private val txReqBdl   = WireInit(0.U.asTypeOf(new DmaReqFlit))
  private val dataTxnid  = io.chiDat.bits.TxnID(log2Ceil(dmaParams.chiEntrySize) - 1, 0)
  private val txDatBdl   = WireInit(0.U.asTypeOf(io.rdDB.bits))

/* 
 * Pointer logic
 */
  headPtr  := Mux(io.axiAr.fire, headPtr + 1.U, headPtr)
  reqDBPtr := Mux(io.reqDB.fire, reqDBPtr + 1.U, reqDBPtr)
  txReqPtr := Mux(io.chiReq.fire, txReqPtr + 1.U, txReqPtr)
  rxRctPtr := Mux(rcvIsRct, rxRctPtr + 1.U, rxRctPtr)
  tailPtr  := Mux(tailPtr =/= rxRctPtr & !(txDatPtr.flag === tailPtr.flag & txDatPtr.set === tailPtr.value), tailPtr + 1.U, tailPtr)
  when(io.rdDB.fire){
    txDatPtr.PtrRdAdd(chiEntrys(txDatPtr.set))
  }

  assert(reqDBPtr <= headPtr)
  assert(txReqPtr <= reqDBPtr)
//  assert(txDatPtr <= txReqPtr)
  //
  assert(tailPtr <= rxRctPtr)
  assert(rxRctPtr <= txReqPtr)


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

/* 
 * Assign logic
 */
  chiEntrys.zipWithIndex.foreach{
    case(e, i) =>
      when(headPtr.value === i.U & io.axiAr.fire){
        e.ARMesInit(io.axiAr.bits) //TODO: change name => initArMsg
      }.elsewhen(reqDBPtr.value === i.U & io.reqDB.fire){
        e.dbSite1  := io.respDB.bits.buf(0)
        e.dbSite2  := io.respDB.bits.buf(1)
      }.elsewhen(dataTxnid === i.U & io.wrDB.fire & io.chiDat.bits.DataID === 0.U){ //TODO: change when and DCT logic
        e.haveWrDB1 := true.B
      }.elsewhen(dataTxnid === i.U & io.wrDB.fire & io.chiDat.bits.DataID === 2.U){
        e.haveWrDB2 := true.B
      }
  }
  txReqBdl.RReqInit(chiEntrys(txReqPtr.value), txReqPtr.value)
  txDatBdl.SetBdl(chiEntrys(txDatPtr.set), txDatPtr)


/* 
 * IO logic
 */
  io.axiAr.ready     := !isFull(headPtr, tailPtr)
  io.reqDB.valid     := reqDBPtr =/= headPtr
  io.reqDB.bits      := chiEntrys(reqDBPtr.value).double
  io.chiReq.valid    := (txReqPtr =/= reqDBPtr) & ((rxRctPtr === txReqPtr) | (rxRctPtr =/= txReqPtr) & rcvIsRct)
  io.chiReq.bits     := txReqBdl
  io.wrDB.bits.data  := io.chiDat.bits.Data
  io.wrDB.bits.set   := Mux(chiEntrys(dataTxnid).double & io.chiDat.bits.DataID === 2.U, chiEntrys(dataTxnid).dbSite2, chiEntrys(dataTxnid).dbSite1)
  //TODO:Delete DCT logic
  io.wrDB.valid      := Mux(chiEntrys(dataTxnid).double, io.chiDat.valid, 
                          Mux(fromDCT(io.chiDat.bits.SrcID), Mux(chiEntrys(dataTxnid).addr(5), io.chiDat.bits.DataID === 2.U & io.chiDat.valid, 
                            io.chiDat.valid & io.chiDat.bits.DataID === 0.U), io.chiDat.valid))
  // TODO:  change this logic to true
  io.chiDat.ready    := io.wrDB.ready
  io.chiRsp.ready    := true.B
  io.rdDB.bits       := txDatBdl
  //TODO: change logic
  io.rdDB.valid      := (chiEntrys(txDatPtr.set).haveWrDB1 & chiEntrys(txDatPtr.set).haveWrDB2 & chiEntrys(txDatPtr.set).double |
                        !chiEntrys(txDatPtr.set).double & chiEntrys(txDatPtr.set).haveWrDB1) & !(txDatPtr.set === headPtr.value & txDatPtr.flag === headPtr.flag)
  

/* 
 * Assertion
 */

 when(io.chiDat.valid){
  assert(io.chiDat.bits.Opcode === DatOpcode.CompData)
 }

}
