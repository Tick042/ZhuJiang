package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xs.utils.{CircularQueuePtr, HasCircularQueuePtrHelper}
import zhujiang.axi.{AxiParams, WFlit, RFlit}
import zhujiang.{ZJBundle, ZJModule}
import zhujiang.chi.DataFlit
import xs.utils.sram.DualPortSramTemplate
import xijiang.NodeType.RF

class ChiDataBufferCtrlEntry(bufferSize: Int)(implicit p: Parameters) extends ZJBundle {
  val buf = Vec(512 / dw, UInt(log2Ceil(bufferSize).W))
  val num = UInt(3.W)
}

class ChiDataBufferAllocReq(ctrlSize: Int) extends Bundle {
  val double = Bool()
}

class ChiDataBufferTxReq(ctrlSize: Int)(implicit p: Parameters) extends ZJBundle {
  val idxOH = UInt(ctrlSize.W)
  val flit  = new DataFlit
}

class writeRdDataBuffer(bufferSize: Int)(implicit p: Parameters) extends ZJBundle {
  val set    = UInt(log2Ceil(bufferSize).W)
  val data   = UInt(dw.W)
}

class ChiDataBufferFreelist(ctrlSize: Int, bufferSize: Int)(implicit p: Parameters) extends ZJModule with HasCircularQueuePtrHelper {
  private class ChiDataBufferFreelistPtr extends CircularQueuePtr[ChiDataBufferFreelistPtr](bufferSize)
  private object ChiDataBufferFreelistPtr {
    def apply(f: Bool, v: UInt): ChiDataBufferFreelistPtr = {
        val ptr = Wire(new ChiDataBufferFreelistPtr)
        ptr.flag := f
        ptr.value := v
        ptr
    }
  }
  val io = IO(new Bundle {
    val req     = Flipped(Decoupled(new ChiDataBufferAllocReq(ctrlSize)))
    val resp    = Valid(new ChiDataBufferCtrlEntry(bufferSize))
    val release = Input(Valid(UInt(log2Ceil(bufferSize).W)))
    val idle    = Output(Bool())
  })
  private val freelist       = RegInit(VecInit(Seq.tabulate(bufferSize)(_.U(log2Ceil(bufferSize).W))))
  private val headPtr        = RegInit(ChiDataBufferFreelistPtr(f = false.B, v = 0.U))
  private val tailPtr        = RegInit(ChiDataBufferFreelistPtr(f = true.B, v = 0.U))
  private val availableSlots = RegInit(bufferSize.U(log2Ceil(bufferSize + 1).W))
  assert(availableSlots === distanceBetween(tailPtr, headPtr))

  private val dataWidthInBytesShift = log2Ceil(dw / 8)
  private val allocMoreThanOne      = io.req.bits.double

  private val reqNum = Mux(allocMoreThanOne, 2.U, 1.U)
  io.req.ready   := availableSlots >= reqNum

  for(i <- io.resp.bits.buf.indices) {
    io.resp.bits.buf(i) := freelist((headPtr + i.U).value)
  }
  io.resp.valid       := io.req.fire
  io.resp.bits.num    := reqNum - 1.U
  io.idle             := headPtr.value === tailPtr.value && headPtr.flag =/= tailPtr.flag

  private val allocNum = Mux(io.req.fire, reqNum, 0.U)
  private val relNum   = Mux(io.release.valid, 1.U, 0.U)

  when(io.req.fire || io.release.valid) {
    headPtr      := headPtr + allocNum
    tailPtr      := tailPtr + relNum
    availableSlots := (availableSlots +& relNum) - allocNum
  }
  freelist(tailPtr.value) := Mux(io.release.valid, io.release.bits, freelist(tailPtr.value))
  // private val releaseMatchSeq = for(i <- io.release.bits.buf.indices) yield (io.release.valid, (tailPtr + i.U).value)
  // private val releaseEntrySeq = for(i <- io.release.bits.buf.indices) yield io.release.bits.buf(i)
  // for(idx <- freelist.indices){
  //   val releaseMatch = releaseMatchSeq.map(elm => elm._1 && elm._2 === idx.U)
  //   when(Cat(releaseMatch).orR) {
  //       freelist(idx) := Mux1H(releaseMatch, releaseEntrySeq)
  //   }
  // }
}

class ChiDataBufferRdRam(axiParams: AxiParams, bufferSize: Int)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
      val writeDataReq = Flipped(Decoupled(new writeRdDataBuffer(bufferSize)))
      val readDataReq  = Flipped(Decoupled(new readRdDataBuffer(bufferSize)))
      val readDataResp = Decoupled(new RFlit(axiParams))
  })

  private val dataRam = Module(new DualPortSramTemplate(
      gen = UInt(dw.W),
      set = bufferSize/2
  ))

  private val wrRamQ = Module(new Queue(new writeRdDataBuffer(bufferSize), entries = 2, flow = false, pipe = false))
  private val readRamStage1Pipe = Module(new Queue(new readRdDataBuffer(bufferSize), entries = 1, pipe = true))
  private val readRamStage2Pipe = Module(new Queue(new respDataBuffer(bufferSize), entries = 2, pipe = true))
  private val sendDataCnt = RegInit(0.U(4.W))
  private val rFlitBdl    = WireInit(0.U.asTypeOf(new RFlit(axiParams)))

  wrRamQ.io.deq.ready     := true.B

  wrRamQ.io.enq <> io.writeDataReq

  dataRam.io.wreq.valid         := wrRamQ.io.deq.valid
  dataRam.io.wreq.bits.data(0)  := wrRamQ.io.deq.bits.data
  dataRam.io.wreq.bits.addr     := wrRamQ.io.deq.bits.set
  // dataRam.io.wreq.bits.mask.get := Fill(bew, 1.U)

  readRamStage1Pipe.io.enq.valid         := io.readDataReq.valid
  readRamStage1Pipe.io.enq.bits.id       := io.readDataReq.bits.id
  readRamStage1Pipe.io.enq.bits.last     := io.readDataReq.bits.last
  readRamStage1Pipe.io.enq.bits.resp     := io.readDataReq.bits.resp
  readRamStage1Pipe.io.enq.bits.sendCnts := io.readDataReq.bits.sendCnts
  readRamStage1Pipe.io.enq.bits.set      := io.readDataReq.bits.set
  readRamStage1Pipe.io.deq.ready         := readRamStage2Pipe.io.enq.ready

  readRamStage2Pipe.io.enq.valid         := readRamStage1Pipe.io.deq.valid
  readRamStage2Pipe.io.enq.bits.id       := readRamStage1Pipe.io.deq.bits.id
  readRamStage2Pipe.io.enq.bits.last     := readRamStage1Pipe.io.deq.bits.last
  readRamStage2Pipe.io.enq.bits.resp     := readRamStage1Pipe.io.deq.bits.resp
  readRamStage2Pipe.io.enq.bits.data     := dataRam.io.rresp.bits.asUInt
  readRamStage2Pipe.io.enq.bits.sendCnts := readRamStage1Pipe.io.deq.bits.sendCnts
  readRamStage2Pipe.io.deq.ready         := sendDataCnt + 1.U === readRamStage2Pipe.io.deq.bits.sendCnts
  
  dataRam.io.rreq.valid     := io.readDataReq.valid & readRamStage1Pipe.io.enq.ready
  dataRam.io.rreq.bits      := io.readDataReq.bits.set

  rFlitBdl          := 0.U.asTypeOf(rFlitBdl)
  rFlitBdl.id       := readRamStage2Pipe.io.deq.bits.id
  rFlitBdl.data     := readRamStage2Pipe.io.deq.bits.data
  rFlitBdl.resp     := readRamStage2Pipe.io.deq.bits.resp
  rFlitBdl.last     := readRamStage2Pipe.io.deq.bits.last && readRamStage2Pipe.io.deq.ready
  rFlitBdl.user     := 0.U

  sendDataCnt  := Mux(io.readDataResp.fire & !io.readDataResp.bits.last & !readRamStage2Pipe.io.deq.ready, sendDataCnt + 1.U, 
                    Mux(io.readDataResp.fire & (io.readDataResp.bits.last | readRamStage2Pipe.io.deq.ready), 0.U, sendDataCnt))

  io.readDataReq.ready   := readRamStage1Pipe.io.enq.ready
  io.readDataResp.valid  := readRamStage2Pipe.io.deq.valid
  io.readDataResp.bits   := rFlitBdl
}


class DataBufferForRead(axiParams: AxiParams, bufferSize: Int, ctrlSize: Int)(implicit p: Parameters) extends ZJModule {
  val io = IO(new Bundle {
    val alloc    = Flipped(Decoupled(new ChiDataBufferAllocReq(ctrlSize)))
    val axiR     = Decoupled(new RFlit(axiParams))
    val wrDB     = Flipped(Decoupled(new writeRdDataBuffer(bufferSize)))
    val rdDB     = Flipped(Decoupled(new readRdDataBuffer(bufferSize)))
    val allocRsp = Valid(new ChiDataBufferCtrlEntry(bufferSize))
  })

  private val dataBuffer   = Module(new ChiDataBufferRdRam(axiParams, bufferSize))
  private val freelist     = Module(new ChiDataBufferFreelist(ctrlSize, bufferSize))
  private val ctrlValidVec = RegInit(VecInit(Seq.fill(ctrlSize)(false.B)))

  freelist.io.req.valid     := io.alloc.valid
  freelist.io.req.bits      := io.alloc.bits
  freelist.io.release.valid := dataBuffer.io.readDataReq.fire
  freelist.io.release.bits  := dataBuffer.io.readDataReq.bits.set

  dataBuffer.io.readDataReq  <> io.rdDB
  dataBuffer.io.writeDataReq <> io.wrDB
  io.axiR                    <> dataBuffer.io.readDataResp

  io.alloc.ready       := freelist.io.req.ready
  io.allocRsp.valid    := freelist.io.resp.valid
  io.allocRsp.bits     := freelist.io.resp.bits
}