package xijiang.bridge.test

import zhujiang._
import zhujiang.chi._
import chisel3._
import chisel3.util._
import xijiang.bridge.parameter._
import org.chipsalliance.cde.config._
import xijiang.bridge.parameter.CHIOp._
import xijiang.bridge.parameter.BridgeParam
import xijiang.bridge.parameter.BridgeParamKey
import freechips.rocketchip.diplomacy.BufferParams.pipe
import xijiang.bridge.parameter.CHIOp.REQ.ReadOnce
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.regmapper.RegField.r

object DDRState {
  val width        = 2
  val Free         = "b00".U
  val SendDBIDResp = "b01".U
  val WaitData     = "b10".U
  val WriteData    = "b11".U
}

class DDREntry(implicit p: Parameters) extends BridgeBundle {
    val state           = UInt(DDRState.width.W)
    val datVal          = Vec(nrBeat, Bool())
    val data            = Vec(nrBeat, UInt(fakeMemBits.W))
    val addr            = UInt(fakeMemBits.W)
    val txnid           = UInt(12.W)
}

class FakeCHISlave(implicit p : Parameters) extends BridgeModule {
  //---------------------------------------------------------------------------------------------------------------------------------//
  //----------------------------------------------------- IO Bundle -----------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  val io = IO(new Bundle{

    //CHI interface
      val txreq = Flipped(DecoupledIO(new ReqFlit))
      val txdat = Flipped(DecoupledIO(new DataFlit))
      val rxrsp = DecoupledIO(new RespFlit)
      val rxdat = DecoupledIO(new DataFlit)

    //Mem interface
      val ren   = Output(Bool())
      val raddr = Output(UInt(fakeMemBits.W))
      val rData = Input(UInt(fakeMemBits.W))

      val wen   = Output(Bool())
      val waddr = Output(UInt(fakeMemBits.W))
      val wmask = Output(UInt(fakeMemBits.W))
      val wdata = Output(UInt(fakeMemBits.W))
    })
  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  val mem          = Seq.fill(nrBeat) { Module(new MemHelper()) }
  val wrBufRegVec  = RegInit(VecInit(Seq.fill(16){0.U.asTypeOf(new DDREntry)}))

  val rDataQueue   = Module(new Queue(Vec(nrBeat, UInt(chiBeatBits.W)), entries = 4, flow = false, pipe = true))
  val rReqQueue    = Module(new Queue(new ReqFlit, entries = 4, flow = false, pipe = true))
  val rDataFlitQ   = Module(new Queue(new DataFlit, entries = 4, flow = false, pipe = true))


  val bufFreeVec     = wrBufRegVec.map(_.state === DDRState.Free)
  val bufSendDBIDVec = wrBufRegVec.map(_.state === DDRState.SendDBIDResp)
  val bufWaitDataVec = wrBufRegVec.map(_.state === DDRState.WaitData)

  val selFreeBuf   = PriorityEncoder(bufFreeVec)
  val selSendDBIDBuf = PriorityEncoder(bufSendDBIDVec)
  
  
  val receiptValid = RegInit(false.B)
  val receiptGen   = Wire(rReqQueue.io.deq.fire)
  val readReceipt  = RegInit(0.U.asTypeOf(new RespFlit))

  val dbidValid    = Wire((io.txreq.bits.Opcode === REQ.WriteUniqueFull || io.txreq.bits.Opcode === REQ.WriteUniquePtl) & io.txreq.fire)
  
  val sendBeatNumReg = RegInit(0.U(beatNumBits.W))

  //---------------------------------------------------------------------------------------------------------------------------------//
  //------------------------------------------------------- Logic -------------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  /* 
   * Generate ReadReceipt signal
   */

  when(receiptGen){
    receiptValid := true.B
  }
  when(io.rxrsp.fire & io.rxrsp.bits.Opcode === RSP.ReadReceipt){
    receiptValid := false.B
  }

  when(receiptValid){
    readReceipt.TxnID  := io.txreq.bits.TxnID
    readReceipt.Opcode := RSP.ReadReceipt
  }


  /* 
   * Read data from fake mem
   */
  
   when(io.txreq.fire & io.txreq.bits.Opcode === REQ.ReadOnce){
    rReqQueue.io.enq <> io.txreq
   }
   mem.foreach { case m => m.clk := clock}
   mem.foreach { case m => 
    m.rIdx := rReqQueue.io.deq.bits.Addr
    m.ren  := rReqQueue.io.deq.fire
  }

  rDataQueue.io.enq.valid := rReqQueue.io.deq.fire
  rDataQueue.io.enq.bits  := Cat(mem.map(_.rdata)).asTypeOf(Vec(nrBeat, UInt(chiBeatBits.W)))
  rDataQueue.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.rxdat.fire
  
  rDataFlitQ.io.deq.ready := sendBeatNumReg === (nrBeat - 1).U & io.rxdat.fire

  sendBeatNumReg := sendBeatNumReg + io.rxdat.fire.asUInt

  rReqQueue.io.deq.ready  := rDataFlitQ.io.enq.ready
  rDataFlitQ.io.enq.valid := rReqQueue.io.deq.valid
  rDataFlitQ.io.enq.bits  := DontCare
  rDataFlitQ.io.enq.bits.Opcode := DAT.CompData
  rDataFlitQ.io.enq.bits.TxnID  := rReqQueue.io.deq.bits.TxnID

  


  /* 
   * Write FSM Updata
   */

  wrBufRegVec.zipWithIndex.foreach{
    case(w, i) =>
      switch(w.state){
        is(DDRState.Free){
          val hit = dbidValid & selFreeBuf === i.U
          when(hit){
            w.state := DDRState.SendDBIDResp
            w.addr  := io.txreq.bits.Addr
            w.txnid := io.txreq.bits.TxnID
          }.otherwise{
            w := 0.U.asTypeOf(w)
          }
        }
        is(DDRState.SendDBIDResp){
          val hit = io.rxrsp.fire & io.rxrsp.bits.Opcode === RSP.CompDBIDResp & selSendDBIDBuf === i.U
          when(hit){
            w.state := DDRState.WaitData
          }
        }
        is(DDRState.WaitData) {
          val hit = io.txdat.fire & io.txdat.bits.TxnID === i.U
          when(hit){
            w.state := Mux(PopCount(w.datVal) === 1.U, DDRState.WriteData, w.state)
            w.datVal(toBeatNum(io.txdat.bits.DataID)) := true.B
            w.data(toBeatNum(io.txdat.bits.DataID))   := io.txdat.bits.Data
          }
        }
        is(DDRState.WriteData) {
          w.state := DDRState.Free
        }
      }
  }


  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- IO Interface ---------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//


  io.txreq.ready := bufFreeVec.reduce(_|_)

  io.txdat.ready := true.B

  io.rxrsp.valid       := bufSendDBIDVec.reduce(_|_) | receiptValid
  io.rxrsp.bits.Opcode := Mux(receiptValid, RSP.ReadReceipt, RSP.CompDBIDResp)
  io.rxrsp.bits.DBID   := selSendDBIDBuf
  io.rxrsp.bits.TxnID  := Mux(receiptValid, readReceipt.TxnID, wrBufRegVec(selSendDBIDBuf).txnid)

  io.rxdat.valid       := rDataQueue.io.deq.valid & rDataFlitQ.io.deq.valid
  io.rxdat.bits        := rDataFlitQ.io.deq.bits
  io.rxdat.bits.Data   := rDataQueue.io.deq.bits(sendBeatNumReg)
  io.rxdat.bits.DataID := toDataID(sendBeatNumReg)

  
}