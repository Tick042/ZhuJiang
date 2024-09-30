package xijiang.bridge.test

import zhujiang._
import zhujiang.chi._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xijiang.bridge.parameter.CHIOp._
import xijiang.bridge.parameter.BridgeParam
import xijiang.bridge.parameter.BridgeParamKey

object DDRState {
  val width        = 2
  val Free         = "b00".U
  val SendDBIDResp = "b01".U
  val WaitData     = "b10".U
  val WriteData    = "b11".U
}

class DDREntry(implicit p: Parameters) extends Bundle {
    val state           = UInt(DDRState.width.W)
    val datVal          = Vec(2, Bool())
    val data            = Vec(2, UInt(256.W))
    val addr            = UInt(64.W)
}

class FakeCHISlave(implicit p : Parameters) extends Module {
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
      val raddr = Output(UInt(64.W))
      val rData = Input(UInt(64.W))

      val wen   = Output(Bool())
      val waddr = Output(UInt(64.W))
      val wmask = Output(UInt(64.W))
      val wdata = Output(UInt(64.W))
    })
  //---------------------------------------------------------------------------------------------------------------------------------//
  //-------------------------------------------------- Reg and Wire Define ----------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//
  val mem          = Seq.fill(2) { Module(new MemHelper()) }
  val wrBufRegVec  = RegInit(VecInit(Seq.fill(16){0.U.asTypeOf(new DDREntry)}))

  val bufFreeVec     = wrBufRegVec.map(_.state === DDRState.Free)
  val bufSendDBIDVec = wrBufRegVec.map(_.state === DDRState.SendDBIDResp)
  val bufWaitDataVec = wrBufRegVec.map(_.state === DDRState.WaitData)

  val selFreeBuf   = PriorityEncoder(bufFreeVec)
  val selSendDBIDBuf = PriorityEncoder(bufSendDBIDVec)
  
  
  val receiptValid = RegInit(false.B)
  val receiptGen   = Wire(io.txreq.bits.Opcode === REQ.ReadOnce && io.txreq.fire && io.txreq.bits.Order =/= 0.U)
  val readReceipt  = RegInit(0.U.asTypeOf(new RespFlit))

  val dbidResp     = RegInit(0.U.asTypeOf(new RespFlit))
  val dbidValid    = Wire((io.txreq.bits.Opcode === REQ.WriteUniqueFull || io.txreq.bits.Opcode === REQ.WriteUniquePtl) & io.txreq.fire)


  //---------------------------------------------------------------------------------------------------------------------------------//
  //------------------------------------------------------- Logic -------------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  /* 
   * Generate ReadReceipt signal
   */

  when(receiptGen){
    receiptValid := true.B
  }
  when(io.rxrsp.fire){
    receiptValid := false.B
  }

  when(receiptValid){
    readReceipt.TxnID  := io.txreq.bits.TxnID
    readReceipt.Opcode := RSP.ReadReceipt
  }
  
  io.rxrsp.bits  := Mux(receiptValid, readReceipt, 0.U.asTypeOf(readReceipt))
  io.rxrsp.valid := receiptValid

  /* 
   * Generate CompDBIDResp
   */

  when(dbidValid){
    // dbidResp.DBID   := selFreeBuf
    dbidResp.TxnID  := io.txreq.bits.TxnID
    dbidResp.Opcode := RSP.CompDBIDResp
  }


  /* 
   * FSM Updata
   */

  wrBufRegVec.zipWithIndex.foreach{
    case(w, i) =>
      switch(w.state){
        is(DDRState.Free){
          val hit = dbidValid & selFreeBuf === i.U
          when(hit){
            w.state := DDRState.SendDBIDResp
            w.addr  := io.txreq.bits.Addr
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
            w.datVal
          }
        }
      }
  }


  //---------------------------------------------------------------------------------------------------------------------------------//
  //---------------------------------------------------- IO Interface ---------------------------------------------------------------//
  //---------------------------------------------------------------------------------------------------------------------------------//

  io.rxrsp.bits  := Mux(receiptValid, readReceipt, 0.U.asTypeOf(readReceipt))
  io.rxrsp.valid := receiptValid

  io.txreq.ready := bufFreeVec.reduce(_|_)
  io.txdat.ready := bufWaitDataVec.reduce(_|_)

}