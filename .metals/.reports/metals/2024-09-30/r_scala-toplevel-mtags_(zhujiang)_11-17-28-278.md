error id: file://<WORKSPACE>/src/main/scala/xijiang/bridge/test/FakeCHISlave.scala:[287..292) in Input.VirtualFile("file://<WORKSPACE>/src/main/scala/xijiang/bridge/test/FakeCHISlave.scala", "package xijiang.bridge.test

import zhujiang._
import zhujiang.chi._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xijiang.bridge.parameter.CHIOp._
import xijiang.bridge.parameter.BridgeParam
import xijiang.bridge.parameter.BridgeParamKey

object 

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
    dbidResp.DBID   := io.txreq.bits.TxnID + 1.U
    dbidResp.TxnID  := io.txreq.bits.TxnID
    dbidResp.Opcode := RSP.CompDBIDResp
  }





}")
file://<WORKSPACE>/src/main/scala/xijiang/bridge/test/FakeCHISlave.scala
file://<WORKSPACE>/src/main/scala/xijiang/bridge/test/FakeCHISlave.scala:14: error: expected identifier; obtained class
class FakeCHISlave(implicit p : Parameters) extends Module {
^
#### Short summary: 

expected identifier; obtained class