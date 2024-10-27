package dongjiang.pcu.exu

import dongjiang._
import dongjiang.pcu._
import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import xs.utils._
import dongjiang.utils.FastArb._

class ExecuteUnit(implicit p: Parameters) extends DJModule {
// --------------------- IO declaration ------------------------//
  val io = IO(new Bundle {
    val valid           = Input(Bool())
    val bank            = Input(UInt(bankBits.W))
    val incoID          = Input(UInt(bankPerPCUBits.W))
    // Intf <> Exu
    val req2Exu         = Flipped(Decoupled(new Req2ExuBundle()))
    val reqAck2Intf     = Decoupled(new ReqAck2IntfBundle())
    val resp2Intf       = Decoupled(new Resp2IntfBundle())
    val req2Intf        = Decoupled(new Req2IntfBundle())
    val resp2Exu        = Flipped(Decoupled(new Resp2ExuBundle()))
    // Req To DataBuffer
    val dbRCReq         = Decoupled(new DBRCReq())
  })

// --------------------- Modules declaration ------------------------//
  val directory     = Module(new DirectoryWrapper())
  val reqPipe       = Module(new ProcessPipe())
  val respPipe      = Module(new ProcessPipe())
  val mshrCtl       = Module(new MSHRCtl())
  val mpReqQueue    = Module(new Queue(gen = new Req2IntfBundle(), entries = djparam.nrExuReqQueue, pipe = true, flow = true))
  val mpRespQueue   = Module(new Queue(gen = new Resp2IntfBundle(),entries = djparam.nrExuRespQueue, pipe = true, flow = true))

// --------------------------- Connection ---------------------------//
  directory.io.earlyRReqVec <> mshrCtl.io.earlyRReqVec
  directory.io.dirRead      <> mshrCtl.io.dirRead
  directory.io.dirWrite(0)  <> respPipe.io.dirWrite // Low bit is high priority
  directory.io.dirWrite(1)  <> reqPipe.io.dirWrite
  directory.io.dirResp(0)   <> respPipe.io.dirResp
  directory.io.dirResp(1)   <> reqPipe.io.dirResp
  directory.io.readMshr     <> mshrCtl.io.dirReadMshr
  directory.io.mshrResp     <> mshrCtl.io.mshrResp2Dir


  mshrCtl.io.bank           := io.bank
  mshrCtl.io.req2Exu        <> io.req2Exu
  mshrCtl.io.reqAck2Intf    <> io.reqAck2Intf
  mshrCtl.io.resp2Exu       <> io.resp2Exu
  mshrCtl.io.pipeTask(0)    <> respPipe.io.task;  assert(!mshrCtl.io.pipeTask(PipeID.RESP).valid | mshrCtl.io.pipeTask(PipeID.RESP).bits.taskMes.pipeID === PipeID.RESP)
  mshrCtl.io.pipeTask(1)    <> reqPipe.io.task;   assert(!mshrCtl.io.pipeTask(PipeID.REQ).valid  | mshrCtl.io.pipeTask(PipeID.REQ).bits.taskMes.pipeID === PipeID.REQ)
  mshrCtl.io.updMSHR        <> fastPriorityArbDec(Seq(respPipe.io.updMSHR, reqPipe.io.updMSHR))
  mshrCtl.io.updLockMSHR(0) <> respPipe.io.updLockMSHR
  mshrCtl.io.updLockMSHR(1) <> reqPipe.io.updLockMSHR

  respPipe.io.bank          := io.bank
  respPipe.io.incoID        := io.incoID
  reqPipe.io.bank           := io.bank
  reqPipe.io.incoID         := io.incoID


  mpReqQueue.io.enq         <> fastPriorityArbDec(Seq(respPipe.io.req2Intf, reqPipe.io.req2Intf))
  mpRespQueue.io.enq        <> fastPriorityArbDec(Seq(respPipe.io.resp2Intf, reqPipe.io.resp2Intf))
  io.req2Intf               <> mpReqQueue.io.deq
  io.resp2Intf              <> mpRespQueue.io.deq
  io.dbRCReq                <> fastPriorityArbDec(Seq(respPipe.io.dbRCReq, reqPipe.io.dbRCReq))

// --------------------------- Assertion ---------------------------//
  assert(io.req2Exu.bits.pcuIndex.bankID      === io.bank   | !io.req2Exu.valid)
  assert(io.resp2Intf.bits.pcuIndex.bankID    === io.bank   | !io.resp2Intf.valid)
  assert(io.req2Intf.bits.pcuIndex.bankID     === io.bank   | !io.req2Intf.valid)

  assert(io.req2Exu.bits.pcuIndex.to.incoID   === io.incoID | !io.req2Exu.valid)
  assert(io.resp2Intf.bits.pcuIndex.to.incoID === io.incoID | !io.resp2Intf.valid)
  assert(io.req2Intf.bits.pcuIndex.to.incoID  === io.incoID | !io.req2Intf.valid)
}