// package xijiang.bridge.chi2axi

// import chisel3._
// import chisel3.util._
// import org.chipsalliance.cde.config._
// import freechips.rocketchip.amba.axi4._
// import org.chipsalliance.cde.config.Parameters
// import xijiang.bridge._
// import zhujiang._
// import zhujiang.chi._
// import xijiang.bridge.Utils.GenerateVerilog
// import _root_.circt.stage.FirtoolOption
// import chisel3.stage.ChiselGeneratorAnnotation
// import _root_.circt.stage._

// class AXI4ChannelB(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         val b      = Flipped(Irrevocable(new AXI4BundleB(AXI4Params())))
//         val id2Rsp = DecoupledIO(new IDBundle(CHIBundleParameters()))
//         val bID    = DecoupledIO(UInt(log2Ceil(lcreditNum).W))
//         val bTxId  = Flipped(DecoupledIO(new MapBundle(CHIBundleParameters())))
//     })

//     io.id2Rsp <> DontCare
//     io.bID    <> DontCare

//     val queue = Module(new Queue(new AXI4BundleB(AXI4Params()), entries = lcreditNum, pipe = true, flow = false))

//     //enqueue
//     queue.io.enq.bits  := io.b.bits
//     queue.io.enq.valid := io.b.valid
//     io.b.ready         := queue.io.enq.ready

//     val deq       = queue.io.deq
//     val deqBits   = deq.bits
//     val allowSend = io.id2Rsp.ready
//     val index     = log2Ceil(lcreditNum) - 1

//     io.bID.valid := false.B

//     //fetch ID from IDMap
//     when(deq.valid && allowSend) {
//         io.bID.valid := true.B
//         io.bID.bits  := deqBits.id(index, 0)
//     }

//     io.bTxId.ready := deq.valid && allowSend
//     deq.ready      := io.bTxId.fire

//     val id   = io.id2Rsp.bits
//     val txID = io.bTxId.bits

//     io.id2Rsp.valid := false.B

//     when(io.bTxId.fire) {
//         io.id2Rsp.valid   := true.B
//         id.dbID           := deqBits.id(index, 0)
//         id.transID.opType := OpcodeType.Comp
//         id.transID.tgtID  := txID.tgtID
//         id.transID.txnID  := txID.txnID
//     }

// }


