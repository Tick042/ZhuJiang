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

// class TXREQ(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         val in             = Flipped(CHIChannelIO(new CHIBundleREQ(CHIBundleParameters()), false))
//         val linkState      = Input(UInt(LinkState.width.W))
//         val mapState       = Input(new State()) //From IDMap
//         val axiState       = Input(new State()) // From AXAddrTrans
//         val id2Map         = DecoupledIO(new TransIDBundle(CHIBundleParameters()))
//         val id2Rsp         = DecoupledIO(new TransIDBundle(CHIBundleParameters()))
//         val addr2axi       = DecoupledIO(new TansAddrBundle(MyAXI4Params())) //To AXAddrTrans
//         val reclaimLCredit = Output(Bool())
//         val queueEmpty     = Output(Bool())
//     })

//     require(lcreditNum <= 15)

//     val flit = Wire(new CHIBundleREQ(CHIBundleParameters()))
//     flit := io.in.flit

//     // one queue
//     val queue = Module(new Queue(flit.cloneType, entries = lcreditNum, pipe = false, flow = false))

//     val lcreditsWidth = log2Up(lcreditNum) + 1
//     val creditSend    = RegInit(0.U(lcreditsWidth.W))
//     val creditRemain  = RegInit(lcreditNum.U(lcreditsWidth.W))
//     assert(creditSend + creditRemain === lcreditNum.U)

//     val valid     = io.in.flitv
//     val linkState = io.linkState

//     val creditReady   = creditSend =/= 0.U
//     val deact         = flit.opcode === CHIOpcodeREQ.ReqLCrdReturn
//     val accept        = creditReady && io.in.flitv && RegNext(io.in.flitpend)
//     val enableLCredit = linkState === LinkState.RUN
//     val crdRetrun     = linkState === LinkState.DEACTIVATE

//     // -----------------------------------------------------------------------------------------
//     // credit control
//     // -----------------------------------------------------------------------------------------
//     when(crdRetrun) {
//         when(accept && deact) { // credit return
//             creditSend   := 0.U
//             creditRemain := lcreditNum.U
//         }
//     }.elsewhen(enableLCredit) { // receive request
//         when(accept && !io.in.lcrdv) {
//             creditSend   := creditSend - 1.U
//             creditRemain := creditRemain + 1.U
//         }.elsewhen(!accept && io.in.lcrdv) {
//             creditSend   := creditSend + 1.U
//             creditRemain := creditRemain - 1.U
//         }
//     }

//     io.in.lcrdv       := enableLCredit && creditRemain =/= 0.U
//     io.reclaimLCredit := creditSend === 0.U

//     val deq       = queue.io.deq
//     val deqOpcode = deq.bits.opcode

//     queue.io.enq.bits  := flit
//     queue.io.enq.valid := enableLCredit && accept

//     deq.ready := false.B

//     io.id2Map   <> DontCare
//     io.id2Rsp   <> DontCare
//     io.addr2axi <> DontCare

//     val ready = WireInit(0.B)

//     deq.ready := ready

//     val test = WireInit(0.B)

//     // -----------------------------------------------------------------------------------------
//     // request decode
//     // diffrent conditions of execution: see ready
//     // -----------------------------------------------------------------------------------------
//     when(deqOpcode === CHIOpcodeREQ.ReadNoSnp) { // read request
//         when(deq.bits.order === "b01".U) {       // need ReadReceipt resp
//             ready                 := io.mapState.r && io.axiState.r && io.id2Rsp.ready // to rsp:ReadReceipt resp
//             io.id2Rsp.bits.opType := OpcodeType.Read
//             io.id2Rsp.bits.tgtID  := deq.bits.srcID
//             io.id2Rsp.bits.txnID  := deq.bits.txnID
//         }.otherwise {
//             io.id2Rsp.bits.opType := OpcodeType.None //No need to rsp
//             ready                 := io.mapState.r && io.axiState.r
//             test                  := true.B
//         }

//         io.addr2axi.bits.opType := OpcodeType.Read // to axi: read req
//         io.addr2axi.bits.addr   := deq.bits.addr
//         io.addr2axi.bits.size   := deq.bits.size

//         io.id2Map.bits.opType := OpcodeType.Read // to map
//         io.id2Map.bits.tgtID  := deq.bits.returnNID 
//         io.id2Map.bits.txnID  := deq.bits.returnTxnID 
//     }.elsewhen(deqOpcode === CHIOpcodeREQ.WriteNoSnpPtl || deqOpcode === CHIOpcodeREQ.WriteNoSnpFull) { // write req
//         ready                 := io.mapState.w && io.axiState.w && io.id2Rsp.ready
//         io.id2Rsp.bits.opType := OpcodeType.Write // to rsp:DBIDrsp
//         io.id2Rsp.bits.tgtID  := deq.bits.returnNID
//         io.id2Rsp.bits.txnID  := deq.bits.returnTxnID

//         io.addr2axi.bits.opType := OpcodeType.Write // to axi
//         io.addr2axi.bits.addr   := deq.bits.addr
//         io.addr2axi.bits.size   := deq.bits.size

//         io.id2Map.bits.opType := OpcodeType.Write // to map
//         io.id2Map.bits.tgtID  := deq.bits.srcID
//         io.id2Map.bits.txnID  := deq.bits.txnID
//     }.otherwise { /////// Neither read nor write
//         ready := io.id2Rsp.ready

//         io.id2Rsp.bits.opType := OpcodeType.Comp // to rsp:Comp rsp
//         io.id2Rsp.bits.tgtID  := deq.bits.srcID
//         io.id2Rsp.bits.txnID  := deq.bits.txnID

//         io.addr2axi.bits.opType := OpcodeType.None // to axi:invalid
//         io.id2Map.bits.opType   := OpcodeType.None // to map:invalid
//     }

//     val queueHandshake = queue.io.deq.ready && queue.io.deq.valid

//     io.id2Map.valid   := queueHandshake
//     io.id2Rsp.valid   := queueHandshake
//     io.addr2axi.valid := queueHandshake
//     io.queueEmpty     := !deq.valid
// }

