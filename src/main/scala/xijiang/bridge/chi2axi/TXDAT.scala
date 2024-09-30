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


// class TXDAT(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         val linkState      = Input(UInt(LinkState.width.W))
//         val in             = Flipped(CHIChannelIO(new CHIBundleDAT(CHIBundleParameters()), false))
//         val awOrder        = Flipped(DecoupledIO(new Order(lcreditNum)))
//         val w              = Irrevocable(new AXI4BundleW(MyAXI4Params()))
//         val reclaimLCredit = Output(Bool())
//     })

//     io.w <> DontCare
//     io.awOrder <> DontCare

//     // -----------------------------------------------------------------------------------------
//     // credit control
//     // -----------------------------------------------------------------------------------------
//     require(lcreditNum <= 15)

//     val flit = Wire(new CHIBundleDAT(CHIBundleParameters()))
//     flit := io.in.flit

//     val lcreditsWidth = log2Up(lcreditNum) + 1
//     val creditSend    = RegInit(0.U(lcreditsWidth.W))
//     val creditRemain  = RegInit(lcreditNum.U(lcreditsWidth.W))
//     assert(creditSend + creditRemain === lcreditNum.U)

//     val valid     = io.in.flitv
//     val linkState = io.linkState

//     val ready         = creditSend =/= 0.U
//     val deact         = flit.opcode === CHIOpcodeREQ.ReqLCrdReturn
//     val accept        = ready && io.in.flitv && RegNext(io.in.flitpend)
//     val enableLCredit = linkState === LinkState.RUN
//     val crdRetrun     = linkState === LinkState.DEACTIVATE

//     // credit control
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


//     // -----------------------------------------------------------------------------------------
//     // write order:
//     // store aw transaction ID in fifo to make sure w in right order
//     // -----------------------------------------------------------------------------------------
//     val queue      = Module(new Queue(UInt(log2Ceil(lcreditNum).W), entries = lcreditNum, pipe = true, flow = false))
//     val dataMemory = RegInit(VecInit(Seq.fill(lcreditNum)(0.U.asTypeOf(new WriteStoreBundle()))))

//     val order = io.awOrder.bits.order

//     queue.io.enq.bits  := order
//     queue.io.enq.valid := io.awOrder.valid
//     io.awOrder.ready   := queue.io.enq.ready

//     when(io.awOrder.valid){
//         dataMemory(order).packageNum := io.awOrder.bits.packageNum
//         dataMemory(order).burstLen   := io.awOrder.bits.burstLen 
//         dataMemory(order).busy       := true.B
//     }


//     // -----------------------------------------------------------------------------------------
//     // receive data 
//     // -----------------------------------------------------------------------------------------
//     val index = log2Ceil(lcreditNum) - 1
//     val deq   = queue.io.deq
//     val w     = io.w.bits
//     val id    = flit.txnID(index, 0)

//     deq.ready := false.B

//     val offset       = 2 - log2Ceil(512/Params.chiDataBits)
//     val dataIDoffset = flit.dataID >> offset 

//     when(enableLCredit && accept) {
//            val equal = dataMemory(id).packageCnt === dataMemory(id).packageNum

//            dataMemory(id).data(dataIDoffset)    := flit.data 
//            dataMemory(id).strb(dataIDoffset)    := flit.be 
//            dataMemory(id).packageCnt            := dataMemory(id).packageCnt + 1.U
//            dataMemory(id).full                  := dataMemory(id).busy & equal   
//     }


//     // -----------------------------------------------------------------------------------------
//     // send data
//     // Not allow to send before full, but RXDAT module could
//     // -----------------------------------------------------------------------------------------
//     val axiDataBits = MyAXI4Params().dataBits
//     val axiDataByte = axiDataBits / 8
//     val axiMaxBurst = Params.axiMaxBurst
//     val cntWidth    = log2Ceil(axiMaxBurst)

//     val data = Wire(Vec(axiMaxBurst, UInt(axiDataBits.W)))
//     val strb = Wire(Vec(axiMaxBurst, UInt(axiDataByte.W)))

//     val dataCombine = dataMemory(deq.bits).data.asUInt
//     val strbCombine = dataMemory(deq.bits).strb.asUInt

//     for (i <- 0 until data.size) {
//         data(i) := dataCombine(axiDataBits * (i + 1) - 1, axiDataBits * i)
//         strb(i) := strbCombine(axiDataByte * (i + 1) - 1, axiDataByte * i)
//     }

//     val cnt  = RegInit(0.U(cntWidth.W))
    
//     val last = cnt === dataMemory(deq.bits).burstLen

//     val sendDate = data(cnt)
//     val sendStrb = strb(cnt)

//     io.w.valid            := false.B
//     w.last                := false.B

//     when(deq.valid) {
//         when(dataMemory(deq.bits).full && io.w.ready) {
//             io.w.valid                := true.B
//             w.data                    := sendDate // memoryDate(deq.bits)
//             w.strb                    := sendStrb // memoryStrb(deq.bits)
//             cnt                       := Mux(last, 0.U, cnt + 1.U)
//             w.last                    := last
//             deq.ready                 := last
//             dataMemory(deq.bits).full := Mux(w.last, false.B, dataMemory(deq.bits).full)
//             dataMemory(deq.bits).busy := Mux(w.last, false.B, dataMemory(deq.bits).busy)
//         }
//     }

// }


