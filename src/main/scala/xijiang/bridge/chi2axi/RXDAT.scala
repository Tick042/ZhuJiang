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


// class RXDAT(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         val linkState = Input(UInt(LinkState.width.W))
//         val r         = Flipped(Irrevocable(new AXI4BundleR(MyAXI4Params())))
//         val out       = CHIChannelIO(new CHIBundleDAT(CHIBundleParameters()), false)
//         val arID      = DecoupledIO(UInt(log2Ceil(lcreditNum).W))
//         val arTxId    = Flipped(DecoupledIO(new MapBundle(CHIBundleParameters())))
//         val mapFreeRd = DecoupledIO(UInt(log2Ceil(lcreditNum).W))
//         val arOrder   = Flipped(DecoupledIO(new Order(lcreditNum)))
//     })

//     // dontTouch(io.out)
//     io.out              <> DontCare
//     io.arID             <> DontCare
//     io.arTxId.bits.busy := DontCare
//     // -----------------------------------------------------------------------------------------
//     // credit control
//     // -----------------------------------------------------------------------------------------

//     require(lcreditNum <= 15)

//     val lcreditsWidth = log2Up(lcreditNum) + 1
//     val creditReceive = RegInit(0.U(lcreditsWidth.W))
//     val linkState     = io.linkState

//     val enableLCredit = linkState === LinkState.RUN
//     val crdRetrun     = linkState === LinkState.DEACTIVATE
//     val allowSend     = (enableLCredit && creditReceive =/= 0.U)

//     val flit = WireInit(0.U.asTypeOf(new CHIBundleDAT(CHIBundleParameters())))
//     io.out.flit := flit

//     // credit control
//     when(crdRetrun) {
//         when(creditReceive =/= 0.U && io.out.flitv) { // credit return
//             creditReceive := 0.U
//         }
//     }.elsewhen(enableLCredit) { // allow to send rsp
//         when(io.out.flitv && !io.out.lcrdv) {
//             creditReceive := creditReceive - 1.U
//         }.elsewhen(!io.out.flitv && io.out.lcrdv) {
//             creditReceive := creditReceive + 1.U
//         }
//     }

//     flit.srcID      := Params.SrcID
//     io.out.flitpend := linkState === LinkState.RUN

//     // -----------------------------------------------------------------------------------------
//     // data buffer
//     // -----------------------------------------------------------------------------------------
//     val dataMemory = RegInit(VecInit(Seq.fill(lcreditNum)(0.U.asTypeOf(new ReadStore()))))

//     // -----------------------------------------------------------------------------------------
//     // order : a index
//     // packageNum: this transaction need package Num
//     // burstLen: need Burst Num  =>  NOT USE, io.r.last instead
//     // -----------------------------------------------------------------------------------------
//     val order = io.arOrder.bits.order

//     when(io.arOrder.valid){
//         dataMemory(order).packageNum := io.arOrder.bits.packageNum
//         //dataMemory(order).burstLen   := io.arOrder.bits.burstLen 
//         dataMemory(order).busy       := true.B     
//     }

//     io.arOrder.ready := dataMemory.map(!_.busy).reduce(_ || _)    

//     // -----------------------------------------------------------------------------------------
//     // receive data 
//     // -----------------------------------------------------------------------------------------
//     val index = log2Ceil(lcreditNum) - 1
//     val id    = io.r.bits.id(index, 0)
//     val last  = io.r.bits.last

//     when(io.r.fire) {
//         val burstCnt  = dataMemory(id).burstCnt
//         val full      = dataMemory(id).full
//         val data      = dataMemory(id).data(burstCnt)
//         val state     = dataMemory(id).state(burstCnt)

//         burstCnt  := Mux(!last, burstCnt + 1.U, 0.U)
//         data      := io.r.bits.data
//         full      := last   // full => NOT USE: Convenient for testing only
//         state     := true.B
//     }

//     // -----------------------------------------------------------------------------------------
//     // monitor dataMemory.state to decide which dataline could send chi pacakage
//     // allow to send before r.last falg
//     // -----------------------------------------------------------------------------------------
//     val axiDataBits = MyAXI4Params().dataBits
//     val chiDataBits = CHIBundleParameters().dataBits

//     val needSend = Wire(Vec(lcreditNum, Bool()))
//     val data     = Wire(Vec(Params.chiMaxPackage, UInt(chiDataBits.W))) //chiMaxPackage is the max number of chi package

//     val num        = chiDataBits/axiDataBits //The number of bursts needed to compose a package
//     val stateIndex = if(num == 0) 1 else num 

//     for(i <- 0 until lcreditNum){
//         val packageCnt = dataMemory(i).packageCnt
//         val stateNum   = dataMemory(i).state.size/stateIndex
//         val state      = dataMemory(i).state.asUInt
//         val stateSplit = Wire(Vec(stateNum, UInt(stateIndex.W)))
//         for(j <- 0 until stateNum){
//             stateSplit(j) := state(stateIndex * (j + 1) - 1, stateIndex * j)
//         }
//         needSend(i) := ~stateSplit(packageCnt >> log2Ceil(axiDataBits/chiDataBits).U) === 0.U // stateSplit all 1'b1: collect enough burst for a package,">>" to match (chiDataWidth < axiDataWidth) 
//     }

//     val dataIndexValid  = needSend.reduce(_ || _)  //needSend not all zero
//     val dataIndex       = PriorityEncoder(needSend)  //which dataMemory need to send

//     val sendMomery    = dataMemory(dataIndex)
//     val dataCombine   = sendMomery.data.asUInt

//     for (i <- 0 until data.size) {
//         data(i) := dataCombine(chiDataBits * (i + 1) - 1, chiDataBits * i)
//     } 

//     // -----------------------------------------------------------------------------------------
//     // send data
//     // -----------------------------------------------------------------------------------------
//     io.out.flitv := false.B

//     val axiDataByte  = MyAXI4Params().dataBits / 8
//     val strb         = Fill(stateIndex, (1.U << axiDataByte) - 1.U)
//     val lastPackage  = sendMomery.packageCnt === sendMomery.packageNum

//     when(crdRetrun && creditReceive =/= 0.U) { // credit return
//         io.out.flitv := true.B
//         flit.tgtID   := Params.TgtID
//         flit.txnID   := 0.U
//         flit.opcode  := CHIOpcodeDAT.DataLCrdReturn
//     }.elsewhen(io.arTxId.fire) { // retrun data
//         io.out.flitv := true.B
//         flit.tgtID   := io.arTxId.bits.tgtID
//         flit.txnID   := io.arTxId.bits.txnID
//         flit.data    := data(sendMomery.packageCnt)
//         flit.dataID  := sendMomery.packageCnt
//         flit.be      := strb
//         flit.opcode  := CHIOpcodeDAT.CompData
 
//         sendMomery.state      := Mux(lastPackage, VecInit(Seq.fill(sendMomery.state.size)(false.B)), sendMomery.state)
//         sendMomery.busy       := Mux(lastPackage, false.B, sendMomery.busy)
//         sendMomery.full       := Mux(lastPackage, false.B, sendMomery.full)
//         sendMomery.packageCnt := Mux(lastPackage, 0.U, sendMomery.packageCnt + 1.U)
//     }

//     val cntTest = RegInit(0.U(2.W))
//     cntTest := RegNext(cntTest + 1.U)

//     io.arTxId.ready := dataIndexValid && allowSend
//     io.r.ready      := cntTest === 2.U//dataMemory.map(!_.full).reduce(_ || _) 

//     io.arID.valid := dataIndexValid // fetch id from map
//     io.arID.bits  := dataIndex

//     io.mapFreeRd.bits  := Mux(io.arTxId.fire, dataIndex, 0.U) // free id in map
//     io.mapFreeRd.valid := io.arTxId.fire && (lastPackage)

// }


