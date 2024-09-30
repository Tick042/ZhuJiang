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

// class AxAddrTrans(implicit p: Parameters) extends Module {
//     val io = IO(new Bundle {
//         val addr2axi = Flipped(DecoupledIO(new TansAddrBundle(AXI4Params())))
//         val axID     = Input(UInt(log2Ceil(lcreditNum).W))
//         val axiState = Output(new State()) // Reflects whether the axaddr channel is available
//         val ar       = Irrevocable(new AXI4BundleAR(AXI4Params()))
//         val aw       = Irrevocable(new AXI4BundleAW(AXI4Params()))
//         val awOrder  = DecoupledIO(new Order(lcreditNum))
//         val arOrder  = DecoupledIO(new Order(lcreditNum))
//     })

//     io.ar <> DontCare
//     io.aw <> DontCare
//     io.awOrder <> DontCare
    
//     val ar = io.ar
//     val aw = io.aw

//     ar.valid     := false.B
//     aw.valid     := false.B

//     val chiSize    = io.addr2axi.bits.size
//     val axiMaxSize = 5.U//Params.axiMaxSize.asUInt
//     val chiMaxSize = Params.chiMaxSize.asUInt

//     // -----------------------------------------------------------------------------------------
//     // chi Size : transaction data, not Bus Bitwidth
//     // axi size: active Bus Bitwidth
//     // -----------------------------------------------------------------------------------------
//     val size = Mux(chiSize <= axiMaxSize, chiSize, axiMaxSize) 

//     when(io.addr2axi.fire) {
//         when(io.addr2axi.bits.opType === OpcodeType.Read) {
//             ar.valid     := true.B // valid depends on ready,ready can't wait for valid
//             ar.bits.id   := io.axID
//             ar.bits.addr := io.addr2axi.bits.addr
//             ar.bits.size := size

//         }.elsewhen(io.addr2axi.bits.opType === OpcodeType.Write) {
//             aw.valid     := true.B
//             aw.bits.id   := io.axID
//             aw.bits.addr := io.addr2axi.bits.addr
//             aw.bits.size := size
//         }
//     }

//     val burstLen   = Wire(UInt(8.W))
//     val packageNum = Wire(UInt(2.W))

//     val test  = WireInit(0.B)
//     val test1  = WireInit(0.B)
//     val test2  = WireInit(0.B)

//     burstLen := 0.U

//     // axi burstLen needed in one transaction
//     // -----------------------------------------------------------------------------------------
//     // chi max size: 3'b110(6); axi size support : 0-7
//     // only support axi Bus Bitwidth >= 128bit(size == 4)
//     // -----------------------------------------------------------------------------------------
//     when(chiSize <= axiMaxSize){
//         burstLen := 0.U  
//     }.elsewhen(chiSize - 1.U === axiMaxSize){
//         burstLen := 1.U
//     }.elsewhen(chiSize - 2.U === axiMaxSize){
//         burstLen := 3.U 
//     } //else......(if your axi Bus Bitwidth < 128)

//     packageNum := 0.U


//     // chi package number needed in one transaction
//     // -----------------------------------------------------------------------------------------
//     // chi Bus Bitwidth: 128, 256, 512 ony
//     // -----------------------------------------------------------------------------------------
//     when(chiSize <= chiMaxSize){
//         packageNum := 0.U
//         test  := true.B
//     }.elsewhen(chiSize - 1.U === chiMaxSize){
//         packageNum := 1.U
//         test1  := true.B
//     }.elsewhen(chiSize - 2.U === chiMaxSize){
//         packageNum := 3.U  
//         test2  := true.B     
//     }
    
//     aw.bits.len := burstLen
//     ar.bits.len := burstLen

//     aw.bits.burst := "b01".U // always INCR
//     ar.bits.burst := "b01".U

//     io.addr2axi.ready := aw.ready || ar.ready
//     io.axiState.r     := ar.ready // ar channel available 
//     io.axiState.w     := aw.ready // aw channel available

//     io.awOrder.bits.order       := io.axID
//     io.awOrder.bits.packageNum  := packageNum
//     io.awOrder.bits.burstLen    := burstLen
//     io.awOrder.valid            := aw.fire

//     io.arOrder.bits.order       := io.axID
//     io.arOrder.bits.packageNum  := packageNum
//     io.arOrder.bits.burstLen    := burstLen
//     io.arOrder.valid            := ar.fire

// }


