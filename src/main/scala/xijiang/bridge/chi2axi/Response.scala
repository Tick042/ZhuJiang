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

// class Response(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         // rsp
//         val idFromR   = Flipped(DecoupledIO(new IDBundle(CHIBundleParameters())))
//         val idFromB   = Flipped(DecoupledIO(new IDBundle(CHIBundleParameters())))
//         val id2Rsp    = DecoupledIO(new IDBundle(CHIBundleParameters()))
//         val mapFreeWr = DecoupledIO(UInt(log2Ceil(lcreditNum).W))
//     })

//     io.mapFreeWr <> DontCare
//     io.id2Rsp    <> DontCare

//     io.mapFreeWr.valid := false.B

//     //select which to rsp
//     when(io.idFromB.fire) {
//         io.id2Rsp          <> io.idFromB
//         io.mapFreeWr.valid := true.B
//         io.mapFreeWr.bits  := io.idFromB.bits.dbID
//     }.otherwise {
//         io.id2Rsp <> io.idFromR
//     }

//     val reqNotUse =  (io.idFromR.valid && (io.idFromR.bits.transID.opType === OpcodeType.None)) || !io.idFromR.valid    

//     io.idFromR.ready := io.id2Rsp.ready
//     io.idFromB.ready := io.id2Rsp.ready && reqNotUse
   

// }

