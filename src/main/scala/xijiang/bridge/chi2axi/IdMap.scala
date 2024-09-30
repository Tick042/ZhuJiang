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

// class IdMap(lcreditNum: Int = 4) extends Module {
//     val io = IO(new Bundle {
//         val id2Map    = Flipped(DecoupledIO(new TransIDBundle(CHIBundleParameters())))
//         val mapState  = Output(new State())                                // Reflects whether the map is available
//         val mapFreeWr = Flipped(DecoupledIO(UInt(log2Ceil(lcreditNum).W))) // Resource release
//         val mapFreeRd = Flipped(DecoupledIO(UInt(log2Ceil(lcreditNum).W)))
//         val axID      = Output(UInt(log2Ceil(lcreditNum).W))               // to RSP and axAddrTrans, Use the index as axi ID,also DBID
//         val mapEmpty  = Output(Bool())
//         val arID      = Flipped(DecoupledIO(UInt(log2Ceil(lcreditNum).W))) // index for fetch txID,rxdat channel
//         val arTxId    = DecoupledIO(new MapBundle(CHIBundleParameters()))
//         val bID       = Flipped(DecoupledIO(UInt(log2Ceil(lcreditNum).W))) // index for fetch txID,axi b channel
//         val bTxId     = DecoupledIO(new MapBundle(CHIBundleParameters()))

//     })

//     val write = RegInit(VecInit(Seq.fill(lcreditNum)(0.U.asTypeOf(new MapBundle(CHIBundleParameters())))))
//     val read  = RegInit(VecInit(Seq.fill(lcreditNum)(0.U.asTypeOf(new MapBundle(CHIBundleParameters())))))

//     val writeAvailable = PriorityEncoder(VecInit(write.map(!_.busy))) // available index
//     val readAvailable  = PriorityEncoder(VecInit(read.map(!_.busy)))

//     io.axID := DontCare

//     // -----------------------------------------------------------------------------------------
//     // store ID
//     // -----------------------------------------------------------------------------------------
//     when(io.id2Map.fire) {
//         when(io.id2Map.bits.opType === "b01".U) { 
//             io.axID                   := readAvailable  //giving index as axi ID
//             read(readAvailable).busy  := true.B
//             read(readAvailable).tgtID := io.id2Map.bits.tgtID
//             read(readAvailable).txnID := io.id2Map.bits.txnID
//         }.elsewhen(io.id2Map.bits.opType === "b10".U) {
//             io.axID                     := writeAvailable ////giving index as axi ID, also DBID for rsp
//             write(writeAvailable).busy  := true.B
//             write(writeAvailable).tgtID := io.id2Map.bits.tgtID
//             write(writeAvailable).txnID := io.id2Map.bits.txnID
//         }
//     }

//     io.mapState.r := read.map(!_.busy).reduce(_ || _)  // read availabe
//     io.mapState.w := write.map(!_.busy).reduce(_ || _) // write availabe

//     io.id2Map.ready := io.mapState.r || io.mapState.w
//     io.mapEmpty     := !((read.map(_.busy).reduce(_ || _) || write.map(_.busy).reduce(_ || _)))

//     // -----------------------------------------------------------------------------------------
//     // fetch
//     // -----------------------------------------------------------------------------------------
//     val arID = io.arID.bits
//     val bID  = io.bID.bits

//     io.arID.ready := true.B
//     io.bID.ready  := true.B

//     io.arTxId.bits  := read(arID)
//     io.arTxId.valid := io.arID.valid //RegNext(io.arID.valid)
//     io.bTxId.bits   := write(bID)
//     io.bTxId.valid  := io.bID.valid

//     // -----------------------------------------------------------------------------------------
//     // free resource
//     // -----------------------------------------------------------------------------------------
//     when(io.mapFreeRd.fire) {
//         read(io.mapFreeRd.bits).busy  := false.B
//     }

//     when(io.mapFreeWr.fire) {
//         write(io.mapFreeWr.bits).busy := false.B
//     }

//     io.mapFreeWr.ready := true.B
//     io.mapFreeRd.ready := true.B
// }

