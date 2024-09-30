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

// class RXRSP(lcreditNum: Int = 4, aggregateIO: Boolean = false) extends Module {
//     val io = IO(new Bundle {
//         val linkState = Input(UInt(LinkState.width.W))
//         val id        = Flipped(DecoupledIO(new IDBundle(CHIBundleParameters())))
//         val out       = CHIChannelIO(new CHIBundleRSP(CHIBundleParameters()), false)
//     })

//     io.out <> DontCare

//     require(lcreditNum <= 15)

//     val lcreditsWidth = log2Up(lcreditNum) + 1
//     val creditReceive = RegInit(0.U(lcreditsWidth.W))
//     val linkState     = io.linkState

//     val enableLCredit = linkState === LinkState.RUN
//     val crdRetrun     = linkState === LinkState.DEACTIVATE

//     val flit = WireInit(0.U.asTypeOf(new CHIBundleRSP(CHIBundleParameters())))
//     io.out.flit := flit

//     // -----------------------------------------------------------------------------------------
//     //  credit control
//     // -----------------------------------------------------------------------------------------
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

//     val bits = io.id.bits
//     val id   = bits.transID

    
//     // -----------------------------------------------------------------------------------------
//     //  Send rsp
//     // -----------------------------------------------------------------------------------------
//     io.out.flitv := false.B

//     when(crdRetrun && creditReceive =/= 0.U) {
//         io.out.flitv := true.B
//         flit.txnID   := 0.U
//         flit.tgtID   := Params.TgtID
//         flit.opcode  := CHIOpcodeRSP.RespLCrdReturn
//     }.elsewhen(io.id.valid && enableLCredit) {

//         flit.txnID := id.txnID
//         flit.tgtID := id.tgtID
//         flit.dbID  := bits.dbID

//         when(id.opType =/= OpcodeType.None) {
//             io.out.flitv := true.B
//         }

//         when(id.opType === OpcodeType.Read) { // ReadReceipt
//             flit.opcode := CHIOpcodeRSP.ReadReceipt
//         }.elsewhen(id.opType === OpcodeType.Write) { // DBIDrsp
//             flit.opcode := CHIOpcodeRSP.DBIDResp
//         }.elsewhen(id.opType === OpcodeType.Comp) { // comp
//             flit.opcode := CHIOpcodeRSP.Comp
//         }
//     }

//     io.id.ready := creditReceive =/= 0.U

// }


