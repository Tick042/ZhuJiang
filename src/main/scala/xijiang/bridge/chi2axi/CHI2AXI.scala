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

// class CHI2AXI(lcreditNum: Int = 4) extends Module {
//     val io = IO(new Bundle {
//         val chi         = new CHIBundleUpstream(CHIBundleParameters())
//         val chiLinkCtrl = Flipped(new CHILinkCtrlIO)
//         val axi         = new AXI4Bundle(MyAXI4Params()) // new AXI4Bundle(AXI4BundleParameters(addrBits = 48, dataBits = 256, idBits = 8))
//     })

//     io.chi.rxsnp := DontCare
//     io.chi.txrsp := DontCare

//     // -----------------------------------------------------------------------------------------
//     //  chiLinkCtrl
//     // -----------------------------------------------------------------------------------------
//     io.chiLinkCtrl.rxsactive := io.chiLinkCtrl.txsactive

//     val txState = RegInit(LinkState.STOP)
//     val rxState = RegInit(LinkState.STOP)

//     val txreqDeact, txdatDeact, txrspDeact = WireInit(false.B)
//     val txDeact                            = txreqDeact && txdatDeact && txrspDeact //All tx channel credits are back 

//     io.chiLinkCtrl.txactiveack := RegNext(io.chiLinkCtrl.txactivereq) || !txDeact

//     txState := MuxLookup(Cat(io.chiLinkCtrl.txactivereq, io.chiLinkCtrl.txactiveack), LinkState.STOP)(
//         Seq(
//             Cat(true.B, false.B)  -> LinkState.ACTIVATE,
//             Cat(true.B, true.B)   -> LinkState.RUN,
//             Cat(false.B, true.B)  -> LinkState.DEACTIVATE,
//             Cat(false.B, false.B) -> LinkState.STOP
//         )
//     )

//     val queueEmpty, mapEmpty = WireInit(false.B)
//     io.chiLinkCtrl.rxactivereq := (txState =/= LinkState.STOP) || !queueEmpty || !mapEmpty  //Empty: No trnsaction need to handle

//     rxState := MuxLookup(Cat(io.chiLinkCtrl.rxactivereq, io.chiLinkCtrl.rxactiveack), LinkState.STOP)(
//         Seq(
//             Cat(true.B, false.B)  -> LinkState.ACTIVATE,
//             Cat(true.B, true.B)   -> LinkState.RUN,
//             Cat(false.B, true.B)  -> LinkState.DEACTIVATE,
//             Cat(false.B, false.B) -> LinkState.STOP
//         )
//     )

//     // -----------------------------------------------------------------------------------------
//     //  modules
//     // -----------------------------------------------------------------------------------------
//     val txreq    = Module(new TXREQ(lcreditNum))
//     val txdat    = Module(new TXDAT(lcreditNum))
//     val rxdat    = Module(new RXDAT(lcreditNum))
//     val rxrsp    = Module(new RXRSP(lcreditNum))
//     val axAddr   = Module(new AxAddrTrans(lcreditNum))
//     val map      = Module(new IdMap(lcreditNum))
//     val response = Module(new Response(lcreditNum))
//     val b        = Module(new AXI4ChannelB(lcreditNum))

//     txreqDeact := txreq.io.reclaimLCredit
//     txdatDeact := txdat.io.reclaimLCredit
//     txrspDeact := true.B
//     queueEmpty := txreq.io.queueEmpty
//     mapEmpty   := map.io.mapEmpty

//     // txreq module
//     txreq.io.in        <> io.chi.txreq
//     txreq.io.linkState := txState
//     txreq.io.axiState  := axAddr.io.axiState
//     txreq.io.mapState  := map.io.mapState
//     txreq.io.id2Map    <> map.io.id2Map
//     txreq.io.addr2axi  <> axAddr.io.addr2axi
//     // txreq.io.id2Rsp    <> response.io.idFromR

//     // txdat module
//     txdat.io.in        <> io.chi.txdat
//     txdat.io.linkState := txState
//     txdat.io.awOrder   <> axAddr.io.awOrder
//     txdat.io.w         <> io.axi.w

//     // rxdat module
//     rxdat.io.linkState := rxState
//     rxdat.io.r         <> io.axi.r
//     rxdat.io.arID      <> map.io.arID
//     rxdat.io.arTxId    <> map.io.arTxId
//     rxdat.io.mapFreeRd <> map.io.mapFreeRd
//     rxdat.io.arOrder   <> axAddr.io.arOrder
//     io.chi.rxdat       <> rxdat.io.out

//     // rxrsp module
//     rxrsp.io.linkState := rxState
//     rxrsp.io.id        <> response.io.id2Rsp
//     rxrsp.io.out       <> io.chi.rxrsp

//     // axaddr module
//     axAddr.io.axID := map.io.axID
//     axAddr.io.ar   <> io.axi.ar
//     axAddr.io.aw   <> io.axi.aw

//     // b module
//     b.io.b      <> io.axi.b
//     b.io.id2Rsp <> response.io.idFromB
//     b.io.bID    <> map.io.bID
//     b.io.bTxId  <> map.io.bTxId

//     // response module
//     response.io.mapFreeWr <> map.io.mapFreeWr

//     val interconnect = response.io.idFromR
//     val id           = interconnect.bits

//     id.transID         <> txreq.io.id2Rsp.bits
//     id.dbID            <> map.io.axID
//     interconnect.valid <> txreq.io.id2Rsp.valid
//     interconnect.ready <> txreq.io.id2Rsp.ready
// }

// object CHI2AXI extends App {
//     GenerateVerilog(args, () => new CHI2AXI(), name = "CHI2AXI", split = false)
// }
