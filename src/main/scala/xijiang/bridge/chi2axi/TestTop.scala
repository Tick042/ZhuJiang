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

// class TestTop(lcreditNum: Int = 4)(implicit p: Parameters) extends L2Module {
//     val io = IO(new Bundle {
//         val chi = Flipped(new CHIBundleDecoupled(CHIBundleParameters()))
//         val axi = new AXI4Bundle(AXI4Params()) // new AXI4Bundle(AXI4BundleParameters(addrBits = 48, dataBits = 256, idBits = 8))
//     })

//     val linkMonitor = Module(new LinkMonitor())
//     val chi2axi     = Module(new CHI2AXI(lcreditNum))

//     linkMonitor.io.nodeID := io.chi.txreq.bits.srcID

//     linkMonitor.io.in.chi          <> io.chi
//     linkMonitor.io.out.chi         <> chi2axi.io.chi
//     linkMonitor.io.out.chiLinkCtrl <> chi2axi.io.chiLinkCtrl
//     chi2axi.io.axi                 <> io.axi

// }

