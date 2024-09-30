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

// object OpcodeType {
//     val width = 2

//     val Comp  = "b11".U(width.W)
//     val Write = "b10".U(width.W)
//     val Read  = "b01".U(width.W)
//     val None  = "b00".U(width.W)
// }

// class State extends Bundle {
//     val w = Bool()
//     val r = Bool()
// }

// class Order(lcreditNum :Int = 4) extends Bundle{
//     val order      = UInt(log2Ceil(lcreditNum).W)
//     val packageNum = UInt(2.W)
//     val burstLen   = UInt((Params.cntWidth).W)
// }

// class ReadStore extends Bundle {
//     val num = 512/Params.axiDataBits

//     val data  = Vec(num, UInt(Params.axiDataBits.W))
//     val state = Vec(num, Bool())

//     val packageCnt = UInt(2.W)
//     val packageNum = UInt(2.W)
//     val burstCnt   = UInt(Params.cntWidth.W)
//     //val burstLen   = UInt(Params.cntWidth.W)
    
//     val busy = Bool()
//     val full = Bool()
    
// }

// class WriteStoreBundle() extends Bundle {
//     val num = 512/Params.chiDataBits

//     val data = Vec(num, UInt(Params.chiDataBits.W)) 
//     val strb = Vec(num, UInt((Params.chiDataBits/8).W))

//     val packageCnt = UInt(2.W)
//     val packageNum = UInt(2.W)
//     val burstLen   = UInt(Params.cntWidth.W)

//     val busy       = Bool()
//     val full       = Bool()
// }

// class IDBundle(params: CHIBundleParameters) extends Bundle {
//     val dbID    = UInt(12.W)
//     val transID = new TransIDBundle(params)
// }

// class TransIDBundle(params: CHIBundleParameters) extends Bundle {
//     val opType = UInt(2.W)
//     val tgtID  = UInt(params.nodeIdBits.W) // TODO: not use?
//     val txnID  = UInt(12.W)
// }

// class TansAddrBundle(params: AXI4BundleParameters) extends Bundle {
//     val opType = UInt(2.W)
//     val addr   = UInt(params.addrBits.W)
//     val size   = UInt(params.sizeBits.W) // TODO: not use?
// }

// class MapBundle(params: CHIBundleParameters) extends Bundle {
//     val busy  = Bool()
//     val tgtID = UInt(params.nodeIdBits.W) // TODO: not use?
//     val txnID = UInt(12.W)
// }

// object Params {
//     val width = 7
//     val SrcID = 0.U(width.W)
//     val TgtID = 1.U(width.W)

//     val chiDataBits = CHIBundleParameters().dataBits
//     val axiDataBits = MyAXI4Params().dataBits
    
//     val axiMaxSize      = log2Ceil(axiDataBits/8)
//     val chiMaxSize      = log2Ceil(chiDataBits/8)
//     val axiMaxBurst     = 512 / axiDataBits
//     val chiMaxPackage   = 512 / chiDataBits
//     val cntWidth        = log2Ceil(axiMaxBurst) + 1
// }

