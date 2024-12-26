package zhujiang.device.dma

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.{Node, NodeType}
import xijiang.router.base.DeviceIcnBundle
import zhujiang.ZJModule
import zhujiang.axi._
import zhujiang.chi._
import firrtl.options.DoNotTerminateOnExit

case class DmaParams(
  chiEntrySize: Int = 16,
  idBits: Int = 12,
  axiEntrySize: Int = 4,
  bufferSize: Int = 16,
  nrBeats: Int = 2,
)

class Axi2Chi(node: Node)(implicit p: Parameters) extends ZJModule {
  require(node.nodeType == NodeType.RI)
  private val dmaParams = zjParams.dmaParams
  private val axiParams = AxiParams(dataBits = dw, addrBits = raw, idBits = dmaParams.idBits)
  val axi = IO(Flipped(new AxiBundle(axiParams)))
  val icn = IO(new DeviceIcnBundle(node))

  axi.aw := DontCare
  axi.w := DontCare
  axi.b := DontCare
  icn := DontCare

  // //SubModule
  private val axiE = Module(new AxiREntrys)
  private val chiE = Module(new ChiREntrys)
  private val rdDB = Module(new DataBufferForRead(axiParams, dmaParams.bufferSize, dmaParams.chiEntrySize))

  // //Connect logic
  axi.ar <> axiE.io.ar
  axiE.io.alloc <> chiE.io.alloc

  chiE.io.reqDB <> rdDB.io.alloc
  chiE.io.chiReq <> icn.tx.req.get
  chiE.io.chiRsp <> icn.rx.resp.get
  chiE.io.chiDat <> icn.rx.data.get
  chiE.io.wrDB   <> rdDB.io.wrDB
  chiE.io.rdDB   <> rdDB.io.rdDB
  chiE.io.respDB <> rdDB.io.allocRsp
  
  axi.r <> rdDB.io.axiR
  
}
