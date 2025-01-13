package zhujiang.device.socket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xijiang.Node
import xijiang.c2c.{C2cBundle, C2cPacker, C2cUtils}
import xijiang.router.base.{DeviceIcnBundle, IcnBundle}
import zhujiang.ZJParametersKey

object SocketType {
  val sync:Int = 0
  val async:Int = 1
  val c2c:Int = 2

  def getType(t:String):Int = {
    t.toLowerCase match {
      case "sync" => sync
      case "async" => async
      case "c2c" => c2c
    }
  }

  def testType(tgtStr:String, refStr:String): Boolean = {
    getType(tgtStr) == getType(refStr)
  }
}

trait SocketCommon {
  def connChn[T <: Data, K <: Data](sink:Option[DecoupledIO[T]], src:Option[DecoupledIO[K]]):Unit = {
    if(src.isDefined && sink.isDefined) require(src.get.bits.getWidth == sink.get.bits.getWidth)
    sink.foreach(s => s.valid := src.map(_.valid).getOrElse(false.B))
    sink.foreach(s => s.bits := src.map(_.bits).getOrElse(0.U).asTypeOf(s.bits))
    src.foreach(s => s.ready := sink.map(_.ready).getOrElse(false.B))
  }
  def socketType: String
  def testType(refStr:String): Boolean = SocketType.testType(socketType, refStr)
}

class SocketIcnSideBundle(val node:Node)(implicit p:Parameters) extends Bundle with SocketCommon {
  val socketType = node.socket
  val sync = if(testType("sync")) Some(new IcnBundle(node)) else None
  val async = if(testType("async")) Some(new IcnAsyncBundle(node)) else None
  val c2c = if(testType("c2c")) Some(new C2cBundle) else None
  val resetTx = Output(AsyncReset())

  def <>(that: SocketDevSideBundle):Unit = {
    this.sync.foreach(_ <> that.sync.get)
    this.async.foreach(_ <> that.async.get)
    this.c2c.foreach(_ <=> that.c2c.get)
    that.resetRx := this.resetTx
  }
}

class SocketDevSideBundle(val node:Node)(implicit p:Parameters) extends Bundle with SocketCommon {
  val socketType = node.socket
  val sync = if(testType("sync")) Some(new DeviceIcnBundle(node)) else None
  val async = if(testType("async")) Some(new DeviceIcnAsyncBundle(node)) else None
  val c2c = if(testType("c2c")) Some(new C2cBundle) else None
  val resetRx = Input(AsyncReset())

  def <>(that: SocketIcnSideBundle):Unit = {
    this.sync.foreach(_ <> that.sync.get)
    this.async.foreach(_ <> that.async.get)
    this.c2c.foreach(_ <=> that.c2c.get)
    this.resetRx := that.resetTx
  }
}

class SocketChipBundle(implicit p:Parameters) extends Bundle {
  val remote = Output(UInt(p(ZJParametersKey).nodeAidBits.W))
  val local = Input(UInt(p(ZJParametersKey).nodeAidBits.W))
}

class SocketIcnSide(node:Node)(implicit p:Parameters) extends Module with SocketCommon {
  val socketType = node.socket
  val io = IO(new Bundle {
    val icn = new DeviceIcnBundle(node)
    val socket = new SocketIcnSideBundle(node)
  })
  io.socket.resetTx := reset
  if(testType("sync")) {
    io.socket.sync.get <> io.icn
  }
  if(testType("async")) {
    val asyncModule = Module(new IcnSideAsyncModule(node))
    asyncModule.io.icn <> io.icn
    io.socket.async.get <> asyncModule.io.async
  }
  if(testType("c2c")) {
    val c2c = Module(new C2cPacker)
    c2c.io.userTx := DontCare
    connChn(Some(c2c.io.icn.rx.req), io.icn.rx.req)
    connChn(Some(c2c.io.icn.rx.rsp), io.icn.rx.resp)
    connChn(Some(c2c.io.icn.rx.dat), io.icn.rx.data)
    connChn(Some(c2c.io.icn.rx.snp), io.icn.rx.snoop)

    connChn(io.icn.tx.req, Some(c2c.io.icn.tx.req))
    connChn(io.icn.tx.resp, Some(c2c.io.icn.tx.rsp))
    connChn(io.icn.tx.data, Some(c2c.io.icn.tx.dat))
    connChn(io.icn.tx.snoop, Some(c2c.io.icn.tx.snp))

    io.socket.c2c.get <> c2c.io.c2c
    io.socket.c2c.get.tx <> c2c.io.c2c.tx
  }
}

class SocketDevSide(node:Node)(implicit p:Parameters) extends Module with SocketCommon {
  val socketType = node.socket
  val io = IO(new Bundle {
    val icn = new IcnBundle(node)
    val socket = new SocketDevSideBundle(node)
    val resetOut = Output(AsyncReset())
  })
  io.resetOut := io.socket.resetRx
  if(testType("sync")) {
    io.socket.sync.get <> io.icn
  }
  if(testType("async")) {
    val asyncModule = Module(new DeviceSideAsyncModule(node))
    asyncModule.io.icn <> io.icn
    io.socket.async.get <> asyncModule.io.async
  }
  if(testType("c2c")) {
    val c2c = Module(new C2cPacker)
    connChn(Some(c2c.io.icn.rx.req), io.icn.rx.req)
    connChn(Some(c2c.io.icn.rx.rsp), io.icn.rx.resp)
    connChn(Some(c2c.io.icn.rx.dat), io.icn.rx.data)
    connChn(Some(c2c.io.icn.rx.snp), io.icn.rx.snoop)

    connChn(io.icn.tx.req, Some(c2c.io.icn.tx.req))
    connChn(io.icn.tx.resp, Some(c2c.io.icn.tx.rsp))
    connChn(io.icn.tx.data, Some(c2c.io.icn.tx.dat))
    connChn(io.icn.tx.snoop, Some(c2c.io.icn.tx.snp))

    io.socket.c2c.get <> c2c.io.c2c
  }
}
