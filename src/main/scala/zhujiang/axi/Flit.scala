package zhujiang.axi

import chisel3._
import chisel3.util.{Decoupled, DecoupledIO}

case class AxiParams(
  addrBits: Int = 32,
  idBits: Int = 5,
  userBits: Int = 0,
  dataBits: Int = 64,
  attr:String = ""
)

class AWFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val addr = UInt(params.addrBits.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
//  val lock = UInt(2.W)
  val cache = UInt(4.W)
//  val prot = UInt(3.W)
//  val qos = UInt(4.W)
//  val region = UInt(4.W)
  val user = UInt(params.userBits.W)
}

class ARFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val addr = UInt(params.addrBits.W)
  val len = UInt(8.W)
  val size = UInt(3.W)
  val burst = UInt(2.W)
//  val lock = UInt(2.W)
  val cache = UInt(4.W)
//  val prot = UInt(3.W)
//  val qos = UInt(4.W)
//  val region = UInt(4.W)
  val user = UInt(params.userBits.W)
}

class WFlit(params: AxiParams) extends Bundle {
  val data = UInt(params.dataBits.W)
  val strb = UInt((params.dataBits / 8).W)
  val last = Bool()
  val user = UInt(params.userBits.W)
}

class RFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val data = UInt(params.dataBits.W)
  val resp = UInt(2.W)
  val last = Bool()
  val user = UInt(params.userBits.W)
}

class BFlit(params: AxiParams) extends Bundle {
  val id = UInt(params.idBits.W)
  val resp = UInt(2.W)
  val user = UInt(params.userBits.W)
}

object AxiUtils {
  def extConn(extnl:ExtAxiBundle, intnl: AxiBundle):Unit = {
    for((chn, bd) <- intnl.elements) {
      val dcp = bd.asInstanceOf[DecoupledIO[Bundle]]
      extnl.elements(s"${chn}valid") <> dcp.valid
      extnl.elements(s"${chn}ready") <> dcp.ready
      for((field, sig) <- dcp.bits.elements) {
        extnl.elements(s"$chn$field") <> sig
      }
    }
  }
}

class AxiBundle(val params: AxiParams) extends Bundle {
  val aw = Decoupled(new AWFlit(params))
  val ar = Decoupled(new ARFlit(params))
  val w = Decoupled(new WFlit(params))
  val b = Flipped(Decoupled(new BFlit(params)))
  val r = Flipped(Decoupled(new RFlit(params)))

  def <>(that: ExtAxiBundle):Unit = AxiUtils.extConn(that, this)
}

class ExtAxiBundle(val params: AxiParams) extends Bundle {
  val awvalid = Output(Bool())
  val awready = Input(Bool())
  val awid = Output(UInt(params.idBits.W))
  val awaddr = Output(UInt(params.addrBits.W))
  val awlen = Output(UInt(8.W))
  val awsize = Output(UInt(3.W))
  val awburst = Output(UInt(2.W))
  //  val awlock = Output(UInt(2.W))
  val awcache = Output(UInt(4.W))
  //  val awprot = Output(UInt(3.W))
  //  val awqos = Output(UInt(4.W))
  //  val awregion = Output(UInt(4.W))
  val awuser = Output(UInt(params.userBits.W))

  val arvalid = Output(Bool())
  val arready = Input(Bool())
  val arid = Output(UInt(params.idBits.W))
  val araddr = Output(UInt(params.addrBits.W))
  val arlen = Output(UInt(8.W))
  val arsize = Output(UInt(3.W))
  val arburst = Output(UInt(2.W))
  //  val arlock = Output(UInt(2.W))
  val arcache = Output(UInt(4.W))
  //  val arprot = Output(UInt(3.W))
  //  val arqos = Output(UInt(4.W))
  //  val arregion = Output(UInt(4.W))
  val aruser = Output(UInt(params.userBits.W))

  val wvalid = Output(Bool())
  val wready = Input(Bool())
  val wdata = Output(UInt(params.dataBits.W))
  val wstrb = Output(UInt((params.dataBits / 8).W))
  val wlast = Output(Bool())
  val wuser = Output(UInt(params.userBits.W))

  val bvalid = Input(Bool())
  val bready = Output(Bool())
  val bid = Input(UInt(params.idBits.W))
  val bresp = Input(UInt(2.W))
  val buser = Input(UInt(params.userBits.W))

  val rvalid = Input(Bool())
  val rready = Output(Bool())
  val rid = Input(UInt(params.idBits.W))
  val rdata = Input(UInt(params.dataBits.W))
  val rresp = Input(UInt(2.W))
  val rlast = Input(Bool())
  val ruser = Input(UInt(params.userBits.W))

  def <>(that: AxiBundle):Unit = AxiUtils.extConn(this, that)
}