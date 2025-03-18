package dongjiang.directory

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug.{DomainInfo, HardwareAssertion}

class Directory(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    val config      = Input(new DJConfigIO())
    // Read from frontends
    val readVec     = Vec(djparam.nrDirBank, Flipped(Decoupled(new DJBundle with HasAddr with HasPosIndex {
      val early     = Bool() // Early access to data
    })))
    // Resp to frontends
    val rRespVec    = Vec(djparam.nrDirBank, Valid(new DJBundle {
      val llc       = new DirEntry("llc") with HasPosIndex
      val sf        = new DirEntry("sf")  with HasPosIndex
      val alrDeqDB  = Bool() // Already Req DataBuffer
    }))
    // Write From backend
    val write       = new DJBundle {
      val llc       = Flipped(Decoupled(new DirEntry("llc") with HasPosIndex))
      val sf        = Flipped(Decoupled(new DirEntry("sf")  with HasPosIndex))
    }
    // Resp to backend
    val wResp       = Valid(new DJBundle {
      val llc       = new DirEntry("llc")
      val sf        = new DirEntry("sf")
    })
    // Read LLC Hit Message to Data
    val rHitMesVec  = Vec(djparam.nrDirBank, Decoupled(new DJBundle with HasPosIndex {
      val set       = UInt(llcSetBits.W)
      val way       = UInt(llcWayBits.W)
    }))
    // Clean Signal from backend(Replace CM and Commit CM)
    val unlockVec2  = Vec(djparam.nrDirBank, Vec(2, Flipped(Valid(new PosIndex()))))
  })

  /*
   * Module declaration
   */
  val llcs              = Seq.tabulate(djparam.nrDirBank)(i => Module(new DirectoryBase("llc", i)))
  val sfs               = Seq.tabulate(djparam.nrDirBank)(i => Module(new DirectoryBase("sf", i)))
  val wriLLCBankPipe    = Module(new Pipe(UInt(dirBankBits.W), readDirLatency))
  val wriSFBankPipe     = Module(new Pipe(UInt(dirBankBits.W), readDirLatency))
  val earlyShiftRegVec  = RegInit(VecInit(Seq.fill(djparam.nrDirBank) { 0.U.asTypeOf(new Shift(readDirLatency)) }))

  /*
   * Connect llcs and sfs
   */
  io.write.llc.ready := DontCare
  io.write.sf.ready  := DontCare
  llcs.zip(sfs).zipWithIndex.foreach {
    case((llc, sf), i) =>
      // config
      llc.io.config         := io.config
      sf.io.config          := io.config

      // read valid
      llc.io.read.valid     := io.readVec(i).valid & sf.io.read.ready
      sf.io.read.valid      := io.readVec(i).valid & llc.io.read.ready
      // read bits
      llc.io.read.bits      := io.readVec(i).bits
      sf.io.read.bits       := io.readVec(i).bits
      // read ready
      io.readVec(i).ready   := llc.io.read.ready & sf.io.read.ready

      // write valid
      llc.io.write.valid    := io.write.llc.valid & io.write.llc.bits.dirBank === i.U
      sf.io.write.valid     := io.write.sf.valid  & io.write.sf.bits.dirBank === i.U
      // write bits
      llc.io.write.bits     := io.write.llc.bits
      sf.io.write.bits      := io.write.sf.bits
      // write read
      when(io.write.llc.bits.bankId === i.U) {
        io.write.llc.ready  := llc.io.write.ready
      }
      when(io.write.sf.bits.bankId === i.U) {
        io.write.sf.ready   := sf.io.write.ready
      }

      // Clean Lock Table
      llc.io.unlockVec.zip(io.unlockVec2(i)).foreach { case(a, b) => a := b }
      sf.io.unlockVec.zip(io.unlockVec2(i)).foreach  { case(a, b) => a := b }
  }

  /*
   * Connect IO
   */
  // Read Resp
  io.rRespVec.map(_.valid).zip(llcs.map(_.io.resp)).foreach    { case(a, b) => a := b.valid }
  io.rRespVec.map(_.bits.alrDeqDB).zip(io.rHitMesVec).foreach  { case(a, b) => a := b.fire }
  io.rRespVec.map(_.bits.llc).zip(llcs.map(_.io.resp)).foreach { case(a, b) => a := b.bits }
  io.rRespVec.map(_.bits.sf ).zip( sfs.map(_.io.resp)).foreach { case(a, b) => a := b.bits }

  // Store Write LLC Resp DirBank
  wriLLCBankPipe.io.enq.valid := io.write.llc.fire
  wriLLCBankPipe.io.enq.bits  := io.write.llc.bits.bankId

  // Store Write SF Resp DirBank
  wriSFBankPipe.io.enq.valid  := io.write.sf.fire
  wriSFBankPipe.io.enq.bits   := io.write.sf.bits.bankId

  // Store Early Request
  earlyShiftRegVec.zip(io.readVec).foreach { case(a, b) => a.input(b.fire & b.bits.early) }

  // Output wResp and rHitMesVec
  io.wResp <> DontCare
  llcs.zip(sfs).zipWithIndex.foreach {
    case ((llc, sf), i) =>
      io.wResp.valid      := llc.io.resp.valid
      // LLC wResp
      when(wriLLCBankPipe.io.deq.bits === i.U) {
        io.wResp.bits.llc := llc.io.resp.bits
      }
      // SF wResp
      when(wriSFBankPipe.io.deq.bits === i.U) {
        io.wResp.bits.sf  := sf.io.resp.bits
      }
      // Hit Resp
      io.rHitMesVec(i).valid        := earlyShiftRegVec(i).isValid & llc.io.resp.bits.hit
      io.rHitMesVec(i).bits.pos     := llc.io.resp.bits.pos
      io.rHitMesVec(i).bits.set     := llc.io.resp.bits.set
      io.rHitMesVec(i).bits.way     := OHToUInt(llc.io.resp.bits.wayOH)
  }

  /*
   * HardwareAssertion placePipe
   */
   HardwareAssertion.placePipe(Int.MaxValue-1)
}