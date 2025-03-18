package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import dongjiang.directory._
import xs.utils.debug._

import dongjiang.frontend.decode._

class Issue(dirBank: Int)(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    // Configuration
    val config      = new DJConfigIO()
    // In
    val task_s3     = Flipped(Valid(new DJBundle {
      val chi       = new ChiTask with HasAddr
      val pos       = new PosIndex()
      val dir       = new DirMsg()
      val alrDeqDB  = Bool()
      val code      = new TaskCode()
    }))
    // Out
    val commit_s4   = Valid(new DJBundle {
      val chi       = new ChiTask
      val pos       = new PosIndex()
      val dir       = new DirMsg()
      val alrDeqDB  = Bool()
      val ops       = new Operations()
    })
    val cmTask_s4   = Decoupled(new DJBundle {
      val chi       = new ChiTask with HasAddr
      val pos       = new PosIndex()
      val ops       = new Operations()
      val alrDeqDB  = Bool()
      val snpVec    = Vec(nrSfMetas, Bool())
    })
    val alrUseBuf   = Output(UInt(issueBufBits.W))
  })
  dontTouch(io)
  HardwareAssertion(!io.task_s3.valid)
  io <> DontCare

  /*
   * Module and Reg declaration
   */
//  val cmTaskQ_s4    = Module(new DecoupledQueue(chiselTypeOf(io.cmTask_s4.bits), size = nrIssueBuf))
  val cmtValid_s4   = RegNext(io.task_s3.valid)
  val commitReg_s4  = Reg(chiselTypeOf(io.commit_s4.bits))

  /*
   * Receive task and trans to commit
   */
  // TODO
  commitReg_s4      := io.task_s3.bits.asTypeOf(commitReg_s4)
  commitReg_s4.ops  := io.task_s3.bits.code

  /*
   * Receive task and trans to cmTask
   */
//  cmTaskQ_s4.io.enq.valid := io.task_s3.valid
//  HardwareAssertion(cmTaskQ_s4.io.enq.ready, io.task_s3.valid)
//
//  val cmTask      = cmTaskQ_s4.io.enq.bits
//  val task_s3     = io.task_s3.bits
//  cmTask.pos      := task_s3.pos
//  cmTask.ops      := task_s3.code
//  cmTask.alrDeqDB := task_s3.alrDeqDB
//  cmTask.chi      := task_s3.chi
//  // set by decode
//  cmTask.chi.channel    := Mux(task_s3.code.snoop, ChiChannel.SNP, ChiChannel.REQ)
//  cmTask.chi.opcode     := task_s3.code.opcode
//  cmTask.chi.expCompAck := task_s3.code.expCompAck
//  cmTask.chi.retToSrc   := task_s3.code.retToSrc
//  cmTask.chi.snpVec     := PriorityMux(Seq(
//    task_s3.code.snpAll -> task_s3.dir.sf.metaVec.map(_.state.asBool),
//
//  ))
//



  /*
   * HardwareAssertion placePipe
   */
   HardwareAssertion.placePipe(Int.MaxValue-2)
}