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

class Decode(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    // Configuration
    val config      = new DJConfigIO()
    val task_s2     = Flipped(Valid(new DJBundle {
      val chi       = new ChiTask with HasAddr
      val pos       = new PosIndex()
    }))
    val respDir_s3  = Flipped(Valid(new DJBundle {
      val llc       = new DirEntry("llc")
      val sf        = new DirEntry("sf")
      val alrDeqDB  = Bool()
    }))
    val task_s3     = Valid(new DJBundle {
      val chi       = new ChiTask with HasAddr
      val pos       = new PosIndex()
      val dir       = new DirMsg()
      val code      = new TaskCode()
      val alrDeqDB  = Bool()
    })
    val canNest_s3  = Valid(new PosIndex())
  })

  dontTouch(io)

  /*
   * Reg and Wire declaration
   */
  val chiInst_s2          = Wire(new ChiInst)
  val stateInstVecReg_s3  = RegInit(VecInit(Seq.fill(Decode.l_si) { 0.U.asTypeOf(new StateInst) }))
  val taskCodeVecReg_s3   = RegInit(VecInit(Seq.fill(Decode.l_si) { 0.U.asTypeOf(new TaskCode)  }))
  val validReg_s3         = RegNext(io.task_s2.valid)
  val taskReg_s3          = RegEnable(io.task_s2.bits, 0.U.asTypeOf(io.task_s2.bits), io.task_s2.valid)
  val stateInst_s3        = Wire(new StateInst())

  /*
   * [S2]: Pre-Decode
   */
  chiInst_s2.channel    := io.task_s2.bits.chi.channel
  chiInst_s2.fromLAN    := io.task_s2.bits.chi.fromLAN
  chiInst_s2.toLAN      := io.task_s2.bits.chi.toLAN(io.config.ci)
  chiInst_s2.opcode     := io.task_s2.bits.chi.opcode
  chiInst_s2.expCompAck := io.task_s2.bits.chi.expCompAck
  stateInstVecReg_s3    := Decode.decode(chiInst_s2)._1._1
  taskCodeVecReg_s3     := Decode.decode(chiInst_s2)._1._2

  /*
   * [S3]: Decode
   */
  val metaId_s3     = taskReg_s3.chi.metaId
  val dirValid_s3   = io.respDir_s3.valid
  // Get SF
  val sfHit_s3      = io.respDir_s3.bits.sf.hit
  val _srcHit_s3    = io.respDir_s3.bits.sf.metaVec(metaId_s3).state.asBool
  val _othHit_s3    = io.respDir_s3.bits.sf.metaVec.map(_.state).zipWithIndex.map { case(m, i) => m & metaId_s3 =/= i.U }.reduce(_ | _).asBool
  val srcHit_s3     = Mux(dirValid_s3 & sfHit_s3, _srcHit_s3, false.B)
  val othHit_s3     = Mux(dirValid_s3 & sfHit_s3, _othHit_s3, false.B)
  // Get LLC
  val llcHit_s3     = io.respDir_s3.bits.llc.hit
  val _llcState_s3  = io.respDir_s3.bits.llc.metaVec.head.state
  val llcState_s3   = Mux(dirValid_s3 & llcHit_s3, _llcState_s3, ChiState.I)
  HardwareAssertion.withEn(io.respDir_s3.valid, validReg_s3 & taskReg_s3.chi.memAttr.cacheable)

  stateInst_s3.valid    := true.B
  stateInst_s3.srcHit   := srcHit_s3
  stateInst_s3.othHit   := othHit_s3
  stateInst_s3.llcState := llcState_s3

  /*
   * [S3]: Decode
   */
  val code_s3 = Decode.decode(stateInst_s3, stateInstVecReg_s3, taskCodeVecReg_s3)

  /*
   * [S3]: Output S3
   */
  // task_s3
  io.task_s3.valid          := validReg_s3
  io.task_s3.bits.chi       := taskReg_s3.chi
  io.task_s3.bits.pos       := taskReg_s3.pos
  io.task_s3.bits.dir       := io.respDir_s3.bits
  io.task_s3.bits.alrDeqDB  := io.respDir_s3.bits.alrDeqDB
  io.task_s3.bits.code      := code_s3
  // canNest_s3
  io.canNest_s3.valid       := validReg_s3 & code_s3.canNest
  io.canNest_s3.bits        := taskReg_s3.pos


  /*
   * HardwareAssertion placePipe
   */
   HardwareAssertion.placePipe(Int.MaxValue-2)
}