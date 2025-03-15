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
    val task_s2     = Flipped(Valid(new ChiTask with HasPosIndex))
    val respDir_s3  = Flipped(Valid(new DJBundle {
      val llc       = new DirEntry("llc")
      val sf        = new DirEntry("sf")
      val alrDeqDB  = Bool()
    }))
    val task_s3     = Valid(new ChiTask with HasPosIndex with HasDirMsg {
      val code      = new Code()
      val alrDeqDB  = Bool()
    })
    val canNest_s3  = Valid(new PosIndex())
  })
  dontTouch(io)
  HardwareAssertion(!io.task_s2.valid)

  // TODO: add HarewareAssert
  // TODO: use alrWriDB

  /*
   * Decode Table: Seq[(UInt, Seq[(UInt, UInt)])] <=> Seq[(Req, Seq[(State, Code)])] <=> [x, [y, z]]
   * table is a two-dimensional vector of n x m
   */
  val table     = Read_DCT_DMT.table ++ Dataless.table
  val n         = table.length
  val m         = table.map(_._2.length).max
  val table_reqVec    = WireInit(VecInit(Seq.fill(n) { 0.U(new ReqInst().getWidth.W) }))
  val table_stateVec2 = WireInit(VecInit(Seq.fill(n) { VecInit(Seq.fill(m) { 0.U(new StateInst().getWidth.W) }) }))
  val table_codeVec2  = WireInit(VecInit(Seq.fill(m) { VecInit(Seq.fill(m) { 0.U(new Code().getWidth.W)      }) }))
  val table_stateVec  = table_stateVec2.map(_.reduce(_ ## _))
  val table_codeVec   = table_codeVec2.map(_.reduce(_ ## _))
  table.zipWithIndex.foreach {
    case(t, i) =>
      table_reqVec(i) := t._1
      t._2.zipWithIndex.foreach {
        case(t2, j) =>
          table_stateVec2(i)(j) := t2._1
          table_codeVec2(i)(j)  := t2._2
      }
  }


  /*
   * Reg and Wire declaration
   */
  val reqInst_s2      = Wire(new ReqInst())
  val preStateReg_s3  = RegInit(VecInit(Seq.fill(m) { 0.U.asTypeOf(new StateInst) }))
  val preCodeReg_s3   = RegInit(VecInit(Seq.fill(m) { 0.U.asTypeOf(new Code)      }))
  val validReg_s3     = RegNext(io.task_s2.valid)
  val taskReg_s3      = RegEnable(io.task_s2.bits, 0.U.asTypeOf(io.task_s2.bits), io.task_s2.valid)
  val stateInst_s3    = Wire(new StateInst())
  HardwareAssertion(!(validReg_s3 ^ io.respDir_s3.valid))

  /*
   * [S2]: Pre-Decode
   */
  reqInst_s2.channel  := io.task_s2.bits.channel
  reqInst_s2.toLAN    := io.task_s2.bits.isLAN
  reqInst_s2.opcode   := io.task_s2.bits.opcode
  preStateReg_s3      := Decode.decode(reqInst_s2.asUInt, table_reqVec.zip(table_stateVec)).asTypeOf(preStateReg_s3)
  preCodeReg_s3       := Decode.decode(reqInst_s2.asUInt, table_reqVec.zip(table_codeVec)).asTypeOf(preCodeReg_s3)

  /*
   * [S3]: Decode
   */
  val metaId_s3     = taskReg_s3.metaId
  val dirValid_s3   = io.respDir_s3.valid
  // Get SF
  val sfHit_s3      = io.respDir_s3.bits.sf.hit
  val unique_s3     = io.respDir_s3.bits.sf.uniqueOpt.get
  val srcHit_s3     = io.respDir_s3.bits.sf.metaVec(metaId_s3).state.asBool
  val othHit_s3     = io.respDir_s3.bits.sf.metaVec.map(_.state).zipWithIndex.map { case(m, i) => m & metaId_s3 =/= i.U }.reduce(_ | _).asBool
  val sfState_s3    = Mux(unique_s3, ChiState.UD, ChiState.SC)
  val srcState_s3   = Mux(dirValid_s3 & srcHit_s3, sfState_s3, ChiState.I)
  val othState_s3   = Mux(dirValid_s3 & othHit_s3, sfState_s3, ChiState.I)
  // Get LLC
  val llcHit_s3     = io.respDir_s3.bits.llc.hit
  val _llcState_s3  = io.respDir_s3.bits.llc.metaVec.head.state
  val llcState_s3   = Mux(dirValid_s3 & llcHit_s3, _llcState_s3, ChiState.I)

  stateInst_s3.valid      := true.B
  stateInst_s3.srcState   := srcState_s3
  stateInst_s3.othState   := othState_s3
  stateInst_s3.llcState   := llcState_s3
  stateInst_s3.expCompAck := taskReg_s3.expCompAck

  /*
   * [S3]: Decode
   */
  val code_s3 = Decode.decode(stateInst_s3.asUInt, preStateReg_s3.map(_.asUInt).zip(preCodeReg_s3.map(_.asUInt)))

  /*
   * [S3]: Output S3
   */
  // task_s3
  io.task_s3.valid          := validReg_s3
  io.task_s3.bits           := taskReg_s3.asTypeOf(io.task_s3.bits)
  io.task_s3.bits.llc       := io.respDir_s3.bits.llc
  io.task_s3.bits.sf        := io.respDir_s3.bits.sf
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