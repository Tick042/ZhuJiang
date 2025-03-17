package dongjiang.frontend.decode

import chisel3._
import chisel3.util._
import dongjiang.bundle
import org.chipsalliance.cde.config.Parameters
import zhujiang.chi._
import dongjiang.bundle._
import xs.utils.ParallelLookUp
import dongjiang.frontend.decode.DecodeCHI._
import math.max
import dongjiang.bundle.ChiChannel._


/*
 *   ChiInst <-> [StateInst <-> (TaskCode, [TaskInst <-> CommitCode])]
 *      ^             ^                        ^
 * RxReq/RxSnp     DirResp              ReqResp/SnpResp
 *
 * Table: Seq[(UInt, Seq[(UInt, UInt)])] <=> Seq[(ChiInst, Seq[(StateInst, Code)])]
 * Table is a two-dimensional vector of n x m
 *
 * Code: (UInt, Seq[(UInt, UInt)]) <=> (TaskCode, Seq[(TaskInst, CommitCode)])
 * Code is a tuple, the second element is of length k
 *
 */

class ChiInst extends Bundle {
  val channel     = UInt(ChiChannel.width.W)
  val toLAN       = Bool() // true -> LAN; false -> BBN; only use in REQ
  val opcode      = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val expCompAck  = Bool()
}

class StateInst extends Bundle {
  val valid       = Bool()
  val srcState    = UInt(ChiState.width.W)
  val othState    = UInt(ChiState.width.W)
  val llcState    = UInt(ChiState.width.W)
}

class TaskInst extends Bundle {
  val valid       = Bool()
  val fwdValid    = Bool()
  val channel     = UInt(ChiChannel.width.W)
  val opcode      = UInt(RspOpcode.width.max(DatOpcode.width).W)
  val resp        = UInt(ChiResp.width.W)
  val fwdResp     = UInt(ChiResp.width.W)
}

trait HasOperations { this: Bundle =>
  val snoop       = Bool()
  val read        = Bool()
  val dataless    = Bool()
  val wriOrAtm    = Bool() // Write or Atomic
}

object SnpTgt {
  val width       = 2
  val NONE        = "b00".U
  val ALL         = "b01".U
  val ONE         = "b10".U // Select first other
  val OTH         = "b11".U
}

trait HasTaskCode { this: Bundle with HasOperations =>
  // Common
  val opcode      = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val needDB      = Bool()
  val canNest     = Bool()

  // Req
  val expCompAck  = Bool()
  def doDMT       = read & !needDB

  // Snoop
  val retToSrc    = Bool()
  val snpTgt      = UInt(SnpTgt.width.W)
}

class TaskCode extends Bundle with HasOperations with HasTaskCode

trait HasCommitCode { this: Bundle with HasOperations =>
  // Commit
  val commit      = Bool()
  val fwdCommit   = Bool()
  val channel     = UInt(ChiChannel.width.W)
  val commitOp    = UInt(RspOpcode.width.max(DatOpcode.width).W)
  val resp        = UInt(ChiResp.width.W)
  val fwdResp     = UInt(ChiResp.width.W)

  // Second Req/Snp
  val opcode      = UInt(ReqOpcode.width.max(SnpOpcode.width).W)
  val needDB      = Bool()
  val expCompAck  = Bool()
  val retToSrc    = Bool()
  val snpTgt      = UInt(SnpTgt.width.W)
  val canNest     = Bool()
  val waitSecDone = Bool()
  def doDMT       = read & waitSecDone

  // Write Directory
  val wriSF       = UInt(SnpTgt.width.W)
  val wriLLC      = Bool()
  val srcState    = UInt(ChiState.width.W)
  val othState    = UInt(ChiState.width.W)
  val llcState    = UInt(ChiState.width.W)
}

class CommitCode extends Bundle with HasOperations with HasCommitCode

object DecodeCHI {
  val width = ChiResp.width

  val I  = "b000".U(width.W)
  val SC = "b001".U(width.W)
  val UC = "b010".U(width.W)
  val UD = "b011".U(width.W)

  val I_PD  = "b100".U(width.W)
  val SC_PD = "b101".U(width.W)
  val UC_PD = "b110".U(width.W)
  val UD_PD = "b110".U(width.W)
  val SD_PD = "b111".U(width.W)

  def toResp(x: UInt): UInt = {
    val result = WireInit(0.U(ChiResp.width.W))
    when(x === UD) {
      result := ChiResp.SD
    }.otherwise {
      result := x
    }
    result
  }

  def toState(x: UInt): UInt = {
    val result = WireInit(0.U(ChiState.width.W))
    switch(x) {
      is(I)  { result := ChiState.I  }
      is(SC) { result := ChiState.SC }
      is(UC) { result := ChiState.UC }
      is(UD) { result := ChiState.UD }
    }
    assert(!(x & ChiResp.PassDirty).orR)
    result
  }
}




object Inst {
  // Chi Inst
  def isReq             : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.channel       := REQ;     temp.asUInt }
  def isSnp             : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.channel       := SNP;     temp.asUInt }
  def toLAN             : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.toLAN         := true.B;  temp.asUInt }
  def toBBN             : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.toLAN         := false.B; temp.asUInt }
  def reqIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.opcode        := x;       require(x.getWidth == ReqOpcode.width); temp.asUInt | isReq }
  def snpIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.opcode        := x;       require(x.getWidth == RspOpcode.width); temp.asUInt | isSnp }
  def expCompAck        : UInt = { val temp = WireInit(0.U.asTypeOf(new ChiInst())); temp.expCompAck    := true.B;  temp.asUInt }

  // State Inst
  def stateValid        : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.valid       := true.B;      temp.asUInt }
  def srcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.srcState    := toState(x);  require(x.getWidth == DecodeCHI.width); temp.asUInt | stateValid }
  def othIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.othState    := toState(x);  require(x.getWidth == DecodeCHI.width); temp.asUInt | stateValid }
  def llcIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new StateInst())); temp.llcState    := toState(x);  require(x.getWidth == DecodeCHI.width); temp.asUInt | stateValid }

  // Task Inst
  def taskValid         : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.valid        := true.B;    temp.asUInt }
  def fwdValid          : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.fwdValid     := true.B;    temp.asUInt }
  def isRsp             : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.channel      := RSP;       temp.asUInt | taskValid}
  def isDat             : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.channel      := DAT;       temp.asUInt | taskValid}
  def rspIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.opcode       := x;         require(x.getWidth == RspOpcode.width); temp.asUInt | isRsp }
  def datIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.opcode       := x;         require(x.getWidth == DatOpcode.width); temp.asUInt | isDat }
  def respIs  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.resp         := toResp(x); require(x.getWidth == DecodeCHI.width);   temp.asUInt }
  def fwdIs   (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst())); temp.fwdResp      := toResp(x); require(x.getWidth == DecodeCHI.width);   temp.asUInt | fwdValid}
  def noTaskResp        : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskInst()));                                 temp.asUInt }
}



object Code {
  // Task Code Operations
  def snpAll  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.ALL; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def snpOne  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.ONE; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def snpOth  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.OTH; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def read    (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.read     := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }
  def dataless(x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.dataless := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }
  def wriOrAtm(x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.opcode := x; temp.wriOrAtm := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }

  // Task Code Other
  def needDB            : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.needDB      := true.B; temp.asUInt }
  def canNest           : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.canNest     := true.B; temp.asUInt }
  def taskECA           : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.expCompAck  := true.B; temp.asUInt }
  def retToSrc          : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode())); temp.retToSrc    := true.B; temp.asUInt }
  def nothingTODO       : UInt = { val temp = WireInit(0.U.asTypeOf(new TaskCode()));                             temp.asUInt }

  // Commit Code Commit
  def commit            : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.commit     := true.B;    temp.asUInt }
  def fwdCommit         : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.fwdCommit  := true.B;    temp.asUInt }
  def cmtRsp  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.channel    := RSP;       temp.commitOp := x; require(x.getWidth == RspOpcode.width); temp.asUInt | commit }
  def cmtDat  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.channel    := DAT;       temp.commitOp := x; require(x.getWidth == DatOpcode.width); temp.asUInt | commit }
  def resp    (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.resp       := toResp(x); require(x.getWidth == DecodeCHI.width); temp.asUInt }
  def fwdResp (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.fwdCommit  := toResp(x); require(x.getWidth == DecodeCHI.width); temp.asUInt | fwdCommit }

  // Commit Code Second Operations
  def secSnpAll  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.ALL; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def secSnpOne  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.ONE; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def secSnpOth  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.snoop    := true.B; temp.snpTgt := SnpTgt.OTH; require(x.getWidth == SnpOpcode.width); temp.asUInt }
  def secRead    (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.read     := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }
  def secDataless(x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.dataless := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }
  def secWriOrAtm(x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.opcode := x; temp.wriOrAtm := true.B; require(x.getWidth == ReqOpcode.width); temp.asUInt }
  def secNeedDB            : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.needDB      := true.B;                temp.asUInt }
  def secTaskECA           : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.expCompAck  := true.B;                temp.asUInt }
  def secRetToSrc          : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.retToSrc    := true.B;                temp.asUInt }
  def secCanNest           : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.canNest     := true.B;                temp.asUInt }
  def waitSecDone          : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.waitSecDone := true.B;                temp.asUInt }

  // CommitCode Write SF/LLC
  def wriSRC  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.srcState   := toState(x);  temp.wriSF := true.B;   require(x.getWidth == DecodeCHI.width); temp.asUInt }
  def wriOTH  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.othState   := toState(x);  temp.wriSF := true.B;   require(x.getWidth == DecodeCHI.width); temp.asUInt }
  def wriLLC  (x: UInt) : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode())); temp.llcState   := toState(x);  temp.wriLLC := true.B;  require(x.getWidth == DecodeCHI.width); temp.asUInt }

  // CommitCode ERROR
  def error             : UInt = { val temp = WireInit(0.U.asTypeOf(new CommitCode()));                               temp.asUInt }
}


object Decode {
  def decode(chi: ChiInst = 0.U.asTypeOf(new ChiInst), state: StateInst = 0.U.asTypeOf(new StateInst), task: TaskInst = 0.U.asTypeOf(new TaskInst)) = {
    val table = Read_DCT_DMT.table

    // ChiInst length
    val c = table.length
    // StateInst length
    val s = table.map(_._2.length).max
    // TaskInst length
    val t = table.map(_._2.map(_._2._2.length).max).max

    val chiInstVec      = WireInit(VecInit(Seq.fill(c) { 0.U.asTypeOf(new ChiInst) }))
    val stateInstVec2   = WireInit(VecInit(Seq.fill(c) { VecInit(Seq.fill(s) { 0.U.asTypeOf(new StateInst)  }) }))
    val taskCodeVec2    = WireInit(VecInit(Seq.fill(c) { VecInit(Seq.fill(s) { 0.U.asTypeOf(new TaskCode)   }) }))
    val taskInstVec3    = WireInit(VecInit(Seq.fill(c) { VecInit(Seq.fill(s) { VecInit(Seq.fill(t) { 0.U.asTypeOf(new TaskInst)   }) }) }))
    val commitCodeVec3  = WireInit(VecInit(Seq.fill(c) { VecInit(Seq.fill(s) { VecInit(Seq.fill(t) { 0.U.asTypeOf(new CommitCode) }) }) }))

    table.zipWithIndex.foreach {
      case(t0, i) =>
        chiInstVec(i) := t0._1.asTypeOf(new ChiInst)
        t0._2.zipWithIndex.foreach {
          case(t1, j) =>
            stateInstVec2(i)(j) := t1._1.asTypeOf(new StateInst)
            taskCodeVec2(i)(j)  := t1._2._1.asTypeOf(new TaskCode)
            t1._2._2.zipWithIndex.foreach {
              case(t2, k) =>
                taskInstVec3(i)(j)(k)   := t2._1.asTypeOf(new TaskInst)
                commitCodeVec3(i)(j)(k) := t2._1.asTypeOf(new CommitCode)
            }
        }
    }

    val stateInstVec    = ParallelLookUp(chi.asUInt,    chiInstVec.map(_.asUInt).zip(stateInstVec2))
    val taskCodeVec     = ParallelLookUp(chi.asUInt,    chiInstVec.map(_.asUInt).zip(taskCodeVec2))

    val taskInstVec2    = ParallelLookUp(chi.asUInt,    chiInstVec.map(_.asUInt).zip(taskInstVec3))
    val commitCodeVec2  = ParallelLookUp(chi.asUInt,    chiInstVec.map(_.asUInt).zip(commitCodeVec3))

    val taskInstVec     = ParallelLookUp(state.asUInt,  stateInstVec.map(_.asUInt).zip(taskInstVec2))
    val commitCodeVec   = ParallelLookUp(state.asUInt,  stateInstVec.map(_.asUInt).zip(commitCodeVec2))

    val taskCode        = ParallelLookUp(state.asUInt,  stateInstVec.map(_.asUInt).zip(taskCodeVec))
    val commitCode      = ParallelLookUp(task.asUInt,   taskInstVec.map(_.asUInt).zip(commitCodeVec))

    ((stateInstVec, taskCodeVec), (taskInstVec, commitCodeVec), (taskCode, commitCode), (c, s ,t))
  }

  def decode(inst: StateInst, instVec: Vec[StateInst], codeVec: Vec[TaskCode]): TaskCode = ParallelLookUp(inst.asUInt, instVec.map(_.asUInt).zip(codeVec))

  def decode(inst: TaskInst, instVec: Vec[TaskInst], codeVec: Vec[CommitCode]): CommitCode = ParallelLookUp(inst.asUInt, instVec.map(_.asUInt).zip(codeVec))

  def c: Int = decode()._4._1

  def s: Int = decode()._4._2

  def t: Int = decode()._4._3
}