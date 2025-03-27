package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._

trait HasLockSet extends DJBundle { this: DJBundle =>
  val lock = new Bool()
}

trait HasNest extends DJBundle { this: DJBundle =>
  val canNest = new Bool()
}

class PoS(dirBank: Int)(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    val config      = new DJConfigIO()
    // req and ack
    val req_s0       = Flipped(Valid(new Addr with HasChiChannel))
    val sleep_s1    = Output(Bool())
    val full_s1     = Output(Bool())
    val posIdx_s1   = Output(new PosIndex())
    // retry from block
    val retry_s1    = Input(Bool())
    // Get Full Addr In PoS
    val getAddr     = Input(new PosIndex())
    val respAddr    = Output(new Addr())
    // update PoS
    val updNest     = Flipped(Valid(new PackPosIndex with HasNest))
    val updTag      = Flipped(Valid(new PackPosIndex with HasAddr))
    val clean       = Flipped(Valid(new PackPosIndex with HasChiChannel))
    val lockSet     = Flipped(Valid(new PackPosIndex with HasLockSet)) // TODO
    // wakeup TaskBuf Entry
    val wakeup      = Valid(new Addr)
    // PoS Busy Signal
    val busy        = Output(UInt(2.W))
  })

  io.respAddr := DontCare // TODO

  /*
   * Reg and Wire declaration
   */
  val tagTable    = Reg(Vec(posSets, Vec(posWays, UInt(posTagBits.W))))
  val ctrlTable   = RegInit(VecInit(Seq.fill(posSets) { VecInit(Seq.fill(posWays) { 0.U.asTypeOf(new DJBundle with HasNest {
    val validVec  = UInt(2.W)
    def hasReq    = validVec(0)
    def hasSnp    = validVec(1)
    def valid     = validVec.orR
  }) }) }))
  val reqReg_s1   = Reg(Valid(new DJBundle with HasPackPosIndex {
    val tag       = UInt(posTagBits.W)
    val isSnp     = Bool()
  }))


  /*
   * Receive req from taskBuf
   *
   * 1. req is CHIREQ: not block by addr
   * 2. req is CHISNP:
   *  a. not block by addr
   *  b. nest someone
   */
  // get block message
  val reqSet      = io.req_s0.bits.Addr.posSet
  val ctrlSet     = ctrlTable(reqSet)
  val addrSet     = tagTable(reqSet)
  val posValidVec = ctrlSet.map(_.valid)
  val tagMatchVec = addrSet.map(_ === io.req_s0.bits.Addr.posTag)
  val matchVec    = posValidVec.zip(tagMatchVec).map { case(a, b) => a & b }
  // judge nest
  val nestVec     = matchVec.zip(ctrlSet.map(_.canNest)).map { case(a, b) => a & b }
  val nestWay     = PriorityEncoder(nestVec)
  HardwareAssertion.withEn(!ctrlSet(nestWay).hasSnp, io.req_s0.valid & io.req_s0.bits.isSnp)
  // get free way
  val freeWayVec  = ctrlSet.map(!_.valid)
  val hasFreeWay  = freeWayVec.reduce(_ | _)
  val freeWay     = PriorityEncoder(freeWayVec)
  // judge block
  val hasMatch    = matchVec.reduce(_ | _)
  val snpNest     = nestVec.reduce(_ | _) & io.req_s0.bits.isSnp
  val receive     = Mux(hasMatch, snpNest, hasFreeWay)

  /*
   * Store req from taskBuf
   */
  reqReg_s1.valid        := io.req_s0.valid & receive
  reqReg_s1.bits.tag     := io.req_s0.bits.Addr.posTag
  reqReg_s1.bits.isSnp   := io.req_s0.bits.isSnp
  reqReg_s1.bits.pos.set := reqSet
  reqReg_s1.bits.pos.way := Mux(snpNest, nestWay, freeWay)

  /*
   * Retrun ack to taskBuf and block
   */
  io.sleep_s1   := RegNext(io.req_s0.valid & !receive & hasMatch)
  io.full_s1    := RegNext(io.req_s0.valid & !receive & !hasFreeWay)
  io.posIdx_s1  := reqReg_s1.bits.pos

  /*
   * Modify pos tag
   */
  tagTable.zipWithIndex.foreach {
    case(tagSet, i) =>
      tagSet.zipWithIndex.foreach {
        case(tag, j) =>
          val updHit  = io.updTag.valid & io.updTag.bits.pos.idxMatch(i, j)
          val reqHit  = reqReg_s1.valid & reqReg_s1.bits.pos.idxMatch(i , j) & !io.retry_s1
          when(updHit) {
            tag := io.updTag.bits.Addr.posTag
          }.elsewhen(reqHit) {
            tag := reqReg_s1.bits.tag
          }
          HardwareAssertion(!(updHit & reqHit))
      }
      HardwareAssertion.placePipe(Int.MaxValue-3)
  }

  /*
   * Modify pos ctrl
   */
  ctrlTable.zipWithIndex.foreach {
    case (ctrlSet, i) =>
      ctrlSet.zipWithIndex.foreach {
        case (ctrl, j) =>
          // receive clean
          val cleanHit    = io.clean.valid & io.clean.bits.pos.idxMatch(i, j)
          val cleanSnp    = io.clean.bits.isSnp
          // store req
          val reqHit      = reqReg_s1.valid & reqReg_s1.bits.pos.idxMatch(i, j) & !io.retry_s1
          val reqIsSnp    = reqReg_s1.bits.isSnp
          // modify validVec
          val validVecNxt = WireInit(0.U(2.W))
          ctrl.validVec   := validVecNxt
          when(cleanHit) {
            validVecNxt   := ctrl.validVec & Mux(cleanSnp, "b01".U, "b10".U)
          }.elsewhen(reqHit) {
            validVecNxt   := ctrl.validVec | Mux(reqIsSnp, "b10".U, "b01".U)
          }
          // modify canNest
          val updNestHit  = io.updNest.valid & io.updNest.bits.pos.idxMatch(i, j)
          ctrl.canNest    := Mux(reqHit, false.B, Mux(updNestHit, io.updNest.bits.canNest, ctrl.canNest))
          // assert
          HardwareAssertion.withEn(PopCount(ctrl.validVec ^ validVecNxt) === 1.U, cleanHit | reqHit,
                                                                    desc = cf"PoS Index[${i}][${j}]")
          HardwareAssertion(PopCount(Seq(cleanHit, reqHit)) <= 1.U, desc = cf"PoS Index[${i}][${j}]")
          if(j % 4 == 0) HardwareAssertion.placePipe(Int.MaxValue-3)
      }
  }


  /*
   * wakeup someone by addr
   */
  // get wakeup addr
  val cleanWay = io.clean.bits.pos.way
  val cleanSet = io.clean.bits.pos.set
  val cleanTag = tagTable(cleanSet)(cleanWay)
  io.wakeup.bits.Addr.catPoS(io.config.bankId, cleanTag, cleanSet, dirBank.U(dirBankBits.W))
  // get wakeup valid
  io.wakeup.valid := io.clean.valid & ctrlTable(cleanSet)(cleanWay).validVec.xorR

  /*
   * PoS Busy Signal
   */
  val useNum  = PopCount(ctrlTable.flatten.map(_.valid))
  val posBusy = PriorityMux(Seq(
    (useNum < (nrPoS*0.5 ).toInt.U) -> "b00".U,
    (useNum < (nrPoS*0.75).toInt.U) -> "b01".U,
    (useNum < (nrPoS*0.9 ).toInt.U) -> "b10".U,
    true.B                          -> "b11".U,
  ))
  io.busy := RegNext(posBusy)

  /*
   * HardwareAssertion placePipe
   */
  HardwareAssertion.placePipe(Int.MaxValue-2)
}