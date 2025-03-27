package dongjiang.bundle

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang._
import zhujiang.chi._
import dongjiang.frontend.decode._
import zhujiang.chi.ReqOpcode.WriteEvictOrEvict

/*
 * NoC Type
 */
object NocType {
  val LAN = 1.U
  val BBN = 0.U

  def setRx(filt: Flit, t: Int) : Unit = filt.tgt := t.U
  def setRx(filt: Flit, t: UInt): Unit = filt.tgt := t
  def rxIs (filt: Flit, t: Int) : Bool = filt.tgt === t.U

  def setTx(filt: Flit, t: Int) : Unit = filt.src := t.U
  def setTx(filt: Flit, t: UInt): Unit = filt.src := t
  def txIs (filt: Flit, t: Int) : Bool = filt.src === t.U
}


/*
 * Addr:
 * HasAddr -> Addr
 */
trait HasAddr extends DJBundle { this: DJBundle =>
  val addr      = UInt(addrBits.W)

  def addrType: String = "llc"
  object Addr {
    def ci        = getCI(addr)
    def useAddr   = getUseAddr(addr)
    def bankId    = getBankId(addr)
    def offset    = getOffset(addr)
    def dirBank   = getDirBank(addr)
    def llcTag    = getLlcTag(addr)
    def llcSet    = getLlcSet(addr)
    def sfTag     = getSfTag(addr)
    def sfSet     = getSfSet(addr)
    def posTag    = getPosTag(addr)
    def posSet    = getPosSet(addr)

    def getDataId = Mux(offset(offsetBits-1), "b10".U, "b00".U)

    def isToLAN(selfCI: UInt)  = ci === selfCI
    def isToBBN(selfCI: UInt)  = ci =/= selfCI

    def catByX(bank: UInt, tag: UInt, tagBits: Int, set: UInt, setBits: Int, dirBank: UInt, offset: UInt = 0.U(offsetBits.W)) = {
      require(bank.getWidth    == bankBits,    s"bankBits:    ${bank.getWidth} =/= ${bankBits}")
      require(tag.getWidth     == tagBits,     s"tagBits:     ${tag.getWidth} =/= ${tagBits}")
      require(set.getWidth     == setBits,     s"setBits:     ${set.getWidth} =/= ${setBits}")
      require(dirBank.getWidth == dirBankBits, s"dirBankBits: ${dirBank.getWidth} =/= ${dirBankBits}")
      require(offset.getWidth  == offsetBits,  s"offsetBits:  ${offset.getWidth} =/= ${offsetBits}")
      val useAddr_ = Cat(tag, set, dirBank)
      val addr_ = Cat(useAddr_(useAddrBits-1, bankId_lo-offsetBits), bank, useAddr_(bankId_lo-offsetBits-1, 0), offset)
      require(addr_.getWidth   == addrBits)
      addr := addr_
    }
    def catPoS(bank: UInt, tag: UInt, set: UInt, dirBank: UInt, offset: UInt = 0.U(offsetBits.W)) = catByX(bank, tag, posTagBits, set, posSetBits, dirBank, offset)
    def catLLC(bank: UInt, tag: UInt, set: UInt, dirBank: UInt, offset: UInt = 0.U(offsetBits.W)) = catByX(bank, tag, llcTagBits, set, llcSetBits, dirBank, offset)
    def catSF (bank: UInt, tag: UInt, set: UInt, dirBank: UInt, offset: UInt = 0.U(offsetBits.W)) = catByX(bank, tag, sfTagBits,  set, sfSetBits,  dirBank, offset)

    def cat(bank: UInt, tag: UInt, set: UInt, dirBank: UInt): Unit = if(addrType == "llc") catLLC(bank, tag, set, dirBank) else catSF(bank, tag, set, dirBank)
    def tag: UInt = if(addrType == "llc") llcTag else sfTag
    def set: UInt = if(addrType == "llc") llcSet else sfSet
  }
}

class Addr(dirType: String = "llc")(implicit p: Parameters) extends DJBundle with HasAddr {
  override def addrType: String = dirType
}

/*
 * PoS:
 * HasPosIndex -> PosIndex -> HasPackPosIndex ->
 */
trait HasPosIndex extends DJBundle { this: DJBundle =>
  val set = UInt(posSetBits.W)
  val way = UInt(posWayBits.W)

  def idxMatch(i: Int, j: Int) = set === i.U & way === j.U
  def getLLCTxnID(dirBank: Int) = Cat(dirBank.U, set, way)
  def getLLCTxnID(dirBank: UInt) = Cat(dirBank, set, way)
}

class PosIndex(implicit p: Parameters) extends DJBundle with HasPosIndex

trait HasPackPosIndex extends DJBundle { this: DJBundle =>
  val pos = new PosIndex()
}

class PackPosIndex(implicit p: Parameters) extends DJBundle with HasPackPosIndex

/*
 * LLCTxnID:
 * HasLLCTxnID -> LLCTxnID -> HasPackLLCTxnID -> PackLLCTxnID
 */
trait HasLLCTxnID extends DJBundle { this: DJBundle =>
  val pos     = new PosIndex()
  val dirBank = UInt(dirBankBits.W)
  def get     = pos.getLLCTxnID(dirBank)
}

class LLCTxnID(implicit p: Parameters) extends DJBundle with HasLLCTxnID

trait HasPackLLCTxnID extends DJBundle { this: DJBundle =>
  val llcTxnID = new LLCTxnID()
}

class PackLLCTxnID(implicit p: Parameters) extends DJBundle with HasPackLLCTxnID

/*
 * Chi:
 * HasChi -> Chi -> HasPackChi -> PackChi
 */
trait HasChi { this: DJBundle with HasNodeId with HasChiChannel with HasChiOp
  with HasChiOrderAndExpCompAck with HasChiSnpField with HasChiSize =>
  // REQ
  val txnID     = UInt(ChiTxnIdBits.W)
  val memAttr   = new MemAttr()
  // SNP
  val fwdNID    = UInt(nodeIdBits.W)
  val fwdTxnID  = UInt(ChiFwdTxnIdBits.W)
  val retToSrc  = Bool()
  // Flag
  val toLAN     = Bool()
  val dataId    = UInt(2.W)

  def needSendDBID(sfHit: Bool = false.B) = isAtomic | (isWrite & !reqIs(WriteEvictOrEvict)) | (!sfHit & reqIs(WriteEvictOrEvict))

  def getNoC = Mux(toLAN, NocType.LAN, NocType.BBN)

  def getChiInst: ChiInst = {
    val inst = Wire(new ChiInst)
    inst.channel    := channel
    inst.fromLAN    := fromLAN
    inst.toLAN      := toLAN
    inst.opcode     := opcode
    inst.expCompAck := expCompAck
    inst
  }
}

class Chi(implicit p: Parameters) extends DJBundle with HasNodeId with HasChiChannel
  with HasChiOp with HasChiOrderAndExpCompAck with HasChiSnpField with HasChiSize with HasChi

trait HasPackChi { this: DJBundle => val chi = new Chi() }

class PackChi(implicit p: Parameters) extends DJBundle with HasPackChi
