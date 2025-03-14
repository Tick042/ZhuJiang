package dongjiang.frontend.decode

import dongjiang._
import dongjiang.frontend._
import dongjiang.frontend.decode.Inst._
import dongjiang.frontend.decode.Code._
import dongjiang.bundle._
import dongjiang.bundle.ChiState._
import dongjiang.bundle.ChiChannel._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.SnpOpcode._
import zhujiang.chi._
import dongjiang.bundle.ChiState._
import chisel3._
import chisel3.util._


object Dataless {
  def makeUnique: Seq[(UInt, Seq[(UInt, UInt)])] = Seq(
    // LAN
    (isReq | toLAN | opIs(MakeUnique), Seq(
      // I I I
      (srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
      // I I V
      (srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
      (srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
      (srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
      // I V I
      (srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOth | opcode(SnpMakeInvalid)),
      (srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpOth | opcode(SnpMakeInvalid)),
      // V I I
      (srcIs(UD) | othIs(I)  | llcIs(I) ) -> error,
      (srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
      // V V I
      (srcIs(SC) | othIs(SC) | llcIs(I) ) -> (snpOth | opcode(SnpMakeInvalid)),
    )),
    // BBN
    (isReq | toBBN | opIs(MakeUnique), Seq(
      // I I I
      (srcIs(I)  | othIs(I)  | llcIs(I) ) -> (dataless | opcode(MakeUnique) | needDB | expCompAck | canBeNest),
      // I I V
      (isReq | toBBN | opIs(MakeUnique) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> (dataless | opcode(MakeUnique) | needDB | expCompAck | canBeNest),
      (isReq | toBBN | opIs(MakeUnique) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
      (isReq | toBBN | opIs(MakeUnique) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
      // I V I
      (isReq | toBBN | opIs(MakeUnique) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOth | opcode(SnpMakeInvalid)),
      (isReq | toBBN | opIs(MakeUnique) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> (dataless | opcode(MakeUnique) | needDB | expCompAck | canBeNest), // Will SnpMakeInvalid(oth) when MakeUnique done without nest
      // V I I
      (isReq | toBBN | opIs(MakeUnique) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> error,
      (isReq | toBBN | opIs(MakeUnique) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(MakeUnique) | needDB | expCompAck | canBeNest),
      // V V I
      (isReq | toBBN | opIs(MakeUnique) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> (dataless | opcode(MakeUnique) | needDB | expCompAck | canBeNest), // Will SnpMakeInvalid(oth) when MakeUnique done without nest
    ))
  )

  def evict: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(Evict) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toLAN | opIs(Evict) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(Evict) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toLAN | opIs(Evict) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,

    // BBN
    // I I I
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(Evict) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toBBN | opIs(Evict) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(Evict) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toBBN | opIs(Evict) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,
  )

  def cleanShared: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
    // I V I
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // if oth is UD, WriteNoSnpFull when SnpUnique Done
    (isReq | toLAN | opIs(CleanShared) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toLAN | opIs(CleanShared) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // if src is UD, WriteNoSnpFull when SnpUnique Done
    (isReq | toLAN | opIs(CleanShared) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toLAN | opIs(CleanShared) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,

    // BBN
    // I I I
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanShared) | canBeNest),
    // I I V
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> (dataless | opcode(CleanShared) | canBeNest),
    // I V I
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (dataless | opcode(CleanShared) | canBeNest),
    (isReq | toBBN | opIs(CleanShared) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toBBN | opIs(CleanShared) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanShared) | canBeNest),
    (isReq | toBBN | opIs(CleanShared) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toBBN | opIs(CleanShared) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,
  )


  def cleanInvalid: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
    // I V I
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // when SnpUnique done, send WriteBackFull to SN
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // when SnpUnique done, send WriteBackFull to SN
    // V I I
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // when SnpUnique done, send WriteBackFull to SN
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // when SnpUnique done, send WriteBackFull to SN
    // V V I
    (isReq | toLAN | opIs(CleanInvalid) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB), // when SnpUnique done, send WriteBackFull to SN

    // BBN
    // I I I
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // I I V
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // I V I
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // V I I
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // V V I
    (isReq | toBBN | opIs(CleanInvalid) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
  )


  def makeInvalid: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpAll | opcode(SnpMakeInvalid)),
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpAll | opcode(SnpMakeInvalid)),
    // V I I
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (snpAll | opcode(SnpMakeInvalid)),
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> (snpAll | opcode(SnpMakeInvalid)),
    // V V I
    (isReq | toLAN | opIs(MakeInvalid) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> (snpAll | opcode(SnpMakeInvalid)),

    // BBN
    // I I I
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // I I V
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // I V I
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // V I I
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
    // V V I
    (isReq | toBBN | opIs(MakeInvalid) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> (dataless | opcode(CleanInvalid) | canBeNest),
  )


  def table: Seq[(UInt, Seq[(UInt, UInt)])] = makeUnique // ++ evict ++ cleanShared ++ cleanInvalid ++ makeInvalid
}