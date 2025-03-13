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


object Write {
  def writeNoSnpFull: Seq[(UInt, UInt)] = Seq(
    (isReq | toLAN | opIs(WriteNoSnpFull) | reqExpCompAck) -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
    (isReq | toLAN | opIs(WriteNoSnpFull))                 -> (wriOrAtm | opcode(WriteNoSnpFull) | needDB),
  )

  def writeNoSnpPtl: Seq[(UInt, UInt)] = Seq(
    (isReq | toLAN | opIs(WriteNoSnpPtl) | reqExpCompAck) -> (wriOrAtm | opcode(WriteNoSnpPtl) | needDB),
    (isReq | toLAN | opIs(WriteNoSnpPtl))                 -> (wriOrAtm | opcode(WriteNoSnpPtl) | needDB),
  )

  def writeBackFull: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toLAN | opIs(WriteBackFull) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,

    // BBN
    // I I I
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toBBN | opIs(WriteBackFull) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,
  )

  def writeCleanFull: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toLAN | opIs(WriteCleanFull) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,

    // BBN
    // I I I
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toBBN | opIs(WriteCleanFull) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,
  )

  def writeEvictOrEvict: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toLAN | opIs(WriteEvictOrEvict) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,

    // BBN
    // I I I
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(I) ) -> nothing,
    // I I V
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(SC)) -> nothing,
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(UC)) -> nothing,
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(I)  | llcIs(UD)) -> nothing,
    // I V I
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(UD) | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(I)  | othIs(SC) | llcIs(I) ) -> nothing,
    // V I I
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(UD) | othIs(I)  | llcIs(I) ) -> nothing,
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(SC) | othIs(I)  | llcIs(I) ) -> nothing,
    // V V I
    (isReq | toBBN | opIs(WriteEvictOrEvict) | srcIs(SC) | othIs(SC) | llcIs(I) ) -> nothing,
  )

  // Note: Size must be 6
  def writeUniqueFull: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> nothing, // It will send WriteNoSnp when get NCBWrData
    // I I V
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpMakeInvalid)),
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpMakeInvalid)),
    // V I I
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpMakeInvalid)),
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpMakeInvalid)),
    // V V I
    (isReq | toLAN | opIs(WriteUniqueFull) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpMakeInvalid)),

    // BBN
    // I I I
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // I I V
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> error,
    // I V I
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,
    // V I I
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // V V I
    (isReq | toBBN | opIs(WriteUniqueFull) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,
  )

  def writeUniquePtl: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> nothing, // It will send WriteNoSnp when get NCBWrData
    // I I V
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB),
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB),
    // V I I
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB),
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB),
    // V V I
    (isReq | toLAN | opIs(WriteUniquePtl) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpAll | opcode(SnpUnique) | retToSrc | needDB),

    // BBN
    // I I I
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // I I V
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> error,
    // I V I
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,
    // V I I
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // V V I
    (isReq | toBBN | opIs(WriteUniquePtl) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,
  )

  def table: Seq[(UInt, UInt)] = writeNoSnpFull ++ writeNoSnpPtl ++ writeBackFull ++ writeCleanFull ++ writeEvictOrEvict ++ writeUniqueFull ++ writeUniquePtl
}