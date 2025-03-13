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


object Read_DCT_DMT {
  def readNoSnp: Seq[(UInt, UInt)] = Seq(
    (isReq | toLAN | opIs(ReadNoSnp) | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
    (isReq | toLAN | opIs(ReadNoSnp))                 -> (read | opcode(ReadNoSnp) | needDB),
  )

  def readNotSharedDirty: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
    // I I V
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOne | opcode(SnpNotSharedDirtyFwd) | needDB),
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOne | opcode(SnpNotSharedDirtyFwd) | needDB),
    // V I I
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // V V I
    (isReq | toLAN | opIs(ReadNotSharedDirty) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,

    // BBN
    // I I I
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadNotSharedDirty) | needDB | expCompAck | canBeNest),
    // I I V
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOne | opcode(SnpNotSharedDirtyFwd) | needDB),
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOne | opcode(SnpNotSharedDirtyFwd) | needDB),
    // V I I
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    // V V I
    (isReq | toBBN | opIs(ReadNotSharedDirty) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> error,
  )

  def readUnique: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
    // I I V
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    (isReq | toLAN | opIs(ReadUnique) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    // V I I
    (isReq | toLAN | opIs(ReadUnique) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toLAN | opIs(ReadUnique) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
    // V V I
    (isReq | toLAN | opIs(ReadUnique) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),

    // BBN
    // I I I
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadUnique) | needDB | expCompAck | canBeNest),
    // I I V
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest),
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    (isReq | toBBN | opIs(ReadUnique) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest), // Will SnpUniqueFwd(oth) when MakeReadUnique done without nest
    // V I I
    (isReq | toBBN | opIs(ReadUnique) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(ReadUnique) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest),
    // V V I
    (isReq | toBBN | opIs(ReadUnique) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest), // Will SnpMakeInvalid(oth) when MakeReadUnique done without nest
  )

  def makeReadUnique: Seq[(UInt, UInt)] = Seq(
    // LAN
    // I I I
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
    // I I V
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    // V I I
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> nothing,
    // V V I
    (isReq | toLAN | opIs(MakeReadUnique) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpMakeInvalid)),

    // BBN
    // I I I
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest),
    // I I V
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(SC) | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest),
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UC) | reqExpCompAck) -> nothing,
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(I)  | llcIs(UD) | reqExpCompAck) -> nothing,
    // I V I
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(UD) | llcIs(I)  | reqExpCompAck) -> (snpOth | opcode(SnpUniqueFwd) | needDB),
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(I)  | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest), // Will SnpUniqueFwd(oth) when MakeReadUnique done without nest
    // V I I
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(UD) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> error,
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(SC) | othIs(I)  | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB | expCompAck | canBeNest),
    // V V I
    (isReq | toBBN | opIs(MakeReadUnique) | srcIs(SC) | othIs(SC) | llcIs(I)  | reqExpCompAck) -> (read | opcode(MakeReadUnique) | needDB  | expCompAck | canBeNest), // Will SnpMakeInvalid(oth) when MakeReadUnique done without nest
  )
  
  def table: Seq[(UInt, UInt)] = readNoSnp ++ readNotSharedDirty ++ readUnique ++ makeReadUnique
}