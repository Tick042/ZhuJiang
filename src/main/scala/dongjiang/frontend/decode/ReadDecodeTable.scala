package dongjiang.frontend.decode

import dongjiang._
import dongjiang.frontend._
import dongjiang.frontend.decode.Inst._
import dongjiang.frontend.decode.Code._
import dongjiang.frontend.decode.DecodeCHI._
import dongjiang.bundle._
import dongjiang.bundle.ChiChannel._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.SnpOpcode._
import zhujiang.chi._
import chisel3._
import chisel3.util._



object Read_DCT_DMT {
//  def readNoSnp: Seq[(UInt, UInt)] = Seq(
//    (isReq | toLAN | opIs(ReadNoSnp) | reqExpCompAck) -> (read | opcode(ReadNoSnp)),
//    (isReq | toLAN | opIs(ReadNoSnp))                 -> (read | opcode(ReadNoSnp) | needDB),
//  )

  // ReadNotSharedDirty To LAN
  // TODO: SnpXFwd to SC
  def readNotSharedDirty_lan: (UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))]) = (toLAN | reqIs(ReadNotSharedDirty) | expCompAck, Seq(
    // I I I
    (srcIs(I)  | othIs(I)  | llcIs(I) ) -> (read(ReadNoSnp), Seq(noTaskResp -> wriSRC(UD))),
    // I I UC
    (srcIs(I)  | othIs(I)  | llcIs(UC)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD) | wriLLC(I)))),
    // I I UD
    (srcIs(I)  | othIs(I)  | llcIs(UD)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriLLC(I)))),
    // I SC I
    (srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpOne(SnpNotSharedDirtyFwd), Seq(
      (rspIs(SnpRespFwded)     | resp(SC) | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (datIs(SnpRespDataFwded) | resp(SC) | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (rspIs(SnpRespFwded)     | resp(I)  | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (datIs(SnpRespDataFwded) | resp(I)  | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (rspIs(SnpResp)          | resp(I))              -> (secRead(ReadNoSnp) | secNeedDB | waitSecDone | cmtDat(CompData) | resp(SC) | wriSRC(SC)), // SC SC  I // No Fwd
    )),
    // I UD I
    (srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOne(SnpNotSharedDirtyFwd) | needDB, Seq(
      (rspIs(SnpRespFwded)     | resp(SC)    | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (datIs(SnpRespDataFwded) | resp(SC)    | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (rspIs(SnpRespFwded)     | resp(I)     | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (datIs(SnpRespDataFwded) | resp(I)     | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (datIs(SnpRespDataFwded) | resp(SC_PD) | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC) | secWriOrAtm(WriteNoSnpFull)), // SC SC I
      (datIs(SnpRespDataFwded) | resp(I_PD)  | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)  | secWriOrAtm(WriteNoSnpFull)), // SC I  I
      (rspIs(SnpResp)          | resp(I))                 -> (secRead(ReadNoSnp) | waitSecDone | wriSRC(UD) | wriOTH(I)), // UD I  I // No Fwd
    )),
    // SC I I
    (srcIs(SC) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // UD I I
    (srcIs(UD) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // SC SC I
    (srcIs(SC) | othIs(SC) | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
  ))


  // ReadNotSharedDirty To BBN
  def readNotSharedDirty_bbn: (UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))]) = (toBBN | reqIs(ReadNotSharedDirty) | expCompAck, Seq(
    // I I I
    (srcIs(I)  | othIs(I)  | llcIs(I) ) -> (read(ReadNotSharedDirty) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | respIs(SC))    -> (cmtDat(CompData) | resp(SC)    | wriSRC(SC)), // SC I I
      (datIs(CompData) | respIs(UC))    -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD)), // UD I I
      (datIs(CompData) | respIs(UD_PD)) -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD)), // UD I I
    )),
    // I I SC
    (srcIs(I)  | othIs(I)  | llcIs(SC)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(SC) | wriSRC(SC) | wriLLC(I)))),
    // I I UC
    (srcIs(I)  | othIs(I)  | llcIs(UC)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UC) | wriSRC(UD) | wriLLC(I)))),
    // I I UD
    (srcIs(I)  | othIs(I)  | llcIs(UD)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriLLC(I)))),
    // I SC I
    (srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpOth(SnpUnique) | retToSrc | needDB, Seq(
      (datIs(SnpRespData)  | resp(I))   -> (cmtDat(CompData)  | resp(SC) | wriSRC(SC) | wriOTH(SC)), // SC I I
      (rspIs(SnpResp)      | resp(I))   -> (secRead(ReadUnique) | secTaskECA | secNeedDB | secCanNest | waitSecDone | cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)),  // UD I I
    )),
    // I UD I
    (srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOne(SnpNotSharedDirtyFwd) | needDB, Seq(
      (rspIs(SnpRespFwded)     | resp(SC)    | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (datIs(SnpRespDataFwded) | resp(SC)    | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC)), // SC SC I
      (rspIs(SnpRespFwded)     | resp(I)     | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (datIs(SnpRespDataFwded) | resp(I)     | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)),  // SC I  I
      (datIs(SnpRespDataFwded) | resp(SC_PD) | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(SC) | secWriOrAtm(WriteNoSnpFull)), // SC SC I
      (datIs(SnpRespDataFwded) | resp(I_PD)  | fwdIs(SC)) -> (wriSRC(SC) | wriOTH(I)  | secWriOrAtm(WriteNoSnpFull)), // SC I  I
      (rspIs(SnpResp)          | resp(I))                 -> (secRead(ReadUnique) | secTaskECA | secNeedDB | secCanNest | waitSecDone | cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)),  // UD I I
    )),
    // SC I I
    (srcIs(SC) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // UD I I
    (srcIs(UD) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // SC SC I
    (srcIs(SC) | othIs(SC) | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
  ))


  // ReadUnique To LAN
  def readUnique_lan: (UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))]) = (toLAN | reqIs(ReadUnique) | expCompAck, Seq(
    // I I I
    (srcIs(I)  | othIs(I)  | llcIs(I) ) -> (read(ReadNoSnp), Seq(noTaskResp -> wriSRC(UD))),
    // I I UC
    (srcIs(I)  | othIs(I)  | llcIs(UC)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD) | wriLLC(I)))),
    // I I UD
    (srcIs(I)  | othIs(I)  | llcIs(UD)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriLLC(I)))),
    // I SC I
    (srcIs(I)  | othIs(SC) | llcIs(I) ) -> (snpOth(SnpUniqueFwd), Seq(
      (rspIs(SnpRespFwded) | resp(I) | fwdIs(UC)) -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (rspIs(SnpResp)      | resp(I))             -> (secRead(ReadNoSnp) | waitSecDone | wriSRC(UD) | wriOTH(I)), // UD I  I // No Fwd
    )),
    // I UD I
    (srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOth(SnpUniqueFwd) | needDB, Seq(
      (rspIs(SnpRespFwded) | resp(I)  | fwdIs(UC))    -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (rspIs(SnpRespFwded) | resp(I)  | fwdIs(UD_PD)) -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (datIs(SnpRespData)  | resp(I_PD))              -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)),   // UD I I
      (rspIs(SnpResp)      | resp(I))                 -> (secRead(ReadNoSnp) | waitSecDone | wriSRC(UD) | wriOTH(I)), // UD I  I // No Fwd
    )),
    // SC I I
    (srcIs(SC) | othIs(I)  | llcIs(I) ) -> (read(ReadNoSnp), Seq(noTaskResp -> wriSRC(UD))),
    // UD I I
    (srcIs(UD) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // SC SC I
    (srcIs(SC) | othIs(SC) | llcIs(I) ) -> (snpOth(SnpUniqueFwd), Seq(
      (rspIs(SnpRespFwded) | resp(I) | fwdIs(UC)) -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (rspIs(SnpResp)      | resp(I))             -> (secRead(ReadNoSnp) | waitSecDone | wriSRC(UD) | wriOTH(I)), // UD I  I // No Fwd
    )),
  ))

  // ReadUnique To BBN
  def readUnique_bbn: (UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))]) = (toBBN | reqIs(ReadUnique) | expCompAck, Seq(
    // I I I
    (srcIs(I)  | othIs(I)  | llcIs(I) ) -> (read(ReadUnique) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | respIs(UC))    -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD)), // UD I I
      (datIs(CompData) | respIs(UD_PD)) -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD)), // UD I I
    )),
    // I I SC
    (srcIs(I)  | othIs(I)  | llcIs(SC)) -> (read(ReadUnique) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | respIs(UC))    -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD) | wriLLC(I)), // UD I I
      (datIs(CompData) | respIs(UD_PD)) -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriLLC(I)), // UD I I
    )),
    // I I UC
    (srcIs(I)  | othIs(I)  | llcIs(UC)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD) | wriLLC(I)))),
    // I I UD
    (srcIs(I)  | othIs(I)  | llcIs(UD)) -> (nothingTODO, Seq(noTaskResp -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriLLC(I)))),
    // I SC I
    (srcIs(I)  | othIs(SC) | llcIs(I) ) -> (read(ReadUnique) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | resp(UC))      -> (secSnpOth(SnpMakeInvalid) | waitSecDone | cmtDat(CompData) | resp(UC)    | wriSRC(UD) | wriOTH(I)), // UD I I
      (datIs(CompData) | resp(UC_PD))   -> (secSnpOth(SnpMakeInvalid) | waitSecDone | cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)), // UD I I
    )),
    // I UD I
    (srcIs(I)  | othIs(UD) | llcIs(I) ) -> (snpOth(SnpUniqueFwd) | needDB, Seq(
      (rspIs(SnpRespFwded) | resp(I) | fwdIs(UC))     -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (rspIs(SnpRespFwded) | resp(I) | fwdIs(UD_PD))  -> (wriSRC(UD) | wriOTH(I)), // UD I I
      (datIs(SnpRespData)  | resp(I_PD))              -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)), // UD I I
      (rspIs(SnpResp)      | resp(I))                 -> (secRead(ReadUnique) | secTaskECA | canNest | waitSecDone | cmtDat(CompData) | resp(UD_PD) | wriSRC(UD) | wriOTH(I)), // UD I  I // No Fwd
    )),
    // SC I I
    (srcIs(SC) | othIs(I)  | llcIs(I) ) -> (read(ReadUnique) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | respIs(UC))    -> (cmtDat(CompData) | resp(UC)    | wriSRC(UD)), // UD I I
      (datIs(CompData) | respIs(UD_PD)) -> (cmtDat(CompData) | resp(UD_PD) | wriSRC(UD)), // UD I I
    )),
    // UD I I
    (srcIs(UD) | othIs(I)  | llcIs(I) ) -> (nothingTODO, Seq(noTaskResp -> error)),
    // SC SC I
    (srcIs(SC) | othIs(SC) | llcIs(I) ) -> (read(ReadUnique) | needDB | canNest | taskECA, Seq(
      (datIs(CompData) | respIs(UC))    -> (secSnpOth(SnpMakeInvalid) | waitSecDone | cmtDat(CompData) | resp(UC)    | wriSRC(UD)), // UD I I
      (datIs(CompData) | respIs(UD_PD)) -> (secSnpOth(SnpMakeInvalid) | waitSecDone | cmtDat(CompData) | resp(UD_PD) | wriSRC(UD)), // UD I I
    )),
  ))


  // readNoSnp ++ readOnce ++ readNotSharedDirty ++ readUnique ++ makeReadUnique
  def table: Seq[(UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))])] = Seq(readNotSharedDirty_lan, readNotSharedDirty_bbn, readUnique_lan, readUnique_bbn)
  def table_lan: Seq[(UInt, Seq[(UInt, (UInt, Seq[(UInt, UInt)]))])] = Seq(readNotSharedDirty_lan, readUnique_lan)
}