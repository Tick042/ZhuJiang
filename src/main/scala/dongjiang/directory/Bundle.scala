package dongjiang.directory

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import dongjiang._
import dongjiang.bundle._

trait HasDirParam extends DJBundle { this: DJBundle =>
  def paramType: String

  def sets     : Int = if(paramType == "llc") llcSets           else sfSets
  def ways     : Int = if(paramType == "llc") djparam.llcWays   else djparam.sfWays
  def tagBits  : Int = if(paramType == "llc") llcTagBits        else sfTagBits
  def setBits  : Int = if(paramType == "llc") llcSetBits        else sfSetBits
  def nrMetas  : Int = if(paramType == "llc") 1                 else nrSfMetas
  def wayBits  : Int = log2Ceil(ways)
}

class DirParam(dirType: String)(implicit p: Parameters) extends DJBundle with HasDirParam {
  override def paramType: String = dirType
}

trait HasDirBaseMsg extends DJBundle { this: DJBundle with HasDirParam =>
  val wayOH   = UInt(ways.W)
  val hit     = Bool()
  val metaVec = Vec(nrMetas, new ChiState(paramType))
}

// Without Addr
trait HasDirMsg extends DJBundle { this: DJBundle =>
  val llc = new DJBundle with HasDirParam with HasDirBaseMsg {
    override def paramType: String = "llc"
  }
  val sf  = new DJBundle with HasDirParam with HasDirBaseMsg {
    override def paramType: String = "sf"
  }
}

// With Addr
class DirEntry(dirType: String)(implicit p: Parameters) extends DJBundle with HasDirParam with HasAddr {
  override def paramType: String = dirType
  override def addrType : String = dirType

  val wayOH     = UInt(ways.W)
  val hit       = Bool()
  val metaVec   = Vec(nrMetas, new ChiState(dirType))
  val uniqueOpt = if(dirType == "sf") Some(Bool()) else None
}

