package dongjiang.utils

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class Shift(latency: Int)(implicit p: Parameters) extends Bundle {
  val s = UInt(latency.W)

  def input(fire: Bool) = {
    if (latency == 1) {
      this.s := fire
    } else {
      this.s := Cat(fire, s(latency - 1, 1))
    }
  }
  def isValid = s(0)
}