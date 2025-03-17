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

class Issue(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    val task_s3     = Valid(new ChiTask with HasPosIndex)
    val RespTask_s4 = Valid(new ChiTask())

  })
  dontTouch(io)
  HardwareAssertion(!io.task_s3.valid)
  io <> DontCare

  /*
   * Reg and Wire declaration
   */

  /*
   * HardwareAssertion placePipe
   */
   HardwareAssertion.placePipe(Int.MaxValue-2)
}