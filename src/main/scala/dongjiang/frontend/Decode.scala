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
import zhujiang.chi.RspOpcode._

class Decode(dirBank: Int)(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    val task_s2     = Flipped(Valid(new ChiTask with HasPosIndex with HasDCID))
    val respDir_s3  = Input(new DJBundle {
      val llc       = new DirEntry("llc")
      val sf        = new DirEntry("sf")
    })
    val task_s3     = Valid(new ChiTask with HasPosIndex with HasDCID with HasDirMsg)
  })
  dontTouch(io)
  io <> DontCare
  HardwareAssertion(!io.task_s2.valid)

  /*
   * Reg and Wire declaration
   */


  /*
   * HardwareAssertion placePipe
   */
   HardwareAssertion.placePipe(Int.MaxValue-2)
}