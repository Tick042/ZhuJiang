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

class Issue(dirBank: Int)(implicit p: Parameters) extends DJModule {
  /*
   * IO declaration
   */
  val io = IO(new Bundle {
    // Configuration
    val config      = new DJConfigIO()
    // In
    val task_s3     = Flipped(Valid(new Bundle {
      val chi       = new ChiTask with HasAddr
      val pos       = new PosIndex()
      val dir       = new DirMsg()
      val code      = new TaskCode()
      val alrDeqDB  = Bool()
    }))
    // Out
    val commit_s4   = Valid(new DJBundle with HasLLCTxnID {
      val chi       = new ChiTask
      val dir       = new DirMsg()
      val ops       = new Operations()
      val alrDeqDB  = Bool()
    })
    val cmTask_s4   = Decoupled(new DJBundle with HasLLCTxnID {
      val chi       = new ChiTask with HasAddr
      val ops       = new Operations()
      val alrDeqDB  = Bool()
    })
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