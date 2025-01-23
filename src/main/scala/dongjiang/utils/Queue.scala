package dongjiang.utils

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._

class DecoupledQueue[T <: Data](gen:T) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
  })

  val q = Module(new Queue(gen, entries = 2, flow = false, pipe = false))

  // Solving enq ready timing problems
  val enqReadyReg = RegInit(true.B)
  switch(q.io.count) {
    is(2.U) { enqReadyReg := io.deq.ready }
    is(1.U) { enqReadyReg := io.deq.ready | !io.enq.valid }
    is(0.U) { enqReadyReg := true.B }
  }
  assert(!(enqReadyReg ^ q.io.enq.ready))

  // Solving deq valid timing problems
  val deqValidReg = RegInit(false.B)
  switch(q.io.count) {
    is(2.U) { deqValidReg := true.B }
    is(1.U) { deqValidReg := io.enq.valid | !io.deq.ready }
    is(0.U) { deqValidReg := io.enq.valid }
  }
  assert(!(deqValidReg ^ q.io.deq.valid))

  io.enq <> q.io.enq
  io.deq <> q.io.deq
  io.enq.ready := enqReadyReg
  io.deq.valid := deqValidReg
}


// For timing
object fastDecoupledQueue {
  def apply[T <: Bundle](in: DecoupledIO[T], out: DecoupledIO[T]): Unit = {
    val q = Module(new DecoupledQueue(chiselTypeOf(in.bits)))
    q.io.enq <> in
    q.io.deq <> out
  }

  def apply[T <: Bundle](in: DecoupledIO[T]): DecoupledIO[T] = {
    val q = Module(new DecoupledQueue(chiselTypeOf(in.bits)))
    q.io.enq <> in
    q.io.deq
  }
}
