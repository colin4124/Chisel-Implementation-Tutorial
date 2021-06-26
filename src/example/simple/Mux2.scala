package example

import chisel._

class Mux2 extends RawModule {
  val sel = IO(Input(UInt(1.W)))
  val in0 = IO(Input(UInt(1.W)))
  val in1 = IO(Input(UInt(1.W)))
  val out = IO(Output(UInt(1.W)))
  out := (sel & in1) | (~sel & in0)
}
