package example

import chisel._

class RegNextCase extends RawModule {
  val clk = IO(Input(Clock()))
  val in  = IO(Input(UInt(3.W)))
  val out = IO(Output(UInt(3.W)))

  val delayReg = withClock(clk) { RegNext(in) }

  out := delayReg
}
