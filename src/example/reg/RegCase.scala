package example

import chisel._

class RegCase extends RawModule {
  val clk = IO(Input(Clock()))
  val in  = IO(Input(UInt(3.W)))
  val out = IO(Output(UInt(3.W)))

  val delayReg = withClock(clk) { Reg(UInt(3.W)) }

  delayReg := in
  out := delayReg
}
