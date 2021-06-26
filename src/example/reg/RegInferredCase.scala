package example

import chisel._

class RegInferredCase extends RawModule {
  val clk = IO(Input(Clock()))
  val rst = IO(Input(Reset()))
  val in  = IO(Input(UInt(3.W)))
  val out = IO(Output(UInt(3.W)))

  val delayReg = withClock(clk) { Reg(UInt()) }

  delayReg := in
  out := delayReg
}
