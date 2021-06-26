package example

import chisel._

class RegNextInitCase extends RawModule {
  val clk = IO(Input(Clock()))
  val rst = IO(Input(Reset()))
  val in  = IO(Input(UInt(3.W)))
  val out = IO(Output(UInt(3.W)))

  val delayReg = withClockAndReset(clk, rst) { RegNext(in, 0.U(3.W)) }

  out := delayReg
}
