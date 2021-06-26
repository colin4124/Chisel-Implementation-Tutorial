package example

import chisel._

class WhichFruit extends RawModule {
  val clk    = IO(Input(Clock()))
  val rst    = IO(Input(Reset()))
  val sel    = IO(Input(Bool()))
  val apple  = IO(Input(Bool()))
  val cherry = IO(Input(Bool()))
  val water  = IO(Input(UInt(3.W)))
  val juice  = IO(Output(UInt(3.W)))

  val bowl = withClockAndReset(clk, rst) { RegInit(5.U) }
  val fruit = WireInit((sel & apple) | (~sel & cherry))
  bowl  := bowl + fruit 
  juice := bowl
}
