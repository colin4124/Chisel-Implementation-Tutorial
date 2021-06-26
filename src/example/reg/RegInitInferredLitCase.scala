package example

import chisel._

class RegInitInferredLitCase extends RawModule {
  val clk   = IO(Input(Clock()))
  val rst   = IO(Input(Reset()))
  val x_in  = IO(Input(UInt(3.W)))
  val y_in  = IO(Input(UInt(8.W)))
  val x_out = IO(Output(UInt(3.W)))
  val y_out = IO(Output(UInt(8.W)))

  val x = withClockAndReset(clk, rst) { RegInit(5.U) } // width will be inferred to be 3
  val y = withClockAndReset(clk, rst) { RegInit(5.U(8.W)) } // width is set to 8

  x := x_in
  y := y_in
  x_out := x
  y_out := y
}
