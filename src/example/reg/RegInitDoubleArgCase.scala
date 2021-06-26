package example

import chisel._

class RegInitDoubleArgCase extends RawModule {
  val clk    = IO(Input(Clock()))
  val rst    = IO(Input(Reset()))
  val x_init = IO(Input(UInt(3.W)))
  val y_init = IO(Input(UInt(8.W)))
  val x_in   = IO(Input(UInt(3.W)))
  val y_in   = IO(Input(UInt(8.W)))
  val x_out  = IO(Output(UInt(3.W)))
  val y_out  = IO(Output(UInt(8.W)))

  val x  = Wire(UInt())
  val y  = Wire(UInt(8.W))
  val r1 = withClockAndReset(clk, rst) { RegInit(UInt(), x) } // width will be inferred to be 3
  val r2 = withClockAndReset(clk, rst) { RegInit(UInt(8.W), y) } // width is set to 8

  x := x_init
  y := y_init

  r1 := x_in
  r2 := y_in

  x_out := r1
  y_out := r2
}
