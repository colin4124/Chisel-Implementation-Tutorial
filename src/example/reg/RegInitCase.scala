package example

import chisel._

class RegInitCase extends RawModule {
  val clk_1   = IO(Input(Clock()))
  val rst_1   = IO(Input(Reset()))
  val clk_2   = IO(Input(Clock()))
  val rst_2   = IO(Input(AsyncNegReset()))
  val clk_3   = IO(Input(Clock()))
  val rst_3   = IO(Input(AsyncPosReset()))
  val foo     = IO(Input(UInt(3.W)))
  val bar     = IO(Input(UInt(3.W)))
  val cat     = IO(Input(UInt(3.W)))
  val foo_out = IO(Output(UInt(3.W)))
  val bar_out = IO(Output(UInt(3.W)))
  val cat_out = IO(Output(UInt(3.W)))

  val reg_foo = withClockAndReset(clk_1, rst_1) { RegInit(0.U(3.W)) }
  val reg_bar = withClockAndReset(clk_2, rst_2) { RegInit(0.U(3.W)) }
  val reg_cat = withClockAndReset(clk_3, rst_3) { RegInit(0.U(3.W)) }

  reg_foo := foo
  reg_bar := bar
  reg_cat := cat
  foo_out := reg_foo
  bar_out := reg_bar
  cat_out := reg_cat
}
