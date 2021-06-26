package example

import chisel._

class WireCase extends RawModule {
  val out1 = IO(Output(UInt(8.W)))
  val out2 = IO(Output(UInt(8.W)))

  val my_node = Wire(UInt(8.W))
  my_node := 10.U

  val init_node = WireInit(10.U(8.W))

  out1 := my_node
  out2 := init_node
}
