package chisel

import example._

object Main extends App {
  val modules = List(
    () => new Mux2,
    () => new WhichFruit,
    () => new WireCase,
    () => new RegCase,
    () => new RegInferredCase,
    () => new RegInitCase,
    () => new RegInitDoubleArgCase,
    () => new RegInitInferredLitCase,
    () => new RegInitInferredNonLitCase,
    () => new RegNextCase,
    () => new RegNextInitCase,
  )

  modules foreach { m =>
    Driver.execute(m, args(0))
  }
}
