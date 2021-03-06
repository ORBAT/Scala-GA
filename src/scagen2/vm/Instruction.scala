package scagen2.vm

object InstructionTools {
  /**
   * A function that when given the paremeter `item` returns a function that pushes `item` onto a stack `s`
   *
   */
  val pushGen = (item: SimpleStack.ItemType) => (s: SimpleStack) => s.push(item)
}

object Instruction {
  type OpType = (SimpleStack) => Unit
  val allInstructions: Seq[OpType] = {
    import SimpleStack.ItemType
    val binaryOps: Seq[OpType] = {
      import math._
      val selfDefined: Seq[(SimpleStack) => Unit] = Seq(add, sub, mul, div)
      val ops: Seq[(SimpleStack) => Unit] = Seq[(Double, Double) => Double](atan2, hypot, max, min, pow) map {
        op => (s: SimpleStack) => s.push(op(s.pop(), s.pop()))
      }
      selfDefined ++ ops
    }
    val unaryOps: Seq[OpType] = {
      import math._
      Seq[(Double) => Double](abs, exp, log, sin, tan, cos, sinh, tanh, cosh, asin, atan, acos, exp, floor, ceil).map {
        op => (s: SimpleStack) => s.push(op(s.pop))
      }
    }
    binaryOps ++ unaryOps
  }
  /**
   * A function that takes a stack `s` and does `s.push(s.pop() + s.pop())`. The type ''(SimpleStack) => ()''
   * is assigned to `OpType` for brevity.
   */
  val add                          = (s: SimpleStack) => s.push(s.pop() + s.pop())
  /**
   * `s.push(s.pop() - s.pop())`
   */
  val sub                          = (s: SimpleStack) => s.push(s.pop() - s.pop())
  /**
   * `s.push(s.pop() * s.pop())`
   */
  val mul                          = (s: SimpleStack) => s.push(s.pop() * s.pop())
  /**
   * `s.push(s.pop() / s.pop())`
   */
  val div                          = (s: SimpleStack) => s.push(s.pop() / s.pop())
  /*  /**
     * `s.push(math.abs(s.pop()))`
     */
    val abs = (s: SimpleStack) => s.push(math.abs(s.pop()))
    /**
     * `s.push(math.exp(s.pop()))`
     */
    val exp = (s: SimpleStack) => s.push(math.exp(s.pop()))
    /**
     * `s.push(math.exp(s.pop()))`
     */
    val log = (s: SimpleStack) => s.push(math.log(s.pop()))
    /**
     * `s.push(math.sin(s.pop()))`
     */
    val sin = (s: SimpleStack) => s.push(math.sin(s.pop()))
    /**
     * `s.push(math.tans.pop()))`
     */
    val tan = (s: SimpleStack) => s.push(math.tan(s.pop()))*/
}
