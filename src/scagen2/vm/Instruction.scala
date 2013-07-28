package scagen2.vm

object InstructionTools {
  /**
   * A function that when given the paremeter `item` returns a function that pushes `item` onto a stack `s`
   *
   */
  import SimpleStack.ItemType
  import Instruction.Operation
  def pushGen(item: SimpleStack.ItemType): (SimpleStack) => Unit = (s: SimpleStack) => s.push(item)

  def toOpType(f:(ItemType, ItemType) => ItemType): Operation = {
    (s: SimpleStack) => s.push(f(s.pop(), s.pop()))
  }
}

object Instruction {
  type Operation = (SimpleStack) => Unit
  val binaryOps: Seq[Operation] = {
    import math._
    val selfDefined: Seq[(SimpleStack) => Unit] = Seq(add, sub, mul, div)
    val ops: Seq[(SimpleStack) => Unit] = Seq[(Double, Double) => Double](atan2, hypot, max, min, pow) map {
      op => (s: SimpleStack) => s.push(op(s.pop(), s.pop()))
    }
    selfDefined ++ ops
  }
  val unaryOps : Seq[Operation] = {
    import math._
    Seq[(Double) => Double](abs, exp, log, sin, tan, cos, sinh, tanh, cosh, asin, atan, acos, exp, floor, ceil).map {
      op => (s: SimpleStack) => s.push(op(s.pop()))
    }
  }
  val stackOps       : Seq[Operation] = Seq(
                                          (s: SimpleStack) => s.drop()
                                          , (s: SimpleStack) => s.dup()
                                          , (s: SimpleStack) => s.over()
                                          , (s: SimpleStack) => s.rot()
                                          , (s: SimpleStack) => s.swap()
                                          , (s: SimpleStack) => s.clear()
                                        )
  val allInstructions: Seq[Operation] = {
    binaryOps ++ unaryOps ++ stackOps
  }
  /**
   * A function that takes a stack `s` and does `s.push(s.pop() + s.pop())`. The type ''(SimpleStack) => ()''
   * is assigned to `Operation` for brevity.
   */
  val add            : Operation      = (s: SimpleStack) => s.push(s.pop() + s.pop())
  /**
   * `s.push(s.pop() - s.pop())`
   */
  val sub            : Operation      = (s: SimpleStack) => s.push(s.pop() - s.pop())
  /**
   * `s.push(s.pop() * s.pop())`
   */
  val mul            : Operation      = (s: SimpleStack) => s.push(s.pop() * s.pop())
  /**
   * `s.push(s.pop() / s.pop())`
   */
  val div            : Operation      = (s: SimpleStack) => s.push(s.pop() / s.pop())
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
