package scagen2.vm

object InstructionTools {
  /**
   * A function that when given the paremeter `item` returns a function that pushes `item` onto a stack `s`
   *
   */

  import SimpleStack.ItemType
  import Instruction.Operation

  def pushGen(item: SimpleStack.ItemType): Operation = (s: SimpleStack) => s.push(item)

  type BinFn = (ItemType, ItemType) => ItemType

  def toOpType(f: BinFn): Operation = {
    (s: SimpleStack) => {
      val a = s.pop()
      val b = s.pop()
      s.push(f(a, b))
    }
  }

  val basicMathFns: Seq[(String, BinFn)] = Seq((" + ", _ + _)
                                                , (" - ", _ - _)
                                                , (" * ", _ * _)
                                                , (" / ", _ / _))
  val advMathFns  : Seq[(String, BinFn)] = Seq((" atan2 ", math.atan2 _)
                                                , (" hypot ", math.hypot _)
                                                , (" max ", math.max _)
                                                , (" min ", math.min _)
                                                , (" pow ", math.pow _))
  val binaryFns                          = basicMathFns ++ advMathFns

}

object Instruction {
  type Operation = (SimpleStack) => Unit
  val binaryOps      : Seq[Operation] = {
    import math._
    val selfDefined: Seq[Operation] = Seq(add, sub, mul, div)
    val ops: Seq[Operation] = Seq[(Double, Double) => Double](atan2, hypot, max, min, pow) map {
      op => (s: SimpleStack) => s.push(op(s.pop(), s.pop()))
    }
    selfDefined ++ ops
  }
  val unaryOps       : Seq[Operation] = {
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
   * A function that takes a stack `s` and does `s.push(s.pop() + s.pop())`. The type ''(SimpleStack) => Unit''
   * is assigned to `Operation` for brevity.
   */
  val add            : Operation      = InstructionTools.toOpType(_ + _)
  /**
   * `s.push(s.pop() - s.pop())`
   */
  val sub            : Operation      = InstructionTools.toOpType(_ - _)
  /**
   * `s.push(s.pop() * s.pop())`
   */
  val mul            : Operation      = InstructionTools.toOpType(_ * _)
  /**
   * `s.push(s.pop() / s.pop())`
   */
  val div            : Operation      = InstructionTools.toOpType(_ / _)
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
