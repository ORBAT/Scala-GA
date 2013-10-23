package scagen2.vm

import scagen2.vm.SimpleStack._

object Instruction {
  type NullaryFn = () => ItemType
  type UnaryFm = (ItemType) => ItemType
  type BinFn = (ItemType, ItemType) => ItemType
  type Operation = (Context) => Unit

  def unapply(i: Instruction): Option[(String, Operation)] = {
    Some((i.symbol, i.function))
  }

  /**
   * A function that takes a stack `s` and does `s.push(s.pop() + s.pop())`. The type ''(SimpleStack) => Unit''
   * is assigned to `Operation` for brevity.
   */
  val add: Instruction = InstructionTools.toInstruction("+", _ + _)
  /**
   * `s.push(s.pop() - s.pop())`
   */
  val sub: Instruction = InstructionTools.toInstruction("-", _ - _)
  /**
   * `s.push(s.pop() * s.pop())`
   */
  val mul: Instruction = InstructionTools.toInstruction("*", _ * _)
  /**
   * `s.push(s.pop() / s.pop())`
   */
  val div: Instruction = InstructionTools.toInstruction("/", _ / _)
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

  private[this] def sipImpl(c:Context) {
    val stack: SimpleStack = c.stack
    val b = stack.pop()
    val a = stack.pop()
    if(b > 0)
      c.skip(a.asInstanceOf[Int])
  }

  private[this] def sinImpl(c:Context) {
    val stack: SimpleStack = c.stack
    val b = stack.pop()
    val a = stack.pop()
    if(b < 0)
      c.skip(a.asInstanceOf[Int])
  }

  
  /**
   * Pops two values from the stack ( a b -- ), if `b` (i.e. the value last pushed) is >= 0 then skips `a`
   * instructions
   */
  val skipIfGTE0: Instruction = new Instruction("SIG", sipImpl)
  /**
   * Pops two values from the stack ( a b -- ), if `b` (i.e. the value last pushed) is < 0 then skips `a`
   * instructions
   */
  val skipIfLT0: Instruction = new Instruction("SIL", sinImpl)

}

/**
 * Instructions are the smallest executable building block in Scagen2. Instructions are comprised of an `Operation`
 * and a String symbol representation of the operation.
 *
 * They have no state themselves, but they rely on
 * SimpleStack to be mutable as seen in `Instruction.apply(SimpleStack)`
 * @param function A function that uses a [[scagen2.vm.Context]] to do calculations. See [[scagen2.vm.InstructionTools]] for some examples.
 * @param symbol The symbol for this Instruction.
 */
class Instruction(val symbol: String, val function: Instruction.Operation) {

  def apply(v1: Context) {
    function(v1)
  }

  override def toString = {
    symbol
  }
}


object InstructionTools {

  import Instruction.{Operation, NullaryFn, UnaryFm, BinFn}

  def pushGen(item: SimpleStack.ItemType): Instruction =
    new Instruction(item.formatted("%.4g"), (c: Context) => c.stack.push(item))

  def toInstruction(tup: (String, BinFn)): Instruction = {
    toInstruction(tup._1, tup._2)
  }

  def toInstruction(symbol: String, f: BinFn): Instruction = {
    new Instruction(symbol, toOpType(f))
  }

  /**
   * A function that when given the paremeter `item` returns a function that pushes `item` onto a stack `s`
   *
   */
  def toOpType(f: BinFn): Operation = {
    (c: Context) => {
      val s = c.stack
      val a = s.pop()
      val b = s.pop()
      s.push(f(a, b))
    }
  }

}