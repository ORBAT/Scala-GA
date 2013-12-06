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
  val add = InstructionTools.toInstruction("+", _ + _)
  /**
   * `s.push(s.pop() - s.pop())`
   */
  val sub = InstructionTools.toInstruction("-", _ - _)
  /**
   * `s.push(s.pop() * s.pop())`
   */
  val mul = InstructionTools.toInstruction("*", _ * _)
  /**
   * `s.push(s.pop() / s.pop())`
   */
  val div = InstructionTools.toInstruction("/", _ / _)
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
  val skipIfGTE0 = new Instruction("SIG", sipImpl)
  /**
   * Pops two values from the stack ( a b -- ), if `b` (i.e. the value last pushed) is < 0 then skips `a`
   * instructions
   */
  val skipIfLT0 = new Instruction("SIL", sinImpl)

}

/**
 * Instructions are the smallest executable building block in Scagen2. Instructions are comprised of an `Operation`
 * and a String symbol representation of the operation.
 *
 * They have no state themselves, but they rely on  SimpleStack to be mutable
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

  def pushGen(item: SimpleStack.ItemType) =
    new Instruction(item.formatted("%.4g"), (c: Context) => c.stack.push(item))

  def binFnTupToInstruction(tup: (String, BinFn)): Instruction = {
    toInstruction(tup._1, tup._2)
  }

  def toInstruction(symbol: String, f: BinFn): Instruction = {
    new Instruction(symbol, toOpType(f))
  }

  def toInstruction(symbol: String, f: UnaryFm): Instruction = {
    new Instruction(symbol, toOpType(f))
  }


  /**
   * Takes a binary function `f` and returns an Operation that pops two items off the stack and applies `f` to them.
   * The first parameter to `f` will be the first one popped.
   * @param f The binary function
   * @return
   */
  def toOpType(f: BinFn): Operation = {
    (c: Context) => {
      val s = c.stack
      s.push(f(s.pop(), s.pop()))
    }
  }

  def toOpType(f:UnaryFm): Operation = {
    (c: Context) => {
      val s = c.stack
      s.push(f(s.pop))
    }
  }

}