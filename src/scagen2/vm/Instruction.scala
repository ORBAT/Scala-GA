package scagen2.vm

import scagen2.vm.SimpleStack._

object Instruction {
  type NullaryFn = () => ItemType
  type UnaryFm = (ItemType) => ItemType
  type BinFn = (ItemType, ItemType) => ItemType
  type Operation = (SimpleStack) => Unit

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
}

/**
 * Instructions are the smallest executable building block in Scagen2. Instructions are basically
 * comprised of an Operation and a String symbol representation.
 *
 * They have no state themselves, but they rely on
 * SimpleStack to be mutable as seen in `Instruction.apply(SimpleStack)`
 * @param fn A function that takes a `SimpleStack` and does something that has side effects. For example
 *           `(s: SimpleStack) => s.push(s.pop() * 2)` is a valid Operation
 * @param instructionSymbol
 */
class Instruction(instructionSymbol: String, fn: Instruction.Operation) {
  val symbol   = instructionSymbol
  val function = fn

  def apply(v1: SimpleStack) {
    fn(v1)
  }

  override def toString() = {
    instructionSymbol
  }
}


object InstructionTools {

  import Instruction.{Operation, NullaryFn, UnaryFm, BinFn}

  def pushGen(item: SimpleStack.ItemType): Instruction =
    new Instruction(item.formatted("%.4g"), (s: SimpleStack) => s.push(item))

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
    (s: SimpleStack) => {
      val a = s.pop()
      val b = s.pop()
      s.push(f(a, b))
    }
  }

  val basicMathFns: Seq[(String, BinFn)] = Seq(("+", _ + _)
                                                , ("-", _ - _)
                                                , ("*", _ * _)
                                                , ("/", _ / _))

  val advMathFns: Seq[(String, BinFn)] = Seq(("atan2", math.atan2 _)
                                              , ("hypot", math.hypot _)
                                              , ("max", math.max _)
                                              , ("min", math.min _)
                                              , ("pow", math.pow _))
  val binaryFns: Seq[(String, BinFn)] = (basicMathFns ++ advMathFns)
  val binaryInstructions = binaryFns.map(toInstruction(_))

}