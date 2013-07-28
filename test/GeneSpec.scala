import org.scalatest.FlatSpec
import scagen2.vm._
import scagen2.vm.InstructionTools._
import scagen2.vm.SimpleStack._
import scala.annotation.tailrec

class GeneSpec extends FlatSpec {

  import InstructionTools.binaryFns
  import SimpleStack.ItemType

  def numbersToPushes(values: Seq[Double]): Seq[(SimpleStack) => Unit] = {
    values map { value => InstructionTools.pushGen(value) }
  }

  /**
   * Chains binary operators together for testing. E.g. if your `numbersStream` gives only odd numbers and you
   * have the binary operations `Seq(/, *, -, +)` and their character representations
   *
   * "((((3.0 / 1.0)  *  5.00)   -  7.00)   +  9.00) "
   *@param numbersStream
   * @param stringFnPairs
   * @return
   */
  def giveResult(numbersStream: Stream[ItemType], stringFnPairs: Seq[(String, BinFn)]): (String, ItemType) = {
    def inner(fnPairsTail: Seq[(String, BinFn)], accum: ItemType,
              doubles: Stream[ItemType], strAcc: String): (String, ItemType) =
      fnPairsTail match {
        case (fnName, fn) :: restOfFns => {
          val doubleHead = doubles.head
          val newFnString = s" $fnName ${doubleHead.formatted("%.3g")}) "
          inner(restOfFns, fn(accum, doubleHead), doubles.tail, s"$strAcc$newFnString")
        }

        case _ => (strAcc, accum)
      }
    val sndPushed = numbersStream.head
    val fstPushed = numbersStream.tail.head
    val (firstFnSymbol, firstFn) = stringFnPairs.head
    inner(fnPairsTail = stringFnPairs.tail, accum = firstFn(fstPushed, sndPushed), doubles = numbersStream.tail.tail,
           strAcc = s"(((($fstPushed$firstFnSymbol$sndPushed)")
  }

  abstract class TestBinOpGene(opSymbol: String, op: (ItemType, ItemType) => ItemType) {
    val values = Seq(2.9, 4.7)
    assert(values.length > 1)
    val pushes        : Seq[Instruction.Operation] = numbersToPushes(values)
    val asOpType      : (SimpleStack) => Unit      = InstructionTools.toOpType(op)
    val expectedResult: ItemType                   = {
      // keep only the two last pushed items.
      val lastTwo = values.drop(values.length - 2).reverse
      val res = op(lastTwo(0), lastTwo(1))
      println(s"lastTwo ${lastTwo.mkString(", ")}\n${lastTwo(0) }${opSymbol }${lastTwo(1)} = ${res}")
      res
    }
    val gene                                       = new Gene(pushes :+ asOpType)
    val maxTolerance                               = 0.001
  }

  behavior of "A Gene"

  it should "handle all simple binary ops during execute()" in new {
    binaryFns.foreach(tuple =>
      new TestBinOpGene(tuple._1, tuple._2) {
        val execute = gene.execute()
        val absDiff = math.abs(execute - expectedResult)
        assert(absDiff < maxTolerance, s"BinOpGene returned ${execute}, diff to expected result (${expectedResult}) " +
                                       s"was ${absDiff}")
      })
  }

  it should "execute() when doing binary ops in a chain" in new {

    import Instruction.Operation

    // TODO: create instructions to the tune of 2 4 + 6 - 8 * 10 /
    val (opsAsString: String, longOps: List[Operation]) = {
      val step: ItemType = 1.79
      val steppedDoubles = Stream.iterate(0.3901)(_ + step)
      val values: Iterator[ItemType] = steppedDoubles.toIterator
      import SimpleStack.ItemType
      import InstructionTools._

      def funToPushAndOp(value: ItemType, x: (ItemType, ItemType) => ItemType): Seq[Operation] = {
        Seq(pushGen(value), toOpType(x))
      }

      // generates a nice String representation of the thing we're trying to do as well
      @tailrec def genOperations(ops: List[(String, BinFn)]
                                 , instrAccum: List[Operation]
                                 , strAccum: String): (String, List[Operation]) = ops.reverse match {
        case (symbol, op) :: rest => {
          val nextVal = values.next()
          genOperations(rest, instrAccum ++ funToPushAndOp(nextVal, op), s"${strAccum} $nextVal $symbol")
        }
        case _ => (strAccum, instrAccum)
      }

      val firstValue = values.next()
      val (stringRep, ops) = genOperations(basicMathFns.toList, List(), s"$firstValue")
      (stringRep, pushGen(firstValue) :: ops)

    }

    println(s"string rep of op being prepared\n$opsAsString\n")

    fail

  }

  it should "execute() when doing simple division" in new TestBinOpGene(" / ", _ / _) {
    val execute = gene.execute()
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
