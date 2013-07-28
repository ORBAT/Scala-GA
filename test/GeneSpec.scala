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
    val a = numbersStream.tail.head
    val b = numbersStream.head
    val (firstFnSymbol, firstFn) = stringFnPairs.head
    val res = firstFn(a, b)
    println(s"fstPushed = $b\tsndPushed = $a\tfirstFnSymbol = $firstFnSymbol\n\t\t$res")
    inner(fnPairsTail = stringFnPairs.tail, accum = res, doubles = numbersStream.tail.tail,
           strAcc = s"(((($b$firstFnSymbol$a)")
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

    val step: ItemType = 1d
    val steppedDoubles = Stream.iterate(1d)(_ + step)
    val selectedOps = basicMathFns.toList.reverse


    // TODO: create instructions to the tune of 2 4 + 6 - 8 * 10 /
    val (opsAsString: String, chainedOps: List[Operation]) = {
      val values: Iterator[ItemType] = steppedDoubles.toIterator
      import SimpleStack.ItemType
      import InstructionTools._

      def funToPushAndOp(value: ItemType, x: BinFn): Seq[Operation] = {
        Seq(pushGen(value), toOpType(x))
      }


      /**
       * Takes all binary operations in `ops`
       * @param ops a list of tuples, all containing a function's "symbol" and the function itself
       * @param instrAccum an accumulator for Operations
       * @param strAccum accumulator for the string representation
       * @return
       */
      @tailrec def genOperations(ops: List[(String, BinFn)]
                                 , instrAccum: List[Operation]
                                 , strAccum: String): (String, List[Operation]) = ops match {
        case (symbol, op) :: rest => {
          val nextVal = values.next()
          println(s"genOperations loop\n\tpush($nextVal)\n\t$symbol")
          genOperations(rest, instrAccum ++ funToPushAndOp(nextVal, op), s"${strAccum} $nextVal $symbol")
        }
        case _ => (strAccum, instrAccum)
      }

      val firstValue = values.next()
      val (stringRep, ops) = genOperations(selectedOps, List(), s"$firstValue")
      (stringRep, pushGen(firstValue) :: ops)

    }


    val g = new Gene(chainedOps)
    val g2 = {
      import Instruction._
      new Gene(Seq(pushGen(1.0), pushGen(2.0), div, pushGen(3.0), mul, pushGen(4.0), sub,
                    pushGen(5.0), add))
    }
    val geneResult = g.execute()
    val g2Result = g2.execute()

    val (secondOpinionStr, secondOpinion) = giveResult(steppedDoubles, selectedOps)
    println(s"RPN $opsAsString -->\nNOR $secondOpinionStr -->\n\tgeneResult = $geneResult" +
            s"\n\tg2Result = $g2Result" +
            s"\n\tsecondOpinion = $secondOpinion\n")
    assert(g.)
    assert(math.abs(geneResult - secondOpinion) < 0.001)
  }

  it should "execute() slightly more sophisticated genes" in new {
    val oneByTwo = Seq(pushGen(2d), pushGen(1d), Instruction.div)
    assert(new Gene(oneByTwo).execute() === 0.5)
    assert(new Gene(oneByTwo ++ Seq(pushGen(3d), Instruction.mul)).execute() === 1.5)
    assert(new Gene(Seq(pushGen(2d), pushGen(1d), Instruction.div, pushGen(3d), Instruction.mul)).execute() === 1.5)
    assert(new Gene(Seq(pushGen(1d), pushGen(2d), Instruction.div
                         , pushGen(3d), Instruction.mul, pushGen(4d), Instruction.sub, pushGen(5d)
                         , Instruction.add)).execute() === 1.5)

  }

  it should "execute() when doing simple division" in new TestBinOpGene(" / ", _ / _) {
    val execute = gene.execute()
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
