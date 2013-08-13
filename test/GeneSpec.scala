import org.scalatest.FlatSpec
import scagen2.vm._
import scagen2.vm.InstructionTools._
import scagen2.vm.SimpleStack._
import scala.annotation.tailrec

class GeneSpec extends FlatSpec {

  import InstructionTools.binaryFns
  import Instruction.BinFn
  import SimpleStack.ItemType

  def numbersToPushes(values: Seq[ItemType]): Seq[Instruction] = {
    values map { value => InstructionTools.pushGen(value) }
  }

  /**
   * Chains binary operators together for testing. E.g. if your `numbersStream` gives only odd numbers and you
   * have the binary operations `Seq(/, *, -, +)` and their character representations
   *
   * "((((3.0 / 1.0)  *  5.00)   -  7.00)   +  9.00) "
   * @param numbersStream
   * @param stringFnPairs
   * @return
   */
  def giveResult(numbersStream: Stream[ItemType], stringFnPairs: Seq[(String, BinFn)]): (String, ItemType) = {
    def inner(fnPairsTail: Seq[(String, BinFn)], accum: ItemType,
              items: Stream[ItemType], strAcc: String): (String, ItemType) =
      fnPairsTail match {
        case (fnName, fn) :: restOfFns => {
          val a = items.head
          val newFnString = s" $fnName ${a.formatted("%.3g")}) "
          inner(restOfFns, fn(a, accum), items.tail, s"$strAcc$newFnString")
        }

        case _ => (strAcc, accum)
      }
    val a = numbersStream.tail.head
    val b = numbersStream.head
    val (firstFnSymbol, firstFn) = stringFnPairs.head
    val res = firstFn(a, b)
    //    println(s"fstPushed = $b\tsndPushed = $a\tfirstFnSymbol = $firstFnSymbol\n\t\t$res")
    inner(fnPairsTail = stringFnPairs.tail, accum = res, items = numbersStream.tail.tail,
           strAcc = s"(((($b$firstFnSymbol$a)")
  }

  abstract class TestBinOpGene(opSymbol: String, op: (ItemType, ItemType) => ItemType) {
    val values = Seq(2.9, 4.7)
    assert(values.length > 1)
    val pushes        : Seq[Instruction] = numbersToPushes(values)
    val asInstruction      : Instruction      = InstructionTools.toInstruction(opSymbol, op)
    val expectedResult: ItemType                   = {
      // keep only the two last pushed items.
      val lastTwo = values.drop(values.length - 2).reverse
      val res = op(lastTwo(0), lastTwo(1))
      //      println(s"lastTwo ${lastTwo.mkString(", ")}\n${lastTwo(0) }${opSymbol }${lastTwo(1)} = ${res}")
      res
    }
    val gene                                       = new Gene(pushes :+ asInstruction)
    val maxTolerance                               = 0.001
  }

  behavior of "A Gene"

  it should "handle all simple binary ops during execute()" in new {
    binaryFns.foreach(tuple =>
      new TestBinOpGene(tuple._1, tuple._2) {
        val execute = gene.execute
        val absDiff = math.abs(execute - expectedResult)
        assert(absDiff < maxTolerance, s"BinOpGene returned ${execute}, diff to expected result (${expectedResult}) " +
                                       s"was ${absDiff}")
      })
  }

  it should "execute() when doing binary ops in a chain" in new {

    import Instruction.Operation


    val step        : ItemType                             = 1d
    val steppedItems: Stream[ItemType]                     = Stream.iterate(1d)(_ + step)
    val selectedOps                                        = binaryFns.reverse.toList
    // TODO: create instructions to the tune of 2 4 + 6 - 8 * 10 /
    val (opsAsString: String, chainedOps: List[Instruction]) = {
      val values: Iterator[ItemType] = steppedItems.toIterator
      import SimpleStack.ItemType
      import InstructionTools._
      def funToPushAndOp(value: ItemType, x: Instruction): Seq[Instruction] = {
        Seq(pushGen(value), x)
      }


      /**
       * Takes all binary operations in `ops`
       * @param ops The list of Instructions which we will use
       * @param instrAccum an accumulator for Operations
       * @param strAccum accumulator for the string representation
       * @return
       */
      @tailrec def genOperations(ops: List[Instruction]
                                 , instrAccum: List[Instruction]
                                 , strAccum: String): (String, List[Instruction]) = ops match {
        case (ins@Instruction(symbol,op)) :: rest => {
          val nextVal = values.next()
          //          println(s"genOperations loop\n\tpush($nextVal)\n\t$symbol")
          genOperations(rest, instrAccum ++ funToPushAndOp(nextVal, ins), s"${strAccum} $nextVal $symbol")
        }
        case _ => (strAccum, instrAccum)
      }

      val firstValue = values.next()
      val (stringRep, ops) = genOperations(selectedOps map (InstructionTools.toInstruction(_)), List(), s"$firstValue")
      (stringRep, pushGen(firstValue) :: ops)

    }
    val g                                                  = new Gene(chainedOps)
    println(s"Gene.toString = ${g.toString}")
    val geneResult                                         = g.execute
    val (secondOpinionStr, secondOpinion)                  = giveResult(steppedItems, selectedOps)
    /*    println(s"RPN $opsAsString -->\nNOR $secondOpinionStr -->\n\tgeneResult = $geneResult" +
                s"\n\tsecondOpinion = $secondOpinion\n")*/

    assert(math.abs(geneResult - secondOpinion) < 0.001, s"geneResult = $geneResult\n" +
                                                         s"secondOpinion = $secondOpinion")
  }

  it should "execute() slightly more sophisticated genes" in new {
    val oneByTwo = Seq(pushGen(2d), pushGen(1d), Instruction.div)
    assert(new Gene(oneByTwo).execute === 0.5, "first")
    assert(new Gene(oneByTwo ++ Seq(pushGen(3d), Instruction.mul)).execute === 1.5, "second")
    assert(new Gene(Seq(pushGen(2d), pushGen(1d), Instruction.div, pushGen(3d), Instruction.mul)).execute === 1.5,
            "third")
    assert(new Gene(Seq(pushGen(1d), pushGen(2d), Instruction.div
                         , pushGen(3d), Instruction.mul, pushGen(4d), Instruction.sub, pushGen(5d)
                         , Instruction.add)).execute === 3, "fourth")

  }

  it should "execute() when doing simple division" in new TestBinOpGene(" / ", _ / _) {
    val execute = gene.execute
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
