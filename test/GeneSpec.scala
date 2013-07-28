import org.scalatest.FlatSpec
import scagen2.vm._

class GeneSpec extends FlatSpec {

  import SimpleStack.ItemType

  val binOps: Seq[(String, (ItemType, ItemType) => ItemType)] = Seq((" + ", _ + _)
                                                                       , (" - ", _ - _)
                                                                       , (" * ", _ * _)
                                                                       , (" / ", _ / _)
                                                                       , (" atan2 ", math.atan2 _)
                                                                       , (" hypot ", math.hypot _)
                                                                       , (" max ", math.max _)
                                                                       , (" min ", math.min _)
                                                                       , (" pow ", math.pow _))


  def numbersToPushes(values: Seq[Double]): Seq[(SimpleStack) => Unit] = {
    values map { value => InstructionTools.pushGen(value) }
  }

  abstract class TestBinOpGene(opSymbol: String, op: (ItemType, ItemType) => ItemType) {
    val values = Seq(2.9, 4.7)
    assert(values.length > 1)
    val pushes        : Seq[Instruction.Operation] = numbersToPushes(values)
    val asOpType      : (SimpleStack) => Unit   = InstructionTools.toOpType(op)
    val expectedResult: ItemType                = {
      // keep only the two last pushed items.
      val lastTwo = values.drop(values.length - 2).reverse
      val res = op(lastTwo(0), lastTwo(1))
      println(s"lastTwo ${lastTwo.mkString(", ")}\n${lastTwo(0) }${opSymbol }${lastTwo(1)} = ${res}")
      res
    }
    val gene                                    = new Gene(pushes :+ asOpType)
    val maxTolerance                            = 0.001
  }

  behavior of "A Gene"

  it should "execute() when doing lots of simple binary ops" in new {

    binOps.foreach(tuple =>
      new TestBinOpGene(tuple._1, tuple._2) {
        val execute = gene.execute()
        val absDiff = math.abs(execute - expectedResult)
        assert(absDiff < maxTolerance, s"BinOpGene returned ${execute}, diff to expected result (${expectedResult}) " +
                                       s"was ${absDiff}")
      })


    binOps.zip(1 to binOps.length map (2 * _)).map {
      tuple =>
          // TODO: create instructions to the tune of 1 2 + 3 - 4 * 5 /
      ???
    }
  }

  it should "execute() when doing simple division" in new TestBinOpGene(" / ", _ / _) {
    val execute = gene.execute()
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
