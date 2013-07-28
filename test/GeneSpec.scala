import org.scalatest.FlatSpec
import scagen2.vm._

class GeneSpec extends FlatSpec {


  import SimpleStack.ItemType
  abstract class TestBinOpGene(opSymbol: String, op: (ItemType, ItemType) => ItemType) {
    val values = Seq(2.9, 4.7)
    assert(values.length > 1)
    val pushes: Seq[Instruction.OpType] = values map {value => InstructionTools.pushGen(value)}
    val asOpType: (SimpleStack) => Unit = InstructionTools.toOpType(op)

    val expectedResult: ItemType = {
      // keep only the two last pushed items.
      val lastTwo = values.drop(values.length - 2).reverse
      val res = op(lastTwo(0), lastTwo(1))
      println(s"lastTwo ${lastTwo.mkString(", ")}\n${lastTwo(0)}${opSymbol}${lastTwo(1)} = ${res}")
      res
    }
    val gene = new Gene(pushes :+ asOpType)
    val maxTolerance = 0.001
  }

  behavior of "A Gene"

  it should "execute() when doing simple division" in new TestBinOpGene(" / ", _ / _) {
    val execute = gene.execute()
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
