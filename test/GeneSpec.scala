import org.scalatest.FlatSpec
import scagen2.vm.{Gene, SimpleStack}

class GeneSpec extends FlatSpec {

  trait TestGene {
    import SimpleStack.ItemType
    val expectedResult:ItemType = ???
    val gene: Gene = ???
    val maxTolerance = 0.001
  }

  behavior of "A Gene"

  it should "execute() " in new TestGene {
    val execute = gene.execute()
    val absDiff = math.abs(execute - expectedResult)
    assert(absDiff < maxTolerance, s"Gene returned ${execute}, diff to expected result (${expectedResult}) " +
                                   s"was ${absDiff}")
  }
}
