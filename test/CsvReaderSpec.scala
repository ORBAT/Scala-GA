import java.io.File
import org.scalatest.FlatSpec
import scagen2.utils.CsvReader
import scala.io.{Codec, Source}

class CsvReaderSpec extends FlatSpec {

  trait TestFileReader {
    implicit val codec = Codec.UTF8
    val testSource: Source = {
      val testMaterial: String = {
        ???
      }
      ???
    }
    throw new IllegalStateException("CsvReaderSpec$TestFileReader# not implemented")
  }

  behavior of "A CsvReader"

  it should "" in new TestFileReader {

  }

  /*  it should "should work in LIFO order" in new StackWithLen(2) {
      assert(stack.pop() === 2)
      assert(stack.pop() === 1)
      assert(stack.pop().isNaN)
    }*/

}
