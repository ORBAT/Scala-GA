import org.scalatest.FlatSpec
import scagen2.utils._
import scagen2.vm.{Instruction, InstructionTools, Gene}

class ContextSpec extends FlatSpec {
  behavior of "A Gene with Context"

  it should "support Skip If >= 0" in {
    // should skip 2 instructions after SIG
    val gene = new Gene(Seq(
      InstructionTools.pushGen(100.0)
      , InstructionTools.pushGen(300.0)
      , InstructionTools.pushGen(2.0) // should get popped
      , InstructionTools.pushGen(42.0) // should get popped
      , Instruction.skipIfGTE0
      , InstructionTools.pushGen(3.0) // should be skipped
      , InstructionTools.pushGen(4.0) // should be skipped
      , Instruction.add // should give 100 300 + and not 3 4 +
    ))
    assert(gene.execute === (100.0 + 300.0))
  }

  it should "support Skip If < 0" in {
    // should skip 2 instructions after SIL
    val gene = new Gene(Seq(
      InstructionTools.pushGen(100.0)
      , InstructionTools.pushGen(300.0)
      , InstructionTools.pushGen(2.0) // should get popped
      , InstructionTools.pushGen(-42.0) // should get popped
      , Instruction.skipIfLT0
      , InstructionTools.pushGen(3.0) // should be skipped
      , InstructionTools.pushGen(4.0) // should be skipped
      , Instruction.add // should give 100 300 + and not 3 4 +
    ))
    assert(gene.execute === (100.0 + 300.0))
  }

}