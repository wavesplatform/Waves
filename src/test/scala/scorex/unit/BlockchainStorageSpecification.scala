package scorex.unit

import org.scalatest.FunSuite
import scorex.Controller
import scorex.block.NxtGenesisBlock

class BlockchainStorageSpecification extends FunSuite {
  test("genesis block save & find") {
    assert(Controller.blockchainStorage.appendBlock(NxtGenesisBlock)
      .heightOf(NxtGenesisBlock.signature).get == 1)
  }
}
