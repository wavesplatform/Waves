package scorex.unit

import org.scalatest.FunSuite
import scorex.block.GenesisBlock
import scorex.database.PrunableBlockchainStorage

class BlockchainStorageSpecification extends FunSuite {

  test("genesis block save & find") {
    assert(PrunableBlockchainStorage.appendBlock(GenesisBlock).heightOf(GenesisBlock.signature).get == 1)
  }

}
