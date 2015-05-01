package scorex.unit

import org.scalatest.FunSuite
import scorex.block.NxtGenesisBlock
import scorex.database.blockchain.PrunableBlockchainStorage

class BlockchainStorageSpecification extends FunSuite {
  test("genesis block save & find") {
    assert(PrunableBlockchainStorage.appendBlock(NxtGenesisBlock).heightOf(NxtGenesisBlock.signature).get == 1)
  }
}
