package scorex.transaction.state.database.blockchain

import scorex.transaction.state.LagonakiState
import scorex.transaction.{BlockChain, BlockStorage}

/**
  * Storage interface combining both history(blockchain/blocktree) and state
  */
class StoredBlockStorage(val history: BlockChain, val state: LagonakiState) extends BlockStorage {


}
