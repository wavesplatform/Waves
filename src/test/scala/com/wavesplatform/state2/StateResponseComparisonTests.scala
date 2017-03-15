package com.wavesplatform.state2

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Account, AddressScheme}
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}
import scorex.transaction.state.database.state.storage._

class StateResponseComparisonTests extends FreeSpec with Matchers {

  import StateResponseComparisonTests._

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'W'
  }

  val BlocksOnDisk = "C:\\Users\\ilyas\\.babun\\cygwin\\home\\ilyas\\waves\\data\\blockchain.dat"


  "provide the same answers to questions after each block from mainnet applied" - {
    val oldStore = BlockStorageImpl.createMVStore("")
    val old = storedBcPlusGenesis(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore("")
    val nev = storedBcPlusGenesis(newState(newStore), new StoredBlockchain(newStore))

    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))


    // 0 doesn't exist, 1 is genesis
    val end = currentMainnet.history.height() + 1
    Range(1, 30).foreach { blockNumber =>
      s"[$blockNumber]" - {

        def block = currentMainnet.history.blockAt(blockNumber).get

        "Block appended successfully" in {
          old.appendBlock(block).get
          nev.appendBlock(block).get
        }
        // should I do this with more ids, like with final state too, to assert negatives too?
        s"findTransaction" in {
          assert(block.transactionData.forall(tx => nev.state.findTransaction[Transaction](tx.id).contains(tx)))
        }
        s"included" in {
          assert(block.transactionData.forall(tx => nev.state.included(tx.id).contains(nev.state.stateHeight)))
        }

        val aliveAccounts = old.state.wavesDistributionAtHeight(old.state.stateHeight)
          .map(_._1)
          .map(Account.fromBase58String(_).right.get)

        s"accountTransactions" in {
          for (acc <- aliveAccounts) {
            val oldtxs = old.state.accountTransactions(acc, Int.MaxValue)
            val newtxs = nev.state.accountTransactions(acc, Int.MaxValue)
            assert(oldtxs.size == newtxs.size,s"acc: ${acc.stringRepr}")
            assert(oldtxs.indices.forall(i => oldtxs(i) == newtxs(i)))
            true
          }
        }
        s"lastAccountPaymentTransaction" in {
          for (acc <- aliveAccounts) {
            assert(old.state.lastAccountPaymentTransaction(acc) == nev.state.lastAccountPaymentTransaction(acc))
          }
        }
        s"balance" in {
          for (acc <- aliveAccounts) {
            assert(old.state.balance(acc) == nev.state.balance(acc))
          }
        }
      }
    }
    ()

  }


  "provide the same answers to questions after rollbacks" in {

  }
}


object StateResponseComparisonTests {

  def oldState(mvStore: MVStore): State = {

    val storage = new MVStoreStateStorage
      with MVStoreOrderMatchStorage
      with MVStoreAssetsExtendedStateStorage
      with MVStoreLeaseExtendedStateStorage
      with MVStoreAliasExtendedStorage {
      override val db: MVStore = mvStore
      if (db.getStoreVersion > 0) {
        db.rollback()
      }
    }
    new StoredState(storage, FunctionalitySettings.MAINNET)
  }

  def newState(mVStore: MVStore): State = new StateWriterAdapter(
    new StateWriterImpl(new MVStorePrimitiveImpl(mVStore)), FunctionalitySettings.MAINNET)

  val settings = BlockchainSettings("", 'W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)

  def storedBC(theState: State, theHistory: History): BlockStorage = {
    val blockStorageImpl = new BlockStorageImpl(settings) {
      override val history: History = theHistory
      override val state: State = theState
    }


    blockStorageImpl
  }

  def storedBcPlusGenesis(theState: State, theHistory: History): BlockStorage = {
    val bc = storedBC(theState, theHistory)
    val maybeGenesisSignature = Option(settings.genesisSettings.signature).filter(_.trim.nonEmpty)
    //
    //    val genesisBLock = Block.genesis(
    //      NxtLikeConsensusBlockData(settings.genesisSettings.initialBaseTarget, WavesConsensusModule.EmptySignature),
    //      SimpleTransactionModule.buildTransactions(settings.genesisSettings),
    //      settings.genesisSettings.blockTimestamp, maybeGenesisSignature)
    //
    //    bc.appendBlock(genesisBLock)
    bc
  }

}