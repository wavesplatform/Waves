package com.wavesplatform.state2

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.Account
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}
import scorex.transaction.state.database.state.storage._
import scorex.transaction.{BlockStorage, History, State, Transaction}

import scala.util.Try

class StateResponseComparisonTests extends FreeSpec with Matchers {

  import StateResponseComparisonTests._

  val BlocksOnDisk = "C:\\Users\\ilyas\\.babun\\cygwin\\home\\ilyas\\waves\\data\\blockchain.dat"


  "provide the same answers to questions after each block from mainnet applied" in {
    val oldStore = BlockStorageImpl.createMVStore("")
    val old = storedBC(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore("")
    val nev = storedBC(newState(newStore), new StoredBlockchain(newStore))

    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))


    // weird, blocks at 0 and 1 do not exist
    Range(2, currentMainnet.history.height() + 1).foreach { blockNumber =>
      val block = currentMainnet.history.blockAt(blockNumber).get
      old.appendBlock(block).get
      nev.appendBlock(block).get

      // should I do this with more ids, like with final state too, to assert negatives too?
      "[findTransaction]" - {
        assert(block.transactionData.forall(tx => nev.state.findTransaction[Transaction](tx.id).contains(tx)))
      }
      "[included]" - {
        assert(block.transactionData.forall(tx => nev.state.included(tx.id).contains(nev.state.stateHeight)))
      }

      val aliveAccounts = old.state.wavesDistributionAtHeight(old.state.stateHeight)
        .map(_._1)
        .map(Account.fromBase58String(_).right.get)

      "[accountTransactions]" - {
        for (acc <- aliveAccounts) {
          val oldtxs = old.state.accountTransactions(acc, Int.MaxValue)
          val newtxs = nev.state.accountTransactions(acc, Int.MaxValue)
          assert(oldtxs.size == newtxs.size)
          assert(oldtxs.indices.forall(i => oldtxs(i) == newtxs(i)))
          true
        }
      }
      "[lastAccountPaymentTransaction]" - {
        for (acc <- aliveAccounts) {
          assert(old.state.lastAccountPaymentTransaction(acc) == nev.state.lastAccountPaymentTransaction(acc))
        }
      }
      "[balance]" - {
        for (acc <- aliveAccounts) {
          assert(old.state.balance(acc) == nev.state.balance(acc))
        }
      }

      ()

    }
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

  def newState(mVStore: MVStore): State = new StateWriterAdapter(new StateWriterImpl(new MVStorePrimitiveImpl(mVStore)))

  def storedBC(theState: State, theHistory: History): BlockStorage = {
    val settings = BlockchainSettings("", 'W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
    new BlockStorageImpl(settings) {
      override val history: History = theHistory
      override val state: State = theState
    }
  }

}