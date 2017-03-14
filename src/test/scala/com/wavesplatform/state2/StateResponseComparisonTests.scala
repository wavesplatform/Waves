package com.wavesplatform.state2

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, FunSuite, Matchers}
import scorex.account.Account
import scorex.transaction.{BlockStorage, State, Transaction}
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState}
import scorex.transaction.state.database.state.storage._

import scala.util.Try

class StateResponseComparisonTests extends FreeSpec with Matchers {

  import StateResponseComparisonTests._

  "provide the same answers to questions after each block from mainnet applied" in {
    val old = storedBC(oldState)
    val nev = storedBC(newState)
    val currentMainnet = storedBC(_ => {
      val mainnetStorage = BlockStorageImpl.createMVStore("C:\\Users\\ilyas\\.babun\\cygwin\\home\\ilyas\\waves\\data")
      oldState(mainnetStorage)
    })

    Range(0, currentMainnet.history.height()).foreach { blockNumber =>
      Try {
        val block = currentMainnet.history.blockAt(blockNumber).get
        old.appendBlock(block).get
        nev.appendBlock(block).get
        assertStates(old, nev.state)

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
      }.recoverWith {
        case e: Throwable => Try {
          println(s"Error applying block #$blockNumber")
          e.printStackTrace()
          throw e
        }
      }
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

  def newState(mVStore: MVStore): State = new StateReaderAdapter(new StateReaderImpl(new MVStorePrimitiveImpl(mVStore)))

  def storedBC(stateProvider: MVStore => State): BlockStorage = {
    val settings = BlockchainSettings("", 'W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
    new BlockStorageImpl(settings) {
      override val state: State = stateProvider(db)
    }
  }

  def assertStates(old: BlockStorage, nev: State): Unit = {

  }
}