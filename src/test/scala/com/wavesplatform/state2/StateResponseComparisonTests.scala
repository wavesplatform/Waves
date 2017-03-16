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
    val old = storedBC(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore("")
    val nev = storedBC(newState(newStore), new StoredBlockchain(newStore))

    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))

    val CHECK_FROM = 70
    val CHECK_TO = 300

    // 0 doesn't exist, 1 is genesis
    val end = currentMainnet.history.height() + 1
    Range(1, CHECK_TO).foreach { blockNumber =>
      s"[$blockNumber]" - {
        def block = currentMainnet.history.blockAt(blockNumber).get

        "Block appended successfully" in {
          val oldTime = withTime(old.appendBlock(block).get)._1
          val newTime = withTime(nev.appendBlock(block).get)._1
        }
        if (blockNumber >= CHECK_FROM) {
          // should I do this with more ids, like with final state too, to assert negatives too?
          s"findTransaction" in {
            assert(block.transactionData.forall(tx => nev.state.findTransaction[Transaction](tx.id).contains(tx)))
          }
          s"included" in {
            assert(block.transactionData.forall(tx => nev.state.included(tx.id).contains(nev.state.stateHeight)))
          }

          def aliveAccounts = old.state.wavesDistributionAtHeight(old.state.stateHeight)
            .map(_._1)
            .map(Account.fromBase58String(_).right.get)

          s"accountTransactions" in {
            for (acc <- aliveAccounts) {
              val oldtxs = old.state.accountTransactions(acc, Int.MaxValue).toList
              val newtxs = nev.state.accountTransactions(acc, Int.MaxValue).toList
              assert(oldtxs.size == newtxs.size, s"acc: ${acc.stringRepr}")
              oldtxs.indices.foreach { i =>
                // we do not assert the actual order here, is it wrong?
                // assert(oldtxs(i).id sameElements newtxs(i).id, s"i = $i")
                assert(newtxs.exists(tx => tx.id sameElements oldtxs(i).id))
              }
            }
          }
          s"lastAccountPaymentTransaction" in {
            for (acc <- aliveAccounts) {
              val oldPtx = old.state.lastAccountPaymentTransaction(acc)
              val nevPts = nev.state.lastAccountPaymentTransaction(acc)
              val areSame = oldPtx == nevPts
              assert(areSame, acc.stringRepr +" " + nevPts)
            }
          }
          s"balance" in {
            for (acc <- aliveAccounts) {
              assert(old.state.balance(acc) == nev.state.balance(acc))
              assert(old.state.effectiveBalance(acc) == nev.state.effectiveBalance(acc))
            }
          }

          "height" in {
            assert(old.state.stateHeight == nev.state.stateHeight)
          }
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

  def withTime[R](r: => R): (Long, R) = {
    val t0 = System.currentTimeMillis()
    val rr = r
    val t1 = System.currentTimeMillis()
    (t1 - t0, rr)
  }
}