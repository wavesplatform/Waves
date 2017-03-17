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


  val CHECK_FROM = 107
  val CHECK_TO = 108
  val BLOCK_IDS_TO_CHECK_TXS = Seq(1, 2, 4, 8, 16, 32, 50, 64, 100, 128, 200,
    256, 300, 512, 600, 800, 1024, 1200, 1500, 1750, 2048, 3011, 3290, 3700, 4096)


  "provide the same answers to questions after each block from mainnet applied" - {
    val oldStore = BlockStorageImpl.createMVStore("")
    val old = storedBC(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore("")
    val nev = storedBC(newState(newStore), new StoredBlockchain(newStore))

    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))

    // 0 doesn't exist, 1 is genesis
    val end = currentMainnet.history.height() + 1
    Range(1, CHECK_TO).foreach { blockNumber =>
      s"[$blockNumber]" - {
        def block = currentMainnet.history.blockAt(blockNumber).get

        "[OLD] Block appended successfully" in {
          val oldTime = withTime(old.appendBlock(block).get)._1
        }
        "[NEW] New appended successfully" in {
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
            .map(Account.fromString(_).right.get)

          if (BLOCK_IDS_TO_CHECK_TXS.contains(blockNumber)) {
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
                assert(areSame, acc.stringRepr + " " + nevPts)
              }
            }
          }
          s"balance, effectiveBalance, leasedSum" in {
            for (acc <- aliveAccounts) {
              val oldBalance = old.state.balance(acc)
              val newBalance = nev.state.balance(acc)
              assert(oldBalance == newBalance, s"old=$oldBalance new=$newBalance acc: $acc")
              assert(old.state.effectiveBalance(acc) == nev.state.effectiveBalance(acc))
              assert(old.state.getLeasedSum(acc.stringRepr) == nev.state.getLeasedSum(acc.stringRepr))
            }
          }

          s"getAccountBalance, assetBalance" in {
            for (acc <- aliveAccounts) {
              val oldAccBalance = old.state.getAccountBalance(acc).map { case (k, v) => EqByteArray(k) -> v }
              val newAccBalance = nev.state.getAccountBalance(acc).map { case (k, v) => EqByteArray(k) -> v }
              assert(oldAccBalance == newAccBalance)

              val oldAssetAccs = oldAccBalance.map(_._1.arr).map(aid => AssetAcc(acc, Some(aid)))

              for (assetAcc <- oldAssetAccs) {
                assert(old.state.assetBalance(assetAcc) == nev.state.assetBalance(assetAcc))
              }
            }
          }

          s"isReissuable, totalAssetQuantity" in {
            val eqAssetIds = aliveAccounts.flatMap(acc => old.state.getAccountBalance(acc).keySet.map(EqByteArray))
            for (eqAssetId <- eqAssetIds) {
              val assetId = eqAssetId.arr
              assert(old.state.isReissuable(assetId) == nev.state.isReissuable(assetId))
              assert(old.state.totalAssetQuantity(assetId) == nev.state.totalAssetQuantity(assetId))
            }
          }

          "height" in {
            assert(old.state.stateHeight == nev.state.stateHeight)
          }

          if (BLOCK_IDS_TO_CHECK_TXS contains blockNumber) {
            s"effectiveBalanceWithConfirmations" in {
              for {
                acc <- aliveAccounts
                confs <- Seq(50, 1000)
                oldEBWC = old.state.effectiveBalanceWithConfirmations(acc, confs, old.state.stateHeight)
                newEBWC = nev.state.effectiveBalanceWithConfirmations(acc, confs, nev.state.stateHeight)

              } yield {
                assert(oldEBWC == newEBWC, s"acc=$acc old=$oldEBWC new=$newEBWC")
              }
            }
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