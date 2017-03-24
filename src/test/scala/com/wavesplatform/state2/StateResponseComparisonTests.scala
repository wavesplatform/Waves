package com.wavesplatform.state2

import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Account, AddressScheme}
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.{StoredBlockchain, StoredState, ValidatorImpl}
import scorex.transaction.state.database.state.storage._

import scala.collection.immutable.Iterable
import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class StateResponseComparisonTests extends FreeSpec with Matchers {

  import StateResponseComparisonTests._

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'W'
  }


  val CHECK_BLOCKS = Range(1, 100)
  val APPLY_TO = 28001


  "provide the same answers to questions after each block from mainnet applied" ignore {
    val oldStore = BlockStorageImpl.createMVStore("")
    val old = storedBC(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore("")
    val nev = storedBC(newState(newStore), new StoredBlockchain(newStore))

    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))

    // 0 doesn't exist, 1 is genesis
    val end = currentMainnet.history.height() + 1
    Range(1, 101).foreach { blockNumber =>
      s"[$blockNumber]" - {
        def block = currentMainnet.history.blockAt(blockNumber).get

        "[OLD] Block appended successfully" in {
          val oldTime = withTime(old.appendBlock(block).get)._1
        }
        "[NEW] New appended successfully" in {
          val newTime = withTime(nev.appendBlock(block).get)._1
        }
        if (CHECK_BLOCKS contains blockNumber) {
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

          s"accountTransactions" in {
            for (acc <- aliveAccounts) {
              val oldtxs = old.state.accountTransactions(acc, Int.MaxValue).toList
              val newtxs = nev.state.accountTransactions(acc, Int.MaxValue).toList
              val same = oldtxs.size == newtxs.size
              assert(same, s"acc: ${acc.stringRepr}")
              oldtxs.indices.foreach { i =>
                // we do not assert the actual order here, is it wrong?
                // assert(oldtxs(i).id sameElements newtxs(i).id, s"i = $i")
                assert(newtxs.exists(tx => tx.id sameElements oldtxs(i).id))
              }
            }
          }
          s"lastAccountPaymentTransaction" ignore {
            for (acc <- aliveAccounts) {
              val oldPtx = old.state.lastAccountPaymentTransaction(acc)
              val nevPtx = nev.state.lastAccountPaymentTransaction(acc)
              val areSame = oldPtx == nevPtx
              assert(areSame, acc.stringRepr + "\n" + "OLD: " + oldPtx + "\n" + "NEW: " + nevPtx)
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
  "block application time measure" ignore {
    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))
    val end = currentMainnet.history.height() + 1
    getStorages(currentMainnet, "C:\\Users\\ilyas\\Desktop\\old_f.store", "C:\\Users\\ilyas\\Desktop\\new_f.store", appl = true)
  }

  "assert state" ignore {
    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))
    val (old, nev) = getStorages(currentMainnet, "C:\\Users\\ilyas\\Desktop\\old_f.store", "C:\\Users\\ilyas\\Desktop\\new_f.store", appl = false)

    lazy val block = old.history.lastBlock

    lazy val aliveAccounts = old.state.wavesDistributionAtHeight(old.state.stateHeight)
      .map(_._1)
      .map(Account.fromString(_).right.get)
      .toIndexedSeq

    "height" in {
      assert(old.state.stateHeight == nev.state.stateHeight)
      assert(old.history.lastBlock.encodedId == nev.history.lastBlock.encodedId)
    }

    s"findTransaction and included" ignore {
      Range(1, old.state.stateHeight).foreach(idx => {
        logStep(idx, old.state.stateHeight)("findTransaction and included")
        val oldBlock = old.history.blockAt(idx).get
        oldBlock.transactionData.foreach { tx =>
          assert(nev.state.included(tx.id).contains(old.state.included(tx.id).get))
          assert(nev.state.findTransaction[Transaction](tx.id).contains(tx))

        }
      })
    }


    s"accountTransactions" ignore {
      for (accIdx <- aliveAccounts.indices) {
        logStep(accIdx, aliveAccounts.size, 100)("accountTransactions")
        val acc = aliveAccounts(accIdx)
        val oldtxs = old.state.accountTransactions(acc, Int.MaxValue).toList
        val newtxs = nev.state.accountTransactions(acc, Int.MaxValue).toList
        assert(oldtxs.size == newtxs.size, s"acc: ${acc.stringRepr}, \n\nold:\n $oldtxs\n\nnew:\n$newtxs")
        val oldSet = oldtxs.map(tx => EqByteArray(tx.id)).toSet
        val newSet = newtxs.map(tx => EqByteArray(tx.id)).toSet
        assert(oldSet.equals(newSet))
      }
    }

    "total waves balance" in {
      assert(aliveAccounts.map(acc => nev.state.balance(acc)).sum == 100000000L)
    }

    s"balance, effectiveBalance, leasedSum" ignore {
      for (accIdx <- aliveAccounts.indices) {
        logStep(accIdx, aliveAccounts.size)("balance, effectiveBalance, leasedSum")
        val acc = aliveAccounts(accIdx)
        val oldBalance = old.state.balance(acc)
        val newBalance = nev.state.balance(acc)
        assert(oldBalance == newBalance, s"old=$oldBalance new=$newBalance acc: $acc")
        assert(old.state.effectiveBalance(acc) == nev.state.effectiveBalance(acc))
        assert(old.state.getLeasedSum(acc.stringRepr) == nev.state.getLeasedSum(acc.stringRepr))
      }
    }

    s"isReissuable, totalAssetQuantity" ignore {
      val eqAssetIds = aliveAccounts.flatMap(acc => old.state.getAccountBalance(acc).keySet.map(EqByteArray)).toIndexedSeq
      for (eqAssetIdIdx <- eqAssetIds.indices) {
        logStep(eqAssetIdIdx, aliveAccounts.size, 100)("isReissuable, totalAssetQuantity")
        val eqAssetId = eqAssetIds(eqAssetIdIdx)
        val assetId = eqAssetId.arr
        val oldreissuable = old.state.isReissuable(assetId)
        val newreissuable = nev.state.isReissuable(assetId)
        val oldTotal = old.state.totalAssetQuantity(assetId)
        val newTotal = nev.state.totalAssetQuantity(assetId)
        assert(oldreissuable == newreissuable, s"old: $oldreissuable, $oldTotal" + s" new $newreissuable, $newTotal " + Base58.encode(assetId))
        assert(oldTotal == newTotal)
      }
    }

    s"getAccountBalance, assetBalance" ignore {
      for (accIdx <- aliveAccounts.indices) {
        logStep(accIdx, aliveAccounts.size)("getAccountBalance, assetBalance")
        val acc = aliveAccounts(accIdx)
        val oldAccBalance = old.state.getAccountBalance(acc).map { case (k, v) => EqByteArray(k) -> v }
        val newAccBalance = nev.state.getAccountBalance(acc).map { case (k, v) => EqByteArray(k) -> v }
        assert(newAccBalance == oldAccBalance, s"acc::: $acc")

        val oldAssetAccs: Iterable[AssetAcc] = oldAccBalance.map(_._1.arr).map(aid => AssetAcc(acc, Some(aid)))

        for (assetAcc <- oldAssetAccs) {
          assert(old.state.assetBalance(assetAcc) == nev.state.assetBalance(assetAcc))
        }
      }
    }


    s"effectiveBalanceWithConfirmations" in {
      for {
        accIdx <- aliveAccounts.indices
        _ = logStep(accIdx, aliveAccounts.size)("effectiveBalanceWithConfirmations")
        acc = aliveAccounts(accIdx)
        confs <- Seq(50, 1000)
        oldEBWC = old.state.effectiveBalanceWithConfirmations(acc, confs, old.state.stateHeight)
        newEBWC = nev.state.effectiveBalanceWithConfirmations(acc, confs, nev.state.stateHeight)

      } yield {
        assert(oldEBWC == newEBWC, s"acc=$acc old=$oldEBWC new=$newEBWC")
      }
    }
  }
}

object StateResponseComparisonTests extends FreeSpec {
  val BlocksOnDisk = "C:\\Users\\ilyas\\waves\\data\\blockchain.dat"

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

  def logStep(step: Int, total: Int, totalSteps: Int = 10)(descr: String = ""): Unit = {
    //    if (step % (total / totalSteps) == 0)
    {
      println(s"$descr: $step of $total..")
    }
  }

  def getStorages(currentMainnet: BlockStorage, oldStorageFile: String, newStorageFile: String, appl: Boolean): (BlockStorage, BlockStorage) = {
    val oldStore = BlockStorageImpl.createMVStore(oldStorageFile)
    val old = storedBC(oldState(oldStore), new StoredBlockchain(oldStore))

    val newStore = BlockStorageImpl.createMVStore(newStorageFile)
    val p = new MVStorePrimitiveImpl(newStore)
    val rrrr: StateWriterImpl = new StateWriterImpl(p)
    val nev = storedBC(new StateWriterAdapter(
      rrrr, FunctionalitySettings.MAINNET), new StoredBlockchain(newStore))
    val start = 1
    val end = currentMainnet.history.height() + 1
    if (appl) {
      val (t1, _) = withTime(Range(start, end).map {
        blockNumber =>
          val block = currentMainnet.history.blockAt(blockNumber).get
          nev.appendBlock(block).get
          if (blockNumber % 1000 == 0 || blockNumber > end - 5) {

            println(blockNumber)

          }
          //          println(blockNumber)
          //          val total = p.portfolios.values.asScala.map(_._1).sum
          //          if (total != 10000000000000000L) {
          //            println(
          //              s"""!!!! bad sum($total) after $blockNumber
          //                 |
          //                   |${block.encodedId}
          //                 |
          //                   |$block""".stripMargin)
          //            throw new Exception()
          //          }

          //      if (blockNumber > 410000) {
          //        val total = p.portfolios.values.asScala.map(_._1).sum
          //        if (total != 10000000000000000L) {
          //          println(
          //            s"""!!!! bad sum($total) after $blockNumber
          //               |
          //               |${block.encodedId}
          //               |
          //               |$block""".stripMargin)
          //          throw new Exception()
          //        }
          //      }
          ()
      })
      newStore.commit()
      newStore.close()

      println("new time " + t1)

      val oldValidator = new ValidatorImpl(old.state, FunctionalitySettings.MAINNET)

      val (t0, _) = withTime(Range(start, end).foreach {
        blockNumber =>
          val block = currentMainnet.history.blockAt(blockNumber).get
          assert(oldValidator.validate(block.transactionData, None, block.timestamp)._1.isEmpty)
          old.appendBlock(block).get
          if (blockNumber % 10000 == 0) {
            println(blockNumber)
          }
      })

      oldStore.commit()
      oldStore.close()
      println("--------------")
      println("old time " + t0)
      println("new time " + t1)

    }
    (old, nev)
  }
}