package com.wavesplatform.it

import com.wavesplatform.settings.Constants
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.Await._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Random

class BalancesSpecification(allNodes: Seq[Node]) extends FunSuite with Matchers with ScalaFutures with IntegrationPatience with BeforeAndAfterAll {
  val sender = Random.shuffle(allNodes).head
  val richBalance = sender.address

  val first = result(sender.createAddress, 35.seconds)
  val second = result(sender.createAddress, 35.seconds)
  val third = result(sender.createAddress, 35.seconds)

  override def beforeAll(): Unit = {
    super.beforeAll()
    val f = for {
      fb <- sender.lastBlock
      txs <- Future.sequence(Seq(
        sender.transfer(richBalance, first, 200 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
        sender.transfer(richBalance, second, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)),
        sender.transfer(richBalance, third, 100 * Constants.UnitsInWave, sender.fee(TransactionType.TransferTransaction)))
      )
      _ <- {
        val allNodesAllTxsFindInBlockFutures = for {
          tx <- txs
          node <- allNodes
        } yield {
          node.findBlock(_.transactions.exists(_.id == tx.id), fb.height)
        }
        Future.sequence(allNodesAllTxsFindInBlockFutures)
      }

      r1 <- sender.balance(first).map(_ == 200 * Constants.UnitsInWave)
      r2 <- sender.balance(second).map(_ == 100 * Constants.UnitsInWave)
      r3 <- sender.balance(third).map(_ == 100 * Constants.UnitsInWave)
    } yield r1 && r2 && r3
    val res = result(f, 90 seconds)
    res shouldBe true
  }

//  private def assertBalances(acc: Account, balance: Long, effectiveBalance: Long): Unit = {
//    state.balance(acc) shouldBe balance
//    state.effectiveBalance(acc) shouldBe effectiveBalance
//  }
//
//  private def assertAssetBalances(acc: AssetAcc, balance: Long): Unit = {
//    state.assetBalance(acc) shouldBe balance
//  }
//
//  private var createdLeaseTxId: String = _
//
//  test("leasing is works correctly") {
//    assertBalances(first, 200 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//
//    createdLeaseTxId = lease(first, second, 100 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave).get
//
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//  }
//
//  test("Сan not make leasing without having enough money") {
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//
//    lease(second, first, 101 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave, assertSuccess = false)
//
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//  }
//
//  test("Can not make transfer without having enough of your own money") {
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//
//    assetTransfer(second, first, 101 * Constants.UnitsInWave, assertSuccess = false)
//
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//  }
//
//
//  test("Can not make transfer without having enough of effective balance") {
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//
//    assetTransfer(first, second, 91 * Constants.UnitsInWave, assertSuccess = false)
//
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//  }
//
//  test("Can not make leasing without having enough waves for fee") {
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//
//    lease(first, second, 88 * Constants.UnitsInWave, assertSuccess = false, fee = 3 * Constants.UnitsInWave)
//
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//  }
//
//
//  test("leasing cancel is works correctly") {
//    assertBalances(first, 190 * Constants.UnitsInWave, 90 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 200 * Constants.UnitsInWave)
//
//    cancelLease(first, createdLeaseTxId, fee = 10 * Constants.UnitsInWave)
//
//    assertBalances(first, 180 * Constants.UnitsInWave, 180 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//  }
//
//  var issuedAssetId: String = _
//
//  test("Assets issue should not lead to a change in the balance and the effective balance only on the fee") {
//    assertBalances(first, 180 * Constants.UnitsInWave, 180 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//
//    issuedAssetId = issue(first, "name", "description", 100000, 2, reissuable = true, fee = 10 * Constants.UnitsInWave).get
//
//    assertBalances(first, 170 * Constants.UnitsInWave, 170 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
//  }
//
//  test("Assets reissue should not lead to a change in the balance and the effective balance only on the fee") {
//    assertBalances(first, 170 * Constants.UnitsInWave, 170 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//
//    reissue(first, issuedAssetId, 100000, reissuable = true, fee = 10 * Constants.UnitsInWave).get
//
//    assertBalances(first, 160 * Constants.UnitsInWave, 160 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 200000)
//  }
//
//  test("Assets transfer should not lead to a change in the balance and the effective balance, only on the fee") {
//    assertBalances(first, 160 * Constants.UnitsInWave, 160 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//
//    assetTransfer(first, second, 100000, fee = 10 * Constants.UnitsInWave, Some(issuedAssetId))
//
//    assertBalances(first, 150 * Constants.UnitsInWave, 150 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
//    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)
//  }
//
//  test("Waves transfer should lead to a change in the balance and the effective balance and on the fee") {
//    assertBalances(first, 150 * Constants.UnitsInWave, 150 * Constants.UnitsInWave)
//    assertBalances(second, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//
//    assetTransfer(first, second, 10 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave)
//
//    assertBalances(first, 130 * Constants.UnitsInWave, 130 * Constants.UnitsInWave)
//    assertBalances(second, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
//  }
//
//  test("Waves payment should lead to a change in the balance and the effective balance and the fee") {
//    assertBalances(first, 130 * Constants.UnitsInWave, 130 * Constants.UnitsInWave)
//    assertBalances(second, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
//
//    payment(first, second, 10 * Constants.UnitsInWave, fee = 10 * Constants.UnitsInWave)
//
//    assertBalances(first, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
//    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
//  }
//
//  test("Burn should not lead to a change in the balance and the effective balance, only on the fee") {
//    assertBalances(first, 110 * Constants.UnitsInWave, 110 * Constants.UnitsInWave)
//    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
//    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 100000)
//    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)
//
//    burn(first, issuedAssetId, 100000, fee = 10 * Constants.UnitsInWave)
//
//    assertBalances(first, 100 * Constants.UnitsInWave, 100 * Constants.UnitsInWave)
//    assertBalances(second, 120 * Constants.UnitsInWave, 120 * Constants.UnitsInWave)
//    assertAssetBalances(AssetAcc(first, Some(Base58.decode(issuedAssetId).get)), 0)
//    assertAssetBalances(AssetAcc(second, Some(Base58.decode(issuedAssetId).get)), 100000)
//  }
}
