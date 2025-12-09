package com.wavesplatform.it.sync.transactions

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.TransactionsApiRoute
import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuiteLike
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.CancelAfterFailure
import org.scalatest.freespec.AnyFreeSpecLike
import play.api.libs.json.Json

class LeasingTransactionsSuite extends AnyFreeSpecLike, BaseTransactionSuiteLike, CancelAfterFailure {
  private val errorMessage = "Reason: Cannot lease more than own"

  "leasing waves decreases lessor's eff.b. and increases lessee's eff.b.; lessor pays fee" - {
    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTx = sender.lease(firstKeyPair, secondAddress, leasingAmount, leasingFee = minFee, version = v)
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTx.id)
      if (v > 2) {
        createdLeaseTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[TransactionInfo](createdLeaseTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)
    }
  }

  "cannot lease non-own waves" - {
    each { v =>
      val createdLeaseTxId = sender.lease(firstKeyPair, secondAddress, leasingAmount, leasingFee = minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      val eff2 = miner.accountBalances(secondAddress)._2

      assertBadRequestAndResponse(sender.lease(secondKeyPair, thirdAddress, eff2 - minFee, leasingFee = minFee, version = v), errorMessage)
    }
  }

  "can not make leasing without having enough balance" - {
    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      // secondAddress effective balance more than general balance
      assertBadRequestAndResponse(sender.lease(secondKeyPair, firstAddress, balance2 + 1.waves, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      assertBadRequestAndResponse(sender.lease(firstKeyPair, secondAddress, balance1, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      assertBadRequestAndResponse(sender.lease(firstKeyPair, secondAddress, balance1 - minFee / 2, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      val newAddress = TxHelpers.signer(1000 + v)
      sender.transfer(sender.keyPair, newAddress.toAddress.toString, minFee, minFee, waitForTx = true)
      assertBadRequestAndResponse(sender.lease(newAddress, secondAddress, minFee + 1, minFee, version = v), errorMessage)
      nodes.waitForHeightArise()

      miner.assertBalances(firstAddress, balance1, eff1)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  "lease cancellation reverts eff.b. changes; lessor pays fee for both lease and cancellation" - {
    def getStatus(txId: String): String = {
      val r = sender.get(s"/transactions/info/$txId")
      (Json.parse(r.getResponseBody) \ "status").as[String]
    }

    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTxId = sender.lease(firstKeyPair, secondAddress, leasingAmount, minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      val status1 = getStatus(createdLeaseTxId)
      status1 shouldBe LeaseStatus.active.toString

      val activeLeases = sender.activeLeases(secondAddress)
      assert(activeLeases.forall(!_.sender.contains(secondAddress)))

      val leases1 = sender.activeLeases(firstAddress)
      assert(leases1.exists(_.id == createdLeaseTxId))

      val createdCancelLeaseTx = sender.cancelLease(firstKeyPair, createdLeaseTxId, minFee, v)
      nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTx.id)
      if (v > 2) {
        createdCancelLeaseTx.chainId shouldBe Some(AddressScheme.current.chainId)
        sender.transactionInfo[TransactionInfo](createdCancelLeaseTx.id).chainId shouldBe Some(AddressScheme.current.chainId)
      }

      miner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
      miner.assertBalances(secondAddress, balance2, eff2)

      val status2 = getStatus(createdLeaseTxId)
      status2 shouldBe TransactionsApiRoute.LeaseStatus.canceled.toString

      val leases2 = sender.activeLeases(firstAddress)
      assert(leases2.forall(_.id != createdLeaseTxId))

      leases2.size shouldBe leases1.size - 1
    }
  }

  "lease cancellation can be done only once" - {
    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeasingTxId = sender.lease(firstKeyPair, secondAddress, leasingAmount, minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeasingTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      val createdCancelLeaseTxId = sender.cancelLease(firstKeyPair, createdLeasingTxId, minFee).id
      nodes.waitForHeightAriseAndTxPresent(createdCancelLeaseTxId)

      assertBadRequestAndResponse(sender.cancelLease(firstKeyPair, createdLeasingTxId, minFee), "Reason: Cannot cancel already cancelled lease")

      miner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
      miner.assertBalances(secondAddress, balance2, eff2)
    }
  }

  "only sender can cancel lease transaction" - {
    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      val (balance2, eff2) = miner.accountBalances(secondAddress)

      val createdLeaseTxId = sender.lease(firstKeyPair, secondAddress, leasingAmount, leasingFee = minFee, version = v).id
      nodes.waitForHeightAriseAndTxPresent(createdLeaseTxId)

      miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
      miner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

      assertBadRequestAndResponse(sender.cancelLease(thirdKeyPair, createdLeaseTxId, minFee), "LeaseTransaction was leased by other sender")
    }
  }

  "can not make leasing to yourself" - {
    each { v =>
      val (balance1, eff1) = miner.accountBalances(firstAddress)
      assertBadRequestAndResponse(sender.lease(firstKeyPair, firstAddress, balance1 + 1.waves, minFee, v), "Transaction to yourself")
      nodes.waitForHeightArise()

      miner.assertBalances(firstAddress, balance1, eff1)
    }
  }

  private def each(f: Byte => Unit): Unit = for (v <- leaseTxSupportedVersions) s"v=$v" in f(v)
}
