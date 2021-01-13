package com.wavesplatform.it.sync.grpc

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync.{aliasTxSupportedVersions, minFee, transferAmount}
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.{PBRecipients, Recipient}
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class CreateAliasTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (aliasCreator, aliasCreatorAddr) = (firstAcc, firstAddress)
  test("Able to send money to an alias") {
    for (v <- aliasTxSupportedVersions) {
      val alias             = randomAlias()
      val creatorBalance    = miner.wavesBalance(aliasCreatorAddr).available
      val creatorEffBalance = miner.wavesBalance(aliasCreatorAddr).effective

      miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v, waitForTx = true)

      miner.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
      miner.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

      miner.resolveAlias(alias) shouldBe PBRecipients.toAddress(aliasCreatorAddr.toByteArray, AddressScheme.current.chainId).explicitGet()

      miner.broadcastTransfer(aliasCreator, Recipient().withAlias(alias), transferAmount, minFee, waitForTx = true)

      miner.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - 2 * minFee
      miner.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - 2 * minFee
    }
  }

  test("Not able to create same aliases to same address") {
    for (v <- aliasTxSupportedVersions) {
      val alias             = randomAlias()
      val creatorBalance    = miner.wavesBalance(aliasCreatorAddr).available
      val creatorEffBalance = miner.wavesBalance(aliasCreatorAddr).effective

      miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v, waitForTx = true)
      miner.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
      miner.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

      assertGrpcError(miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v), "Alias already claimed", Code.INVALID_ARGUMENT)

      miner.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
      miner.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee
    }
  }

  test("Not able to create aliases to other addresses") {
    for (v <- aliasTxSupportedVersions) {
      val alias            = randomAlias()
      val secondBalance    = miner.wavesBalance(secondAddress).available
      val secondEffBalance = miner.wavesBalance(secondAddress).effective

      miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v, waitForTx = true)
      assertGrpcError(miner.broadcastCreateAlias(secondAcc, alias, minFee, version = v), "Alias already claimed", Code.INVALID_ARGUMENT)

      miner.wavesBalance(secondAddress).available shouldBe secondBalance
      miner.wavesBalance(secondAddress).effective shouldBe secondEffBalance
    }
  }

  val aliases_names =
    Table(s"aliasName${randomAlias()}", s"aaaa${randomAlias()}", s"....${randomAlias()}", s"123456789.${randomAlias()}", s"@.@-@_@${randomAlias()}")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      for (v <- aliasTxSupportedVersions) {
        miner.broadcastCreateAlias(aliasCreator, s"$alias$v", minFee, version = v, waitForTx = true)
        miner.resolveAlias(s"$alias$v") shouldBe PBRecipients
          .toAddress(aliasCreatorAddr.toByteArray, AddressScheme.current.chainId)
          .explicitGet()
      }
    }
  }

  val invalid_aliases_names =
    Table(
      ("aliasName", "message"),
      ("", "Alias '' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      ("morethen_thirtycharactersinline", "Alias 'morethen_thirtycharactersinline' length should be between 4 and 30"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("multilnetest\ntest", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("UpperCaseAliase", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz")
    )

  forAll(invalid_aliases_names) { (alias: String, message: String) =>
    test(s"Not able to create alias named $alias") {
      for (v <- aliasTxSupportedVersions) {
        assertGrpcError(miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v), message, Code.INVALID_ARGUMENT)
      }
    }
  }

  test("Able to lease by alias") {
    for (v <- aliasTxSupportedVersions) {
      val (leaser, leaserAddr) = (thirdAcc, thirdAddress)
      val alias                = randomAlias()

      val aliasCreatorBalance    = miner.wavesBalance(aliasCreatorAddr).available
      val aliasCreatorEffBalance = miner.wavesBalance(aliasCreatorAddr).effective
      val leaserBalance          = miner.wavesBalance(leaserAddr).available
      val leaserEffBalance       = miner.wavesBalance(leaserAddr).effective

      miner.broadcastCreateAlias(aliasCreator, alias, minFee, version = v, waitForTx = true)
      val leasingAmount = 1.waves

      miner.broadcastLease(leaser, Recipient().withAlias(alias), leasingAmount, minFee, waitForTx = true)

      miner.wavesBalance(aliasCreatorAddr).available shouldBe aliasCreatorBalance - minFee
      miner.wavesBalance(aliasCreatorAddr).effective shouldBe aliasCreatorEffBalance + leasingAmount - minFee
      miner.wavesBalance(leaserAddr).available shouldBe leaserBalance - leasingAmount - minFee
      miner.wavesBalance(leaserAddr).effective shouldBe leaserEffBalance - leasingAmount - minFee
    }
  }

  test("Not able to create alias when insufficient funds") {
    for (v <- aliasTxSupportedVersions) {
      val balance = miner.wavesBalance(aliasCreatorAddr).available
      val alias   = randomAlias()
      assertGrpcError(
        miner.broadcastCreateAlias(aliasCreator, alias, balance + minFee, version = v),
        "Accounts balance errors",
        Code.INVALID_ARGUMENT
      )
    }
  }

  private def randomAlias(): String = {
    s"testalias.${Random.alphanumeric.take(9).mkString}".toLowerCase
  }

}
