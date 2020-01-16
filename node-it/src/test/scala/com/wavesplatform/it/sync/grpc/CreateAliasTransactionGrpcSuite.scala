package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.NTPTime
import com.wavesplatform.account.Address
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync.{minFee, transferAmount}
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.Recipient
import com.wavesplatform.common.utils.EitherExt2
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class CreateAliasTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (aliasCreator, aliasCreatorAddr) = (firstAcc, firstAddress)
  test("Able to send money to an alias") {
    val alias             = randomAlias()
    val creatorBalance    = sender.wavesBalance(aliasCreatorAddr).available
    val creatorEffBalance = sender.wavesBalance(aliasCreatorAddr).effective

    sender.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)

    sender.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

    sender.resolveAlias(alias) shouldBe Address.fromBytes(aliasCreatorAddr.toByteArray).explicitGet()

    sender.broadcastTransfer(aliasCreator, Recipient().withAlias(alias), transferAmount, minFee, waitForTx = true)

    sender.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - 2 * minFee
    sender.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - 2 * minFee
  }

  test("Not able to create same aliases to same address") {
    val alias             = randomAlias()
    val creatorBalance    = sender.wavesBalance(aliasCreatorAddr).available
    val creatorEffBalance = sender.wavesBalance(aliasCreatorAddr).effective

    sender.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    sender.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

    assertGrpcError(sender.broadcastCreateAlias(aliasCreator, alias, minFee), "Alias already claimed", Code.INVALID_ARGUMENT)

    sender.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee
  }

  test("Not able to create aliases to other addresses") {
    val alias            = randomAlias()
    val secondBalance    = sender.wavesBalance(secondAddress).available
    val secondEffBalance = sender.wavesBalance(secondAddress).effective

    sender.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    assertGrpcError(sender.broadcastCreateAlias(secondAcc, alias, minFee), "Alias already claimed", Code.INVALID_ARGUMENT)

    sender.wavesBalance(secondAddress).available shouldBe secondBalance
    sender.wavesBalance(secondAddress).effective shouldBe secondEffBalance
  }

  val aliases_names =
    Table(s"aliasName${randomAlias()}", s"aaaa${randomAlias()}", s"....${randomAlias()}", s"1234567890.${randomAlias()}", s"@.@-@_@${randomAlias()}")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      sender.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
      sender.resolveAlias(alias) shouldBe Address.fromBytes(aliasCreatorAddr.toByteArray).explicitGet()
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
      assertGrpcError(sender.broadcastCreateAlias(aliasCreator, alias, minFee), message, Code.INTERNAL)
    }
  }

  test("Able to lease by alias") {
    val (leaser, leaserAddr) = (thirdAcc, thirdAddress)
    val alias                = randomAlias()

    val aliasCreatorBalance    = sender.wavesBalance(aliasCreatorAddr).available
    val aliasCreatorEffBalance = sender.wavesBalance(aliasCreatorAddr).effective
    val leaserBalance          = sender.wavesBalance(leaserAddr).available
    val leaserEffBalance       = sender.wavesBalance(leaserAddr).effective

    sender.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    val leasingAmount = 1.waves

    sender.broadcastLease(leaser, Recipient().withAlias(alias), leasingAmount, minFee, waitForTx = true)

    sender.wavesBalance(aliasCreatorAddr).available shouldBe aliasCreatorBalance - minFee
    sender.wavesBalance(aliasCreatorAddr).effective shouldBe aliasCreatorEffBalance + leasingAmount - minFee
    sender.wavesBalance(leaserAddr).available shouldBe leaserBalance - leasingAmount - minFee
    sender.wavesBalance(leaserAddr).effective shouldBe leaserEffBalance - leasingAmount - minFee
  }

  test("Not able to create alias when insufficient funds") {
    val balance = sender.wavesBalance(aliasCreatorAddr).available
    val alias   = randomAlias()
    assertGrpcError(sender.broadcastCreateAlias(aliasCreator, alias, balance + minFee), "negative waves balance", Code.INVALID_ARGUMENT)
  }

  private def randomAlias(): String = {
    s"testalias.${Random.alphanumeric.take(9).mkString}".toLowerCase
  }

}
