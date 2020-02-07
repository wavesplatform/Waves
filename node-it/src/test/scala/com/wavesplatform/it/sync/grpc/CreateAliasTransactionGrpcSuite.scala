package com.wavesplatform.it.sync.grpc

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NTPTime
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{minFee, transferAmount}
import com.wavesplatform.it.util._
import com.wavesplatform.protobuf.transaction.{PBRecipients, Recipient}
import com.wavesplatform.transaction.ChainId
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class CreateAliasTransactionGrpcSuite extends GrpcBaseTransactionSuite with NTPTime with TableDrivenPropertyChecks {

  val (aliasCreator, aliasCreatorAddr) = (firstAcc, firstAddress)
  test("Able to send money to an alias") {
    val alias             = randomAlias
    val creatorBalance    = sender.grpc.wavesBalance(aliasCreatorAddr).available
    val creatorEffBalance = sender.grpc.wavesBalance(aliasCreatorAddr).effective

    sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)

    sender.grpc.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.grpc.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

    sender.grpc.resolveAlias(alias) shouldBe PBRecipients.toAddress(ByteStr(aliasCreatorAddr.toByteArray), ChainId.global).explicitGet()

    sender.grpc.broadcastTransfer(aliasCreator, Recipient().withAlias(alias), transferAmount, minFee, waitForTx = true)

    sender.grpc.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - 2 * minFee
    sender.grpc.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - 2 * minFee
  }

  test("Not able to create same aliases to same address") {
    val alias             = randomAlias
    val creatorBalance    = sender.grpc.wavesBalance(aliasCreatorAddr).available
    val creatorEffBalance = sender.grpc.wavesBalance(aliasCreatorAddr).effective

    sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    sender.grpc.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.grpc.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee

    assertGrpcError(sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee), "Alias already claimed", Code.INVALID_ARGUMENT)

    sender.grpc.wavesBalance(aliasCreatorAddr).available shouldBe creatorBalance - minFee
    sender.grpc.wavesBalance(aliasCreatorAddr).effective shouldBe creatorEffBalance - minFee
  }

  test("Not able to create aliases to other addresses") {
    val alias            = randomAlias
    val secondBalance    = sender.grpc.wavesBalance(secondAddress).available
    val secondEffBalance = sender.grpc.wavesBalance(secondAddress).effective

    sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    assertGrpcError(sender.grpc.broadcastCreateAlias(secondAcc, alias, minFee), "Alias already claimed", Code.INVALID_ARGUMENT)

    sender.grpc.wavesBalance(secondAddress).available shouldBe secondBalance
    sender.grpc.wavesBalance(secondAddress).effective shouldBe secondEffBalance
  }

  val ValidAliases =
    Table(s"aliasName$randomAlias", s"aaaa$randomAlias", s"....$randomAlias", s"1234567890.$randomAlias", s"@.@-@_@$randomAlias")

  ValidAliases.foreach { alias =>
    test(s"create alias named $alias") {
      sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
      sender.grpc.resolveAlias(alias) shouldBe PBRecipients.toAddress(ByteStr(aliasCreatorAddr.toByteArray), ChainId.global).explicitGet()
    }
  }

  val InvalidAliases =
    Table(
      ("aliasName", "message"),
      ("", "Alias '' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      ("morethen_thirtycharactersinline", "Alias 'morethen_thirtycharactersinline' length should be between 4 and 30"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("multilnetest\ntest", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("UpperCaseAliase", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz")
    )

  forAll(InvalidAliases) { (alias: String, message: String) =>
    test(s"Not able to create alias named $alias") {
      assertGrpcError(sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee), message, Code.INTERNAL)
    }
  }

  test("Able to lease by alias") {
    val (leaser, leaserAddr) = (thirdAcc, thirdAddress)
    val alias                = randomAlias

    val aliasCreatorBalance    = sender.grpc.wavesBalance(aliasCreatorAddr).available
    val aliasCreatorEffBalance = sender.grpc.wavesBalance(aliasCreatorAddr).effective
    val leaserBalance          = sender.grpc.wavesBalance(leaserAddr).available
    val leaserEffBalance       = sender.grpc.wavesBalance(leaserAddr).effective

    sender.grpc.broadcastCreateAlias(aliasCreator, alias, minFee, waitForTx = true)
    val leasingAmount = 1.waves

    sender.grpc.broadcastLease(leaser, Recipient().withAlias(alias), leasingAmount, minFee, waitForTx = true)

    sender.grpc.wavesBalance(aliasCreatorAddr).available shouldBe aliasCreatorBalance - minFee
    sender.grpc.wavesBalance(aliasCreatorAddr).effective shouldBe aliasCreatorEffBalance + leasingAmount - minFee
    sender.grpc.wavesBalance(leaserAddr).available shouldBe leaserBalance - leasingAmount - minFee
    sender.grpc.wavesBalance(leaserAddr).effective shouldBe leaserEffBalance - leasingAmount - minFee
  }

  test("Not able to create alias when insufficient funds") {
    val balance = sender.grpc.wavesBalance(aliasCreatorAddr).available
    val alias   = randomAlias
    assertGrpcError(sender.grpc.broadcastCreateAlias(aliasCreator, alias, balance + minFee), "Accounts balance errors", Code.INVALID_ARGUMENT)
  }

  private[this] def randomAlias: String =
    s"testalias.${Random.alphanumeric.take(9).mkString}".toLowerCase
}
