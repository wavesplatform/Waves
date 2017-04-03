package scorex.lagonaki.integration

import org.scalatest._
import scorex.account.{Account, Alias, PrivateKeyAccount}
import scorex.account.PublicKeyAccount._
import scorex.lagonaki.TransactionTestingCommons
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{CreateAliasTransaction, Transaction}
import scorex.transaction.ValidationError.AliasNotExists
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.utils._

import scala.util._

class AliasTransactionSpecification extends FunSuite with Matchers with TransactionTestingCommons with PrivateMethodTester with OptionValues {

  private val state = application.transactionModule.blockStorage.state
  private val validator = application.transactionModule.validator
  private val history = application.transactionModule.blockStorage.history
  private val aliasCreator: PrivateKeyAccount = applicationNonEmptyAccounts.head

  private val TIME = 1L

  private val recipient = applicationEmptyAccounts.head
  require(aliasCreator.address != recipient.address)

  override def beforeAll(): Unit = {
    super.beforeAll()
    Thread.sleep(1000)
    waitForSingleConnection(application)
    waitForNextBlock(application)
    Thread.sleep(1000)
  }

  def ensureSenderHasBalance(sender: PrivateKeyAccount): Unit = {
    val transfer = TransferTransaction.create(None, aliasCreator, sender, 1000000L, TIME, None, 100000L, Array()).right.get
    state.processBlock(TestBlock(Seq(transfer))).get
    ()
  }

  def shouldBeValid(t: Transaction): Assertion = {
    validator.validate(t,TIME) shouldBe 'right
  }

  def shouldBeInvalid(t: Transaction): Assertion = {
    validator.validate(t,TIME) shouldBe 'left
  }


  test("able to issue alias and send money to alias") {

    val creatorBalance0 = state.balance(aliasCreator)
    val fee = 100000L
    val tx = CreateAliasTransaction.create(aliasCreator, Alias.buildWithCurrentNetworkByte("TRANSFER-ALIAS").right.get, fee, 1L).right.get
    val block = TestBlock(Seq(tx))
    shouldBeValid(tx)
    state.processBlock(block) shouldBe a[Success[_]]
    state.balance(aliasCreator) shouldBe creatorBalance0 - fee

    val sender: PrivateKeyAccount = PrivateKeyAccount(randomBytes())
    ensureSenderHasBalance(sender)
    val senderBalance = state.balance(sender)
    val creatorBalance = state.balance(aliasCreator)

    val fee2 = 100001L
    val amount = 1000L
    val tx2 = TransferTransaction.create(None, sender, Alias.buildWithCurrentNetworkByte("TRANSFER-ALIAS").right.get, amount, 1L, None, fee2, Array()).right.get
    shouldBeValid(tx2)
    state.processBlock(TestBlock(Seq(tx2))) shouldBe a[Success[_]]
    state.balance(aliasCreator) shouldBe (creatorBalance + amount)
    state.balance(sender) shouldBe (senderBalance - fee2 - amount)
  }

  test("able to issue alias and lease money to alias") {

    val creatorBalance0 = state.balance(aliasCreator)
    val fee = 100000L
    val tx = CreateAliasTransaction.create(aliasCreator, Alias.buildWithCurrentNetworkByte("LEASE-ALIAS").right.get, fee, 1L).right.get
    val block = TestBlock(Seq(tx))
    shouldBeValid(tx)
    state.processBlock(block) shouldBe a[Success[_]]
    state.balance(aliasCreator) shouldBe creatorBalance0 - fee

    val sender: PrivateKeyAccount = PrivateKeyAccount(randomBytes())
    ensureSenderHasBalance(sender)
    val senderEffectiveBalance = state.effectiveBalance(sender)
    val creatorEffectiveBalance = state.effectiveBalance(aliasCreator)

    val fee2 = 100001L
    val amount = 1000L
    val tx2 = LeaseTransaction.create(sender, amount, fee2, 1L, Alias.buildWithCurrentNetworkByte("LEASE-ALIAS").right.get).right.get
    shouldBeValid(tx2)
    state.processBlock(TestBlock(Seq(tx2))) shouldBe a[Success[_]]
    state.effectiveBalance(aliasCreator) shouldBe (creatorEffectiveBalance + amount)
    state.effectiveBalance(sender) shouldBe (senderEffectiveBalance - fee2 - amount)
  }

  test("unable to send to non-issued alias") {
    val senderBalance = state.balance(aliasCreator)

    val tx = TransferTransaction.create(None, aliasCreator, Alias.buildWithCurrentNetworkByte("NON-EXISTING ALIAS").right.get, 1000L, 1L, None, 100000L, Array()).right.get
    shouldBeInvalid(tx)
    val block = TestBlock(Seq(tx))
    state.processBlock(block) shouldBe a[Failure[_]]
    state.balance(aliasCreator) shouldBe senderBalance
  }

  test("unable to lease to non-issued alias") {
    val senderBalance = state.balance(aliasCreator)

    val tx = LeaseTransaction.create(aliasCreator, 10000L, 100000L, 1L, Alias.buildWithCurrentNetworkByte("NON-EXISTING ALIAS").right.get).right.get
    shouldBeInvalid(tx)
    val block = TestBlock(Seq(tx))
    state.processBlock(block) shouldBe a[Failure[_]]
    state.balance(aliasCreator) shouldBe senderBalance
  }


  test("unable to create the same alias in the next block") {
    val senderBalance = state.balance(aliasCreator)
    val fee = 100000L
    val tx = CreateAliasTransaction.create(aliasCreator, Alias.buildWithCurrentNetworkByte("THE ALIAS").right.get, fee, 1L).right.get
    val block = TestBlock(Seq(tx))
    state.processBlock(block)

    val tx2 = CreateAliasTransaction.create(aliasCreator, Alias.buildWithCurrentNetworkByte("THE ALIAS").right.get, fee, 1L).right.get
    shouldBeInvalid(tx2)

    val anotherCreator: PrivateKeyAccount = PrivateKeyAccount(randomBytes())
    ensureSenderHasBalance(anotherCreator)
    val tx3 = CreateAliasTransaction.create(aliasCreator, Alias.buildWithCurrentNetworkByte("THE ALIAS").right.get, fee, 1L).right.get
    shouldBeInvalid(tx3)
  }

  test("able to recreate alias after rollback") {
    val theAlias = Alias.buildWithCurrentNetworkByte("ALIAS")
    val tx = CreateAliasTransaction.create(aliasCreator, theAlias.right.get, 100000L, 1L).right.get
    val block = TestBlock(Seq(tx))
    state.processBlock(block)
    state.rollbackTo(state.stateHeight - 1)

    shouldBeValid(tx)
    state.processBlock(block) shouldBe a[Success[_]]
  }

}
