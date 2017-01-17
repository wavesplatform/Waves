package scorex.transaction.state

import java.io.File

import org.h2.mvstore.MVStore
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.lagonaki.mocks.BlockMock
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}
import scorex.utils._

import scala.util.{Random, Success, Try}
import scorex.settings.{Settings, WavesHardForkParameters}

class StateTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers {

  property("state test") {
    StateTestSpec.property().mainRunner(Array()) shouldBe 0
  }
}


object StateTestSpec extends Commands {
  val TestFolder = "target/test/"
  new File(TestFolder).mkdirs()
  val accounts = (1 to 10) map (i => new PrivateKeyAccount(randomBytes()))
  val accN = accounts.size
  val TotalBalance = 10000000
  val MaxTransactions = 100
  val genesisTxs: Seq[GenesisTransaction] = accounts.map(a => GenesisTransaction.create(a, TotalBalance / accN, 0L).right.get)
  val genCheckTransaction: Gen[CheckTransaction] = CheckTransaction(createTransaction())
  val genGenesis: Gen[PutTransactions] = PutTransactions(genesisTxs)
  val genTransaction: Gen[PutTransactions] = Gen.chooseNum(1, 2).map { i =>
    //all transactions should be valid
    PutTransactions((1 to i).map(j => createTransaction()))
  }

  override def destroySut(sut: Sut): Unit = {
    sut.db.close()
    new File(sut.fileName).delete()
  }

  override def initialPreCondition(state: State): Boolean = true

  override def canCreateNewSut(newState: State, initSuts: Traversable[State], runningSuts: Traversable[Sut]): Boolean = {
    !initSuts.exists(_.name == newState.name) && !runningSuts.exists(_.fileName == newState.name)
  }

  override def genInitialState: Gen[State] = for {
    name <- Gen.listOfN(16, Gen.alphaLowerChar).map(_.mkString)
  } yield State(TestFolder + name, 1, genesisTxs.map(_ -> 1).toMap)

  override def newSut(state: State): Sut = Sut(state.name)

  override def genCommand(state: State): Gen[Command] = {
    Gen.oneOf(
      genTransaction,
      genCheckExistingTransaction(state),
      genValidateTransactions(state),
      genCheckTransaction
    )
  }

  def genCheckExistingTransaction(state: State): Gen[CheckTransaction] = for {
    key <- Gen.oneOf(state.included.keys.toSeq)
  } yield CheckTransaction(key)

  def forgeSignature(signature: Array[Byte]): Array[Byte] = {
    val modifier: BigInt = BigInt("7237005577332262213973186563042994240829374041602535252466099000494570602496") +
      BigInt("27742317777372353535851937790883648493")
    signature.take(32) ++ (BigInt(signature.takeRight(32).reverse) + modifier).toByteArray.reverse
  }

  def genValidateTransactions(state: State): Gen[ValidateTransactions] = Gen.chooseNum(1, MaxTransactions).map { i =>
    val included = Random.shuffle(state.included.keys).take(i).map((_, true)).toSeq
    val includedPaymentTransactions = included.filter(_._1.isInstanceOf[PaymentTransaction])
    val notIncluded = if (includedPaymentTransactions.nonEmpty) {
      val transactions = (0 until MaxTransactions - i - 1).map(j => (createTransaction(), false))
      val transaction = includedPaymentTransactions.head._1.asInstanceOf[PaymentTransaction]
      val forgedTransaction = PaymentTransaction.create(transaction.sender, transaction.recipient, transaction.amount,
        transaction.fee, transaction.timestamp, forgeSignature(transaction.signature)).right.get

      transactions :+ (forgedTransaction -> true) // forged transaction should be detected as already included in the state
    } else {
      (0 until MaxTransactions - i).map(j => (createTransaction(), false))
    }

    ValidateTransactions(included ++ notIncluded)
  }

  def createTransaction(amount: Long, fee: Long): Transaction = {
    val randomAccounts = Random.shuffle(accounts).take(2)
    createPayment(randomAccounts.head, randomAccounts.last, amount, fee)
  }

  private def createTransaction(): Transaction = createTransaction(1 + Random.nextInt(10), 1 + Random.nextInt(10))

  private def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    Thread.sleep(2)
    val time = System.currentTimeMillis()
    PaymentTransaction.create(sender, recipient, amount, fee, time).right.get
  }

  case class State(name: String, height: Int, included: Map[Transaction, Int])

  case class Sut(fileName: String) {
    val db = new MVStore.Builder().fileName(fileName).compress().open()
    val storedState = StoredState.fromDB(db, WavesHardForkParameters.Disabled)
    storedState.processBlock(new BlockMock(genesisTxs))
  }

  case class CheckTransaction(signature: Transaction) extends Command {
    type Result = Option[Int]

    def run(sut: Sut): Result = sut.synchronized {
      sut.storedState.included(signature)
    }

    def nextState(state: State): State = state

    def preCondition(state: State): Boolean = true

    def postCondition(state: State, result: Try[Result]): Prop =
      state.included.get(signature) match {
        case None => true
        case Some(value) => result == Success(Some(value))
      }
  }

  case class PutTransactions(txs: Seq[Transaction]) extends Command {

    type Result = (Int, Long)

    def run(sut: Sut): Result = sut.synchronized {
      assert(sut.storedState.isValid(txs, blockTime = txs.map(_.timestamp).max))
      val block = new BlockMock(txs)
      sut.storedState.processBlock(block)
      (sut.storedState.stateHeight, sut.storedState.totalBalance)
    }

    def nextState(state: State): State = state.copy(
      height = state.height + 1,
      included = state.included ++ txs.map(_ -> (state.height + 1))
    )

    def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result == Success((state.height + 1, TotalBalance))
  }

  case class ValidateTransactions(txs: Seq[(Transaction, Boolean)]) extends Command {

    type Result = Seq[Transaction]

    def run(sut: Sut): Result = sut.synchronized {
      sut.storedState.validate(txs.map(_._1), blockTime = txs.map(_._1.timestamp).max)
    }

    def nextState(state: State): State = state

    def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      txs.filter(_._2 == false).map(_._1).diff(result.get).isEmpty
    }
  }

}
