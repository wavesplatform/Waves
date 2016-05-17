package scorex.transaction.state

import java.io.File

import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.lagonaki.mocks.BlockMock
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}
import scorex.utils._

import scala.util.{Random, Success, Try}

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
  val genesisTxs: Seq[GenesisTransaction] = accounts.map(a => GenesisTransaction(a, TotalBalance / accN, 0L))

  case class State(name: String, height: Int, included: Map[Transaction, Int])

  case class Sut(fileName: String) {
    val storedState = new StoredState(Some(fileName))
    storedState.processBlock(new BlockMock(genesisTxs))
  }

  override def destroySut(sut: Sut): Unit = {
    sut.storedState.finalize()
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

  val genCheckTransaction: Gen[CheckTransaction] = CheckTransaction(createTransaction())

  def genCheckExistingTransaction(state: State): Gen[CheckTransaction] = for {
    key <- Gen.oneOf(state.included.keys.toSeq)
  } yield CheckTransaction(key)

  val genGenesis: Gen[PutTransactions] = PutTransactions(genesisTxs)

  val genTransaction: Gen[PutTransactions] = Gen.chooseNum(1, 2).map { i =>
    //all transactions should be valid
    PutTransactions((1 to i).map(j => createTransaction()))
  }

  def genValidateTransactions(state: State): Gen[ValidateTransactions] = Gen.chooseNum(1, MaxTransactions).map { i =>
    val included = Random.shuffle(state.included.keys).take(i).map((_, true)).toSeq
    val notIncluded = (0 until MaxTransactions - i).map(j => (createTransaction(), false)).toSeq
    ValidateTransactions(included ++ notIncluded)
  }

  case class CheckTransaction(signature: Transaction) extends Command {
    type Result = Option[Int]

    def run(sut: Sut) = sut.synchronized {
      sut.storedState.included(signature, None)
    }

    def nextState(state: State) = state

    def preCondition(state: State) = true

    def postCondition(state: State, result: Try[Result]) =
      state.included.get(signature) match {
        case None => true
        case Some(value) => result == Success(Some(value))
      }
  }

  case class PutTransactions(txs: Seq[Transaction]) extends Command {

    type Result = (Int, Long)

    def run(sut: Sut) = sut.synchronized {
      assert(sut.storedState.isValid(txs))
      val block = new BlockMock(txs)
      sut.storedState.processBlock(block)
      (sut.storedState.stateHeight, sut.storedState.totalBalance)
    }

    def nextState(state: State) = state.copy(
      height = state.height + 1,
      included = state.included ++ txs.map(_ -> (state.height + 1))
    )

    def preCondition(state: State) = true

    override def postCondition(state: State, result: Try[Result]): Prop =
      result == Success((state.height + 1, TotalBalance))
  }

  case class ValidateTransactions(txs: Seq[(Transaction, Boolean)]) extends Command {

    type Result = Seq[Transaction]

    def run(sut: Sut) = sut.synchronized {
      sut.storedState.validate(txs.map(_._1))
    }

    def nextState(state: State) = state

    def preCondition(state: State) = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      txs.filter(_._2 == false).map(_._1) == result.get
    }
  }

  def createTransaction(): Transaction = createTransaction(1 + Random.nextInt(10), 1 + Random.nextInt(10))

  def createTransaction(amount: Long, fee: Long): Transaction =
    createPayment(accounts(Random.nextInt(accN)), accounts(Random.nextInt(accN)), amount, fee)

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
  }

}
