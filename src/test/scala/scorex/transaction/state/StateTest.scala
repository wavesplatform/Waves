package scorex.transaction.state

import java.io.File

import org.scalacheck.Gen
import org.scalacheck.commands.Commands
import org.scalatest.PropSpec
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.lagonaki.mocks.BlockMock
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}
import scorex.utils._

import scala.util.{Random, Success, Try}

class StateTest extends PropSpec {
  property("Commands state test") {
    StateTestSpec.property().check
  }
}

object StateTestSpec extends Commands {
  val TestFolder = "target/test/"
  new File(TestFolder).mkdirs()
  val genesisAcc = new PrivateKeyAccount(randomBytes())
  val genesisBalance = 1000000

  case class State(name: String, height: Int, included: Map[Array[Byte], Int])

  case class Sut(fileName: String) {
    val storedState = new StoredState(Some(fileName))
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
    name <- Gen.listOfN(8, Gen.alphaLowerChar).map(_.mkString)
  } yield State(TestFolder + name, 0, Map.empty)

  override def newSut(state: State): Sut = Sut(state.name)

  override def genCommand(state: State): Gen[Command] = if (state.height > 0) {
    Gen.oneOf(
      genTransaction,
      genCheckExistingTransaction(state),
      genCheckTransaction
    )
  } else {
    Gen.oneOf(
      genGenesis,
      genCheckTransaction
    )
  }

  val genCheckTransaction: Gen[CheckTransaction] = Gen.resultOf(CheckTransaction)

  def genCheckExistingTransaction(state: State): Gen[CheckTransaction] = for {
    key <- Gen.oneOf(state.included.keys.toSeq)
  } yield CheckTransaction(key)

  val genGenesis: Gen[PutTransactions] = PutTransactions(Seq(genesisTx))

  val genTransaction: Gen[PutTransactions] = Gen.chooseNum(1, 100).map { i =>
    PutTransactions((1 to i).map(j => createTransaction(Random.nextInt(genesisBalance / 2), i)))
  }

  case class CheckTransaction(signature: Array[Byte]) extends Command {
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

  case class PutTransactions(txs: Seq[Transaction]) extends UnitCommand {

    def run(sut: Sut) = sut.synchronized {
      val block = new BlockMock(txs)
      sut.storedState.processBlock(block)
    }

    def nextState(state: State) = state.copy(
      height = state.height + 1,
      included = state.included ++ txs.map(_.signature -> (state.height + 1))
    )

    def preCondition(state: State) = true

    def postCondition(state: State, success: Boolean) = success
  }

  def createTransaction(amount: Long, fee: Long) = createPayment(genesisAcc, genesisAcc, amount, fee)

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
  }

  lazy val genesisTx: GenesisTransaction = GenesisTransaction(genesisAcc, genesisBalance, 0L)


}
