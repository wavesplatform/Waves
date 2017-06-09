package scorex.transaction

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.wavesplatform.settings.UTXSettings
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import scorex.account.PublicKeyAccount
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.network.message.Message
import scorex.network.{NetworkController, _}

import scala.concurrent.duration._
import scala.language.postfixOps

class UnconfirmedPoolSynchronizerSpecification extends TestKit(ActorSystem("UnconfirmedPoolSynchronizerSpecification"))
  with ImplicitSender with WordSpecLike with Matchers with MockFactory
  with BeforeAndAfterAll
  with OneInstancePerTest {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "An UnconfirmedPoolSynchronizer actor" must {

    val transactionModule = mock[NewTransactionHandler]
    val utxStorage = mock[UnconfirmedTransactionsStorage]

    def createPoolSynchronizer(broadcastInterval: FiniteDuration) = {
      TestActorRef(new UnconfirmedPoolSynchronizer(transactionModule, UTXSettings(1000, broadcastInterval), testActor, utxStorage))
    }

    val defaultRecipient = PublicKeyAccount(Array.fill(32)(0: Byte))
    val tx = GenesisTransaction.create(defaultRecipient, 149857264546L, 4598723454L).right.get

    "registers message handler" in {

      (transactionModule.onNewOffchainTransactionExcept(_: Transaction, _: Option[ConnectedPeer])).expects(*, *).returns(Right[ValidationError, Transaction](tx))

      val actorRef = createPoolSynchronizer(100.seconds)
      val sender = stub[ConnectedPeer]
      actorRef ! DataFromPeer(TransactionMessageSpec.messageCode, tx, sender)

      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      actorRef.stop()
    }

    "broadcast one tx periodically" in {
      (utxStorage.all _).expects().returns(Seq(tx))

      val actorRef = createPoolSynchronizer(1 second)
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectMsg(3.seconds, NetworkController.SendToNetwork(ntwMsg, Broadcast))
      actorRef.stop()
    }
  }
}
