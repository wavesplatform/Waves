package scorex.transaction

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.network.NetworkController.DataFromPeer
import scorex.network.TransactionalMessagesRepo.TransactionMessageSpec
import scorex.network.message.Message
import scorex.network.{NetworkController, _}
import scorex.transaction.SimpleTransactionModule.StoredInBlock

import scala.concurrent.duration._

class UnconfirmedPoolSynchronizerSpecification extends TestKit(ActorSystem("UnconfirmedPoolSynchronizerSpecification"))
  with ImplicitSender with WordSpecLike with Matchers with MockFactory
  with BeforeAndAfterAll
  with OneInstancePerTest {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  private object MyTxSettings extends TransactionSettings {
    override val settingsJSON: JsObject = Json.obj()
    override lazy val utxRebroadcastInterval = 1.seconds
  }

  "An UnconfirmedPoolSynchronizer actor" must {

    val defaultRecipient = new PublicKeyAccount(Array.fill(32)(0: Byte))
    val tx = new GenesisTransaction(defaultRecipient, 149857264546L, 4598723454L)

    "broadcast new transaction to network" in {

      val transactionModule = mock[TransactionModule[StoredInBlock]]
      inSequence{
        (transactionModule.isValid(_:Transaction)) expects(*) returning(true)
        (transactionModule.putUnconfirmedIfNew(_:Transaction)) expects(*) returning(true)
      }

      // do
      val settings = new TransactionSettings {
        override val settingsJSON: JsObject = Json.obj()
        override lazy val utxRebroadcastInterval = 100.seconds
      }
      val actorRef = TestActorRef(new UnconfirmedPoolSynchronizer(transactionModule, settings, testActor))
      actorRef ! DataFromPeer(TransactionMessageSpec.messageCode, tx, ConnectedPeer(null, null))

      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectMsg(NetworkController.SendToNetwork(ntwMsg, Broadcast))
      actorRef.stop()
    }

    "broadcast one tx periodically" in {
      val transactionModule = mock[TransactionModule[StoredInBlock]]
     (transactionModule.unconfirmedTxs: () => Seq[Transaction]).expects().returning(Seq(tx))

      val actorRef = TestActorRef(new UnconfirmedPoolSynchronizer(transactionModule, MyTxSettings, testActor))
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectMsg(3.seconds, NetworkController.SendToNetwork(ntwMsg, Broadcast))
      actorRef.stop()
    }
  }
}
