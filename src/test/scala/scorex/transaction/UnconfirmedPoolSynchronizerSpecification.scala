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

    val transactionModule = mock[TransactionModule]

    def createPoolSynchronizer(broadcastInterval: FiniteDuration) = {
      TestActorRef(new UnconfirmedPoolSynchronizer(transactionModule, UTXSettings(1000, broadcastInterval), testActor))
    }

    val defaultRecipient = new PublicKeyAccount(Array.fill(32)(0: Byte))
    val tx = GenesisTransaction.create(defaultRecipient, 149857264546L, 4598723454L).right.get

    "broadcast new transaction to network" in {

      (transactionModule.isValid(_: Transaction, _: Long)) expects(*, *) never()
      transactionModule.putUnconfirmedIfNew _ expects * returns true

      val actorRef = createPoolSynchronizer(100.seconds)
      val sender = stub[ConnectedPeer]
      actorRef ! DataFromPeer(TransactionMessageSpec.messageCode, tx, sender)

      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectMsg(NetworkController.SendToNetwork(ntwMsg, BroadcastExceptOf(sender)))
      actorRef.stop()
    }

    "not broadcast tx if it has been skipped" in {

      transactionModule.putUnconfirmedIfNew _ expects * returns false

      val actorRef = createPoolSynchronizer(100.seconds)
      val sender = stub[ConnectedPeer]
      actorRef ! DataFromPeer(TransactionMessageSpec.messageCode, tx, sender)

      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectNoMsg()
      actorRef.stop()
    }

    "broadcast one tx periodically" in {
      (transactionModule.unconfirmedTxs _).expects().returning(Seq(tx))

      val actorRef = createPoolSynchronizer(1 second)
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(tx), None)
      expectMsg(NetworkController.RegisterMessagesHandler(Seq(spec), actorRef))
      expectMsg(3.seconds, NetworkController.SendToNetwork(ntwMsg, Broadcast))
      actorRef.stop()
    }
  }
}
