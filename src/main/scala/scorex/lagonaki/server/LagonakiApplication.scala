package scorex.lagonaki.server

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http._
import scorex.app.Application
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.consensus.qora.QoraLikeConsensusModule
import scorex.consensus.qora.api.http.QoraConsensusApiRoute
import scorex.lagonaki.api.http.{PaymentApiRoute, PeersHttpService, ScorexApiRoute}
import scorex.network.message.Message
import scorex.network.{UnconfirmedPoolSynchronizer, Broadcast, TransactionalMessagesRepo, NetworkController}
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction._
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.wallet.Payment
import scorex.utils.NTP

import scala.reflect.runtime.universe._


class LagonakiApplication(val settingsFilename: String)
  extends Application {

  override val applicationName = "lagonaki"

  private val appConf = ConfigFactory.load().getConfig("app")

  override implicit val settings = new LagonakiSettings(settingsFilename)

  override implicit val consensusModule =
    appConf.getString("consensusAlgo") match {
      case s: String if s.equalsIgnoreCase("nxt") =>
        new NxtLikeConsensusModule
      case s: String if s.equalsIgnoreCase("qora") =>
        new QoraLikeConsensusModule
      case nonsense =>
        sys.error(s"Unknown consensus algo: $nonsense")
    }

  override implicit val transactionModule: SimpleTransactionModule = new SimpleTransactionModule

  override lazy val state: StoredState = transactionModule.state
  override lazy val history: BlockChain = transactionModule.history

  val consensusApiRoute = consensusModule match {
    case ncm: NxtLikeConsensusModule =>
      new NxtConsensusApiRoute(ncm, history)
    case qcm: QoraLikeConsensusModule =>
      new QoraConsensusApiRoute(qcm, history)
  }

  override lazy val apiRoutes = Seq(
    BlocksApiRoute(history, wallet),
    TransactionsApiRoute(state),
    consensusApiRoute,
    WalletApiRoute(wallet),
    PaymentApiRoute(this),
    ScorexApiRoute(this),
    SeedApiRoute(),
    PeersHttpService(this),
    AddressApiRoute(wallet, state)
  )

  override lazy val apiTypes = Seq(
    typeOf[BlocksApiRoute],
    typeOf[TransactionsApiRoute],
    consensusApiRoute match {
      case nxt: NxtConsensusApiRoute => typeOf[NxtConsensusApiRoute]
      case qora: QoraConsensusApiRoute => typeOf[QoraConsensusApiRoute]
    },
    typeOf[WalletApiRoute],
    typeOf[PaymentApiRoute],
    typeOf[ScorexApiRoute],
    typeOf[SeedApiRoute],
    typeOf[PeersHttpService],
    typeOf[AddressApiRoute]
  )

  //todo: is it needed at all ???
  override lazy val additionalSpecs = TransactionalMessagesRepo.specs

  //checks
  require(transactionModule.balancesSupport)
  require(transactionModule.accountWatchingSupport)

  //todo: move to Application?
  actorSystem.actorOf(Props(classOf[UnconfirmedPoolSynchronizer], this))


  //move away methods below?

  def onNewOffchainTransaction(transaction: LagonakiTransaction) =
    if (UnconfirmedTransactionsDatabaseImpl.putIfNew(transaction)) {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    }

  def createPayment(payment: Payment): Option[PaymentTransaction] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      createPayment(sender, new Account(payment.recipient), payment.amount, payment.fee)
    }
  }

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
    if (payment.validate() == ValidationResult.ValidateOke) {
      onNewOffchainTransaction(payment)
    }
    payment
  }
}