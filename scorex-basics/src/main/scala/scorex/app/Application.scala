package scorex.app

import akka.actor.ActorRef
import scorex.consensus.ConsensusModule
import scorex.network.message.BasicMessagesRepo
import scorex.settings.Settings
import scorex.transaction.{BlockStorage, History, TransactionModule}
import scorex.wallet.Wallet

/**
  * Pure interface to application
  */
trait Application {

  //modules
  implicit val consensusModule: ConsensusModule[_]
  implicit val transactionModule: TransactionModule[_]

  def applicationName: String

  //redefine it as lazy val
  def appVersion: ApplicationVersion

  val basicMessagesSpecsRepo: BasicMessagesRepo

  val history: History

  val blockStorage: BlockStorage

  val peerManager: ActorRef

  val networkController: ActorRef

  val coordinator: ActorRef

  val blockGenerator: ActorRef

  val blockchainSynchronizer: ActorRef

  val scoreObserver: ActorRef

  val settings: Settings

  val wallet: Wallet
}
