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
  implicit def consensusModule: ConsensusModule[_]
  implicit def transactionModule: TransactionModule[_]

  def applicationName: String

  def appVersion: ApplicationVersion

  def basicMessagesSpecsRepo: BasicMessagesRepo

  def history: History

  def blockStorage: BlockStorage

  def peerManager: ActorRef

  def networkController: ActorRef

  def coordinator: ActorRef

  def blockGenerator: ActorRef

  def blockchainSynchronizer: ActorRef

  def scoreObserver: ActorRef

  def settings: Settings

  def wallet: Wallet
}
