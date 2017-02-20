package scorex.app

import akka.actor.ActorRef
import com.wavesplatform.settings.WavesSettings
import scorex.consensus.ConsensusModule
import scorex.network.message.BasicMessagesRepo
import scorex.transaction.{BlockStorage, History, TransactionModule}
import scorex.wallet.Wallet

/**
  * Pure interface to application
  */
trait Application {

  //modules
  implicit def consensusModule: ConsensusModule
  implicit def transactionModule: TransactionModule

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

  def settings: WavesSettings

  def wallet: Wallet
}
