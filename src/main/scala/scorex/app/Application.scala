package scorex.app

import akka.actor.ActorRef
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import scorex.network.UPnP
import scorex.network.message.MessageHandler
import scorex.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet


trait Application {

  def messagesHandler: MessageHandler

  def peerManager: ActorRef

  def networkController: ActorRef

  def coordinator: ActorRef

  def blockGenerator: ActorRef

  def blockchainSynchronizer: ActorRef

  def scoreObserver: ActorRef

  def settings: WavesSettings

  def upnp: UPnP

  def wallet: Wallet

  def utxStorage: UnconfirmedTransactionsStorage

  def history: History

  def stateReader: StateReader

  def blockchainUpdater: BlockchainUpdater

  def checkpoints: CheckpointService

  def time: Time

}
