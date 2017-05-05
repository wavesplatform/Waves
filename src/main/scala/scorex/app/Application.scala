package scorex.app

import akka.actor.ActorRef
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet


trait Application {

  def peerManager: ActorRef

  def networkController: ActorRef

  def coordinator: ActorRef

  def blockGenerator: ActorRef

  def blockchainSynchronizer: ActorRef

  def scoreObserver: ActorRef

  def settings: WavesSettings

  def wallet: Wallet

  def utxStorage: UnconfirmedTransactionsStorage

  def history: History

  def stateReader: StateReader

  def blockchainUpdater: BlockchainUpdater

  def checkpoints: CheckpointService

  def time: Time

}
