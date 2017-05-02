package scorex.app

import akka.actor.ActorRef
import com.wavesplatform.history.BlockStorageImpl
import com.wavesplatform.settings.WavesSettings
import scorex.transaction.{BlockStorage, History, TransactionModule, UnconfirmedTransactionsStorage}
import scorex.utils.Time
import scorex.wallet.Wallet


trait Application {

  implicit def transactionModule: TransactionModule

  def applicationName: String

  def appVersion: ApplicationVersion

  def blockStorage: BlockStorage

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

  def time: Time

}
