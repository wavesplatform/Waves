package scorex.transaction

import scala.util.Try
import scorex.account.{Account, PrivateKeyAccount}
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.state.wallet.{BurnRequest, IssueRequest, Payment, ReissueRequest, TransferRequest}
import scorex.wallet.Wallet
import scorex.waves.transaction.SignedPayment

trait TransactionOperations {
  def transferAsset(request: TransferRequest, wallet: Wallet): Try[Either[ValidationError, TransferTransaction]]
  def issueAsset(request: IssueRequest, wallet: Wallet): Try[IssueTransaction]
  def reissueAsset(request: ReissueRequest, wallet: Wallet): Try[ReissueTransaction]
  def burnAsset(request: BurnRequest, wallet: Wallet): Try[BurnTransaction]

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Either[ValidationError, PaymentTransaction]
  def createPayment(payment: Payment, wallet: Wallet): Option[Either[ValidationError, PaymentTransaction]]
  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationError, PaymentTransaction]
  def signPayment(payment: Payment, wallet: Wallet): Option[Either[ValidationError, PaymentTransaction]]
  def broadcastPayment(payment: SignedPayment): Either[ValidationError, PaymentTransaction]
}
