package scorex.api.http


import play.api.libs.json.{JsObject, JsValue}
import scorex.account.Account
import scorex.block.Block
import scorex.crypto.Base58
import scorex.transaction.History
import scorex.transaction.state.wallet.Wallet


//WALLET

case object WalletNotExist extends ApiError {
  override val id: Int = 201
  override val message: String = "wallet does not exist"
}

case object WalletAddressNotExists extends ApiError {
  override val id: Int = 202
  override val message: String = "address does not exist in wallet"
}

case object WalletLocked extends ApiError {
  override val id: Int = 203
  override val message: String = "wallet is locked"
}

case object WalletAlreadyExists extends ApiError {
  override val id: Int = 204
  override val message: String = "wallet already exists"
}

case object WalletSeedExportFailed extends ApiError {
  override val id: Int = 205
  override val message: String = "seed exporting failed"
}


//BLOCKS
case object BlockNotExists extends ApiError {
  override val id: Int = 301
  override val message: String = "block does not exist"
}

//TRANSACTIONS
case object TransactionNotExists extends ApiError {
  override val id: Int = 311
  override val message: String = "transactions does not exist"
}


case object NoBalance extends ApiError {
  override val id: Int = 2
  override val message: String = "not enough balance"
}

case object NegativeAmount extends ApiError {
  override val id: Int = 111
  override val message: String = "negative amount"
}

case object NegativeFee extends ApiError {
  override val id: Int = 112
  override val message: String = "negative fee"
}


trait CommonTransactionApiFunctions extends CommonApiFunctions {

  protected[api] def walletExists()(implicit wallet: Wallet): Option[JsObject] =
    if (wallet.exists()) Some(WalletAlreadyExists.json) else None

  protected[api] def withBlock(encodedSignature: String)
                              (action: Block => JsValue)
                              (implicit history: History): JsValue =
    Base58.decode(encodedSignature).toOption.map { signature =>
      history.blockById(signature) match {
        case Some(block) => action(block)
        case None => BlockNotExists.json
      }
    }.getOrElse(InvalidSignature.json)

  protected[api] def withPrivateKeyAccount(address: String)
                                          (action: Account => JsValue)
                                          (implicit wallet: Wallet): JsValue =
    walletNotExists().getOrElse {
      if (!Account.isValidAddress(address)) {
        InvalidAddress.json
      } else {
        wallet.privateKeyAccount(address) match {
          case None => WalletAddressNotExists.json
          case Some(account) => action(account)
        }
      }
    }

  protected[api] def walletNotExists()(implicit wallet: Wallet): Option[JsObject] =
    if (!wallet.exists()) Some(WalletNotExist.json) else None
}