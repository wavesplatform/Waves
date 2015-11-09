package scorex.api.http

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

