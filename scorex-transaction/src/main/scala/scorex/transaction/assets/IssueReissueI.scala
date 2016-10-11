package scorex.transaction.assets

import scorex.transaction.SignedTransaction

trait IssueReissueI extends SignedTransaction {

  val assetId: Array[Byte]
  val reissuable: Boolean
}
