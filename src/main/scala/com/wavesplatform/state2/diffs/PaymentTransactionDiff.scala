package com.wavesplatform.state2.diffs

import cats.implicits._
import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.{ByteStr, Diff, LeaseInfo, Portfolio}
import scorex.account.Address
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{PaymentTransaction, ValidationError}

import scala.util.{Left, Right}

object PaymentTransactionDiff {

  def apply(stateReader: StateReader, height: Int, fn: Functionalities, blockTime: Long)
           (tx: PaymentTransaction): Either[ValidationError, Diff] = {

    stateReader.paymentTransactionIdByHash(ByteStr(tx.hash)) match {
      case Some(existing) if fn.requirePaymentUniqueIdAfter.check(blockTime).isRight => Left(GenericError(s"PaymentTx is already registered: $existing"))
      case _ => Right(Diff(height = height,
        tx = tx,
        portfolios = Map(
          tx.recipient -> Portfolio(
            balance = tx.amount,
            LeaseInfo.empty,
            assets = Map.empty)) combine Map(
          Address.fromPublicKey(tx.sender.publicKey) -> Portfolio(
            balance = -tx.amount - tx.fee,
            LeaseInfo.empty,
            assets = Map.empty
          )),
        paymentTransactionIdsByHashes = Map(ByteStr(tx.hash) -> tx.id)
      ))
    }
  }
}