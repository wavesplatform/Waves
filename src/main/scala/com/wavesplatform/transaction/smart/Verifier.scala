package com.wavesplatform.transaction.smart

import cats.implicits._
import com.wavesplatform.crypto
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.{Script, ScriptRunner}
import com.wavesplatform.transaction.transfer._
import shapeless.{:+:, CNil, Coproduct}

object Verifier {

  private type TxOrd = Transaction :+: Order :+: CNil

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    (tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, blockchain.accountScript(pt.sender)) match {
          case (et: ExchangeTransaction, scriptOpt) => verifyExchange(et, blockchain, scriptOpt, currentBlockHeight)
          case (_, Some(script))                    => verifyTx(blockchain, script, currentBlockHeight, pt, false)
          case (stx: SignedTransaction, None)       => stx.signaturesValid()
          case _                                    => verifyAsEllipticCurveSignature(pt)
        }
    }).flatMap(tx => {
      for {
        assetId <- tx match {
          case t: TransferTransaction     => t.assetId
          case t: MassTransferTransaction => t.assetId
          case t: BurnTransaction         => Some(t.assetId)
          case t: ReissueTransaction      => Some(t.assetId)
          case _                          => None
        }

        script <- blockchain.assetDescription(assetId).flatMap(_.script)
      } yield verifyTx(blockchain, script, currentBlockHeight, tx, true)
    }.getOrElse(Either.right(tx)))

  def verifyTx(blockchain: Blockchain,
               script: Script,
               height: Int,
               transaction: Transaction,
               isTokenScript: Boolean): Either[ValidationError, Transaction] = {
    ScriptRunner[Boolean](height, Coproduct[TxOrd](transaction), blockchain, script) match {
      case (ctx, Left(execError)) => Left(ScriptExecutionError(script.text, execError, ctx.letDefs, isTokenScript))
      case (ctx, Right(false)) =>
        Left(TransactionNotAllowedByScript(ctx.letDefs, script.text, isTokenScript))
      case (_, Right(true)) => Right(transaction)
    }
  }

  def verifyOrder(blockchain: Blockchain, script: Script, height: Int, order: Order): Either[ValidationError, Order] = {
    ScriptRunner[Boolean](height, Coproduct[TxOrd](order), blockchain, script) match {
      case (ctx, Left(execError)) => Left(ScriptExecutionError(script.text, execError, ctx.letDefs, true))
      case (ctx, Right(false)) =>
        Left(TransactionNotAllowedByScript(ctx.letDefs, script.text, true))
      case (_, Right(true)) => Right(order)
    }
  }

  def verifyExchange(et: ExchangeTransaction,
                     blockchain: Blockchain,
                     matcherScriptOpt: Option[Script],
                     height: Int): Either[ValidationError, Transaction] = {
    val AssetPair(amountAsset, priceAsset) = et.sellOrder.assetPair

    val amountAssetVerification =
      amountAsset
        .flatMap(blockchain.assetDescription)
        .flatMap(_.script)
        .map(verifyOrder(blockchain, _, height, et.sellOrder))
        .getOrElse(Right(()))

    val priceAssetVerification =
      priceAsset
        .flatMap(blockchain.assetDescription)
        .flatMap(_.script)
        .map(verifyOrder(blockchain, _, height, et.buyOrder))
        .getOrElse(Right(()))

    val sellerTxVerification =
      blockchain
        .accountScript(et.sellOrder.sender.toAddress)
        .map(verifyTx(blockchain, _, height, et, false))
        .getOrElse(Right(()))

    val buyerTxVerification =
      blockchain
        .accountScript(et.buyOrder.sender.toAddress)
        .map(verifyTx(blockchain, _, height, et, false))
        .getOrElse(Right(()))

    for {
      _ <- matcherScriptOpt match {
        case Some(script) => verifyTx(blockchain, script, height, et, false)
        case None         => et.signaturesValid()
      }
      _ <- sellerTxVerification
      _ <- buyerTxVerification
      _ <- amountAssetVerification
      _ <- priceAssetVerification
    } yield et
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}
