package com.wavesplatform.state.diffs

import cats.kernel.Monoid
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.lang.{Global, Version}
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.script.v1.ContractScript
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, WavesEnvironment}
import monix.eval.Coeval

object ContractInvocationTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: ContractInvocationTransaction): Either[ValidationError, Diff] = {
    val sc = blockchain.accountScript(tx.contractAddress)
    sc match {
      case Some(ContractScript(_, contract)) =>
        val functionName = tx.fc.function.asInstanceOf[FunctionHeader.User].name
        contract.cfs.find(_.u.name == functionName) match {
          case None => Left(GenericError(s"No function '$functionName' at address ${tx.contractAddress}"))
          case Some(_) =>
            val ctx = Monoid
              .combineAll(
                Seq(
                  PureContext.build(Version.ContractV),
                  CryptoContext.build(Global),
                  WavesContext.build(Version.ContractV,
                                     new WavesEnvironment(AddressScheme.current.chainId, Coeval(tx.asInstanceOf[In]), Coeval(height), blockchain),
                                     false)
                ))
              .evaluationContext

            val invoker                                       = tx.sender.toAddress.bytes
            val maybePayment: Option[(Long, Option[ByteStr])] = tx.payment.map(p => (p.amount, p.assetId))
            val res =
              ContractEvaluator.apply(ctx, contract, ContractEvaluator.Invokation(tx.fc, invoker, maybePayment, tx.contractAddress.bytes))
            res.left
              .map(a => GenericError(a.toString): ValidationError)
              .flatMap {
                case ContractResult(ds, ps) => {
                  val r: Seq[DataEntry[_]] = ds.map {
                    case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
                    case DataItem.Str(k, b)  => StringDataEntry(k, b)
                    case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
                    case DataItem.Bin(k, b)  => BinaryDataEntry(k, b)
                  }
                  val pmts: List[Map[Address, Map[Option[ByteStr], Long]]] = ps.map {
                    case (Recipient.Address(addrBytes), amt, maybeAsset) =>
                      Map(Address.fromBytes(addrBytes.arr).explicitGet() -> Map(maybeAsset -> amt))
                  }
                  for {
                    _ <- Either.cond(pmts.flatMap(_.values).flatMap(_.values).forall(_ >= 0), (), ValidationError.NegativeAmount(-42, ""))
                    _ <- Either.cond(
                      pmts
                        .flatMap(_.values)
                        .flatMap(_.keys)
                        .flatten
                        .forall(blockchain.assetDescription(_).isDefined),
                      (),
                      GenericError(s"Unissued assets are not allowed")
                    )
                    _ <- Either.cond(true, (), ValidationError.NegativeAmount(-42, "")) //  - sum doesn't overflow
                    _ <- Either.cond(true, (), ValidationError.NegativeAmount(-42, "")) //  - whatever else tranfser/massTransfer ensures
                  } yield {
                    import cats.implicits._
                    val paymentReceiversMap: Map[Address, Portfolio] = Monoid
                      .combineAll(pmts)
                      .mapValues(mp => mp.toList.map(x => Portfolio.build(x._1, x._2)))
                      .mapValues(l => Monoid.combineAll(l))
                    val feePart = Map(
                      tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)
                    )
                    val paymentFromContractMap = Map(tx.contractAddress -> Monoid.combineAll(paymentReceiversMap.values).negate)

                    val payablePart: Map[Address, Portfolio] = tx.payment match {
                      case None => Map.empty
                      case Some(ContractInvocationTransaction.Payment(amt, assetOpt)) =>
                        assetOpt match {
                          case Some(asset) =>
                            Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))).combine(
                              Map(tx.contractAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                            )
                          case None =>
                            Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty))
                              .combine(Map(tx.contractAddress -> Portfolio(amt, LeaseBalance.empty, Map.empty)))
                        }
                    }
                    val transfers = Monoid.combineAll(Seq(payablePart, feePart, paymentReceiversMap, paymentFromContractMap))
                    Diff(
                      height = height,
                      tx = tx,
                      portfolios = transfers,
                      accountData = Map(tx.contractAddress -> AccountDataInfo(r.map(d => d.key -> d).toMap))
                    )
                  }
                }
              }
        }
      case _ => Left(GenericError(s"No contract at address ${tx.contractAddress}"))
    }

  }
}
