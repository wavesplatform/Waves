package com.wavesplatform.state.diffs

import cats.implicits._
import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.{ContractLimits, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.{ContractEvaluator, ContractResult}
import com.wavesplatform.lang.v1.traits.domain.Tx.ContractTransfer
import com.wavesplatform.lang.v1.traits.domain.{DataItem, Recipient}
import com.wavesplatform.lang.{ContentType, Global, ScriptType, StdLibVersion}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.CommonValidation._
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError._
import com.wavesplatform.transaction.smart.BlockchainContext.In
import com.wavesplatform.transaction.smart.script.ContractScript.ContractScriptImpl
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.{Script, ScriptRunner}
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, WavesEnvironment}
import com.wavesplatform.transaction.{Asset, ValidationError}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.util.{Failure, Success, Try}

object ContractInvocationTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: ContractInvocationTransaction): Either[ValidationError, Diff] = {
    val sc = blockchain.accountScript(tx.contractAddress)

    def evalContract(contract: Contract) = {
      val ctx = Monoid
        .combineAll(
          Seq(
            PureContext.build(StdLibVersion.V3),
            CryptoContext.build(Global),
            WavesContext.build(
              DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.Contract),
              new WavesEnvironment(AddressScheme.current.chainId, Coeval(tx.asInstanceOf[In]), Coeval(height), blockchain, Coeval(tx.contractAddress))
            )
          ))
        .evaluationContext

      val invoker                                       = tx.sender.toAddress.bytes
      val maybePayment: Option[(Long, Option[ByteStr])] = tx.payment.headOption.map(p => (p.amount, p.assetId.compatId))
      ContractEvaluator.apply(ctx, contract, ContractEvaluator.Invocation(tx.fc, invoker, maybePayment, tx.contractAddress.bytes))
    }

    sc match {
      case Some(ContractScriptImpl(_, contract, _)) =>
        val functionName = tx.fc.function.asInstanceOf[FunctionHeader.User].name
        contract.cfs.find(_.u.name == functionName) match {
          case None => Left(GenericError(s"No function '$functionName' at address ${tx.contractAddress}"))
          case Some(_) =>
            evalContract(contract).left
              .map(a => GenericError(a.toString): ValidationError)
              .flatMap {
                case ContractResult(ds, ps) =>
                  import cats.implicits._

                  val pmts: List[Map[Address, Map[Option[ByteStr], Long]]] = ps.map {
                    case (Recipient.Address(addrBytes), amt, maybeAsset) =>
                      Map(Address.fromBytes(addrBytes.arr).explicitGet() -> Map(maybeAsset -> amt))
                  }
                  for {
                    feeInfo <- (tx.assetFee._1 match {
                      case Waves => Right((tx.fee, Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty))))
                      case asset @ IssuedAsset(_) =>
                        for {
                          assetInfo <- blockchain
                            .assetDescription(asset)
                            .toRight(GenericError(s"Asset $asset does not exist, cannot be used to pay fees"))
                          wavesFee <- Either.cond(
                            assetInfo.sponsorship > 0,
                            Sponsorship.toWaves(tx.fee, assetInfo.sponsorship),
                            GenericError(s"Asset $asset is not sponsored, cannot be used to pay fees")
                          )
                        } yield {
                          (wavesFee,
                           Map(
                             tx.sender.toAddress        -> Portfolio(0, LeaseBalance.empty, Map(asset         -> -tx.fee)),
                             assetInfo.issuer.toAddress -> Portfolio(-wavesFee, LeaseBalance.empty, Map(asset -> tx.fee))
                           ))
                        }
                    })
                    wavesFee = feeInfo._1
                    dataAndPaymentDiff <- payableAndDataPart(height, tx, ds, feeInfo._2)
                    _                  <- Either.cond(pmts.flatMap(_.values).flatMap(_.values).forall(_ >= 0), (), ValidationError.NegativeAmount(-42, ""))
                    _                  <- validateOverflow(pmts.flatMap(_.values).flatMap(_.values), "Attempt to transfer unavailable funds in contract payment")
                    _ <- Either.cond(
                      pmts
                        .flatMap(_.values)
                        .flatMap(_.keys)
                        .flatten
                        .forall(id => blockchain.assetDescription(IssuedAsset(id)).isDefined),
                      (),
                      GenericError(s"Unissued assets are not allowed")
                    )
                    _ <- {
                      val totalScriptsInvoked =
                        tx.checkedAssets()
                          .collect { case asset @ IssuedAsset(_) => asset }
                          .count(blockchain.hasAssetScript) +
                          ps.count(_._3.fold(false)(id => blockchain.hasAssetScript(IssuedAsset(id))))
                      val minWaves = totalScriptsInvoked * ScriptExtraFee + FeeConstants(ContractInvocationTransaction.typeId) * FeeUnit
                      Either.cond(
                        minWaves <= wavesFee,
                        (),
                        GenericError(s"Fee in ${tx.assetFee._1
                          .fold("WAVES")(_.toString)} for ${tx.builder.classTag} with $totalScriptsInvoked total scripts invoked does not exceed minimal value of $minWaves WAVES: ${tx.assetFee._2}")
                      )
                    }
                    _ <- foldContractTransfers(blockchain, tx)(ps, dataAndPaymentDiff)
                  } yield {
                    val paymentReceiversMap: Map[Address, Portfolio] = Monoid
                      .combineAll(pmts)
                      .mapValues(mp => mp.toList.map(x => Portfolio.build(Asset.fromCompatId(x._1), x._2)))
                      .mapValues(l => Monoid.combineAll(l))
                    val paymentFromContractMap = Map(tx.contractAddress -> Monoid.combineAll(paymentReceiversMap.values).negate)
                    val transfers              = Monoid.combineAll(Seq(paymentReceiversMap, paymentFromContractMap))
                    dataAndPaymentDiff.combine(Diff.stateOps(portfolios = transfers))
                  }
              }
        }
      case _ => Left(GenericError(s"No contract at address ${tx.contractAddress}"))
    }

  }

  private def payableAndDataPart(height: Int, tx: ContractInvocationTransaction, ds: List[DataItem[_]], feePart: Map[Address, Portfolio]) = {
    val r: Seq[DataEntry[_]] = ds.map {
      case DataItem.Bool(k, b) => BooleanDataEntry(k, b)
      case DataItem.Str(k, b)  => StringDataEntry(k, b)
      case DataItem.Lng(k, b)  => IntegerDataEntry(k, b)
      case DataItem.Bin(k, b)  => BinaryDataEntry(k, b)
    }
    if (r.length > ContractLimits.MaxWriteSetSize) {
      Left(GenericError(s"WriteSec cann't content more than ${ContractLimits.MaxWriteSetSize} entries"))
    } else if (r.exists(_.key.getBytes().length > ContractLimits.MaxKeySize)) {
      Left(GenericError(s"Key size must be less than ${ContractLimits.MaxKeySize}"))
    } else {
      val totalDataBytes = r.map(_.toBytes.size).sum

      val payablePart: Map[Address, Portfolio] = (tx.payment
        .map {
          case ContractInvocationTransaction.Payment(amt, assetId) =>
            assetId match {
              case asset @ IssuedAsset(_) =>
                Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> -amt))).combine(
                  Map(tx.contractAddress -> Portfolio(0, LeaseBalance.empty, Map(asset -> amt)))
                )
              case Waves =>
                Map(tx.sender.toAddress -> Portfolio(-amt, LeaseBalance.empty, Map.empty))
                  .combine(Map(tx.contractAddress -> Portfolio(amt, LeaseBalance.empty, Map.empty)))
            }
        })
        .foldLeft(Map[Address, Portfolio]())(_ combine _)
      if (totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes)
        Right(
          Diff(
            height = height,
            tx = tx,
            portfolios = feePart combine payablePart,
            accountData = Map(tx.contractAddress -> AccountDataInfo(r.map(d => d.key -> d).toMap))
          ))
      else Left(GenericError(s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"))
    }
  }

  private def foldContractTransfers(blockchain: Blockchain, tx: ContractInvocationTransaction)(ps: List[(Recipient.Address, Long, Option[ByteStr])],
                                                                                               dataDiff: Diff): Either[ValidationError, Diff] = {
    if (ps.length <= ContractLimits.MaxPaymentAmount)
      ps.foldLeft(Either.right[ValidationError, Diff](dataDiff)) { (diffEi, payment) =>
        val (addressRepr, amount, asset) = payment
        val address                      = Address.fromBytes(addressRepr.bytes.arr).explicitGet()
        Asset.fromCompatId(asset) match {
          case Waves =>
            diffEi combine Right(
              Diff.stateOps(
                portfolios = Map(
                  address            -> Portfolio(amount, LeaseBalance.empty, Map.empty),
                  tx.contractAddress -> Portfolio(-amount, LeaseBalance.empty, Map.empty)
                )))
          case a @ IssuedAsset(_) =>
            diffEi combine {
              val nextDiff = Diff.stateOps(
                portfolios = Map(
                  address            -> Portfolio(0, LeaseBalance.empty, Map(a -> amount)),
                  tx.contractAddress -> Portfolio(0, LeaseBalance.empty, Map(a -> -amount))
                ))
              blockchain.assetScript(a) match {
                case None =>
                  Right(nextDiff)
                case Some(script) =>
                  diffEi flatMap (d => validateContractTransferWithSmartAssetScript(blockchain, tx)(d, addressRepr, amount, asset, nextDiff, script))
              }
            }
        }
      } else
      Left(GenericError(s"Too many ContractTransfers: max: ${ContractLimits.MaxPaymentAmount}, actual: ${ps.length}"))
  }

  private def validateContractTransferWithSmartAssetScript(blockchain: Blockchain, tx: ContractInvocationTransaction)(
      totalDiff: Diff,
      addressRepr: Recipient.Address,
      amount: Long,
      asset: Option[ByteStr],
      nextDiff: Diff,
      script: Script): Either[ValidationError, Diff] = {
    Try {
      ScriptRunner(
        blockchain.height,
        Coproduct[TxOrd](
          ContractTransfer(
            asset,
            Recipient.Address(tx.contractAddress.bytes),
            Recipient.Address(addressRepr.bytes),
            amount,
            tx.timestamp,
            tx.id()
          )),
        CompositeBlockchain.composite(blockchain, totalDiff),
        script,
        true,
        tx.contractAddress
      ) match {
        case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, true))
        case (log, Right(FALSE)) =>
          Left(TransactionNotAllowedByScript(log, isTokenScript = true))
        case (_, Right(TRUE)) => Right(nextDiff)
        case (_, Right(x))    => Left(GenericError(s"Script returned not a boolean result, but $x"))
      }
    } match {
      case Failure(e) =>
        Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, true))
      case Success(s) => s

    }
  }
}
