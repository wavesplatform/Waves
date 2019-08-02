package com.wavesplatform.generator

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.wavesplatform.account.{AddressScheme, Alias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.generator.NarrowTransactionGenerator.{ScriptSettings, Settings}
import com.wavesplatform.generator.utils.Universe
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.DataEntry.{MaxValueSize, Type}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseCancelTransactionV2, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.LoggerFacade
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.util.Random

class NarrowTransactionGenerator(settings: Settings, val accounts: Seq[KeyPair]) extends TransactionGenerator {
  private[this] val log     = LoggerFacade(LoggerFactory.getLogger(getClass))
  private[this] val typeGen = DistributedRandomGenerator(settings.probabilities)

  override def next(): Iterator[Transaction] =
    generate(settings.transactions).toIterator

  def generate(n: Int): Seq[Transaction] = {
    //noinspection ScalaStyle,TypeAnnotation
    object preconditions {
      val issueTransactionSender = randomFrom(accounts).get

      val tradeAssetIssue = IssueTransactionV2
        .selfSigned(
          AddressScheme.current.chainId,
          issueTransactionSender,
          "TRADE".getBytes("UTF-8"),
          "Waves DEX is the best exchange ever".getBytes("UTF-8"),
          100000000,
          2,
          reissuable = false,
          fee = 100400000L,
          timestamp = System.currentTimeMillis(),
          script = None
        )
        .right
        .get

      val tradeAssetDistribution = {
        tradeAssetIssue +: accounts.map(acc => {
          TransferTransactionV2
            .selfSigned(
              IssuedAsset(tradeAssetIssue.id()),
              issueTransactionSender,
              acc,
              5,
              System.currentTimeMillis(),
              Waves,
              900000,
              Array.fill(random.nextInt(100))(random.nextInt().toByte)
            )
            .right
            .get
        })
      }
    }

    val now = System.currentTimeMillis()

    val generated = (0 until (n * 1.2).toInt).foldLeft(
      (
        Seq.empty[Transaction],
        Seq.empty[IssueTransactionV2],
        Seq.empty[IssueTransactionV2],
        Seq.empty[LeaseTransactionV2],
        Seq.empty[CreateAliasTransaction]
      )) {
      case ((allTxsWithValid, validIssueTxs, reissuableIssueTxs, activeLeaseTransactions, aliases), i) =>
        val moreThanStandardFee = 100000L + 800000

        val ts = now + i

        val tx = typeGen.getRandom match {
          case IssueTransactionV2 =>
            val sender      = randomFrom(accounts).get
            val name        = new Array[Byte](10)
            val description = new Array[Byte](10)
            random.nextBytes(name)
            random.nextBytes(description)
            val reissuable = random.nextBoolean()
            val amount     = 100000000L + Random.nextInt(Int.MaxValue)
            logOption(
              IssueTransactionV2
                .selfSigned(AddressScheme.current.chainId,
                            sender,
                            name,
                            description,
                            amount,
                            Random.nextInt(9).toByte,
                            reissuable,
                            None,
                       100400000L,
                            ts))
          case TransferTransactionV2 =>
            val useAlias  = random.nextBoolean()
            val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
            val sendAsset = random.nextBoolean()
            val senderAndAssetOpt = if (sendAsset) {
              val asset = randomFrom(validIssueTxs)
              asset.map(issue => {
                val pk = accounts.find(_.publicKey == issue.sender).get
                (pk, Some(issue.id()))
              })
            } else Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  TransferTransactionV2
                    .selfSigned(
                      Asset.fromCompatId(asset),
                      sender,
                      recipient,
                      500,
                      ts,
                      Waves,
                      moreThanStandardFee,
                      Array.fill(random.nextInt(100))(random.nextInt().toByte)
                    ))
            }
          case ReissueTransactionV2 =>
            val reissuable = random.nextBoolean()
            randomFrom(reissuableIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.addressString == assetTx.sender.addressString).get
              logOption(
                ReissueTransactionV2.selfSigned(AddressScheme.current.chainId,
                                                sender,
                                                IssuedAsset(assetTx.id()),
                                                Random.nextInt(Int.MaxValue),
                                                reissuable,
                                           100400000L,
                                                ts))
            })
          case BurnTransactionV2 =>
            randomFrom(validIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.addressString == assetTx.sender.addressString).get
              logOption(
                BurnTransactionV2.selfSigned(AddressScheme.current.chainId, sender, IssuedAsset(assetTx.id()), Random.nextInt(1000), 900000L, ts))
            })
          case ExchangeTransactionV1 =>
            val matcher   = randomFrom(accounts).get
            val seller    = randomFrom(accounts).get
            val pair      = AssetPair(Waves, IssuedAsset(preconditions.tradeAssetIssue.id()))
            val sellOrder = OrderV1.sell(seller, matcher, pair, 1, 100000000, ts, ts + 30.days.toMillis, moreThanStandardFee * 3)
            val buyer     = randomFrom(accounts).get
            val buyOrder  = OrderV1.buy(buyer, matcher, pair, 1, 100000000, ts, ts + 1.day.toMillis, moreThanStandardFee * 3)
            logOption(ExchangeTransactionV1.create(matcher, buyOrder, sellOrder, 1, 100000000, 300000, 300000, moreThanStandardFee * 3, ts))
          case ExchangeTransactionV2 =>
            val matcher = randomFrom(accounts).get
            val seller  = randomFrom(accounts).get
            val pair    = AssetPair(Waves, IssuedAsset(preconditions.tradeAssetIssue.id()))
            // XXX generate order version
            val sellOrder = OrderV2.sell(seller, matcher, pair, 100000000, 1, ts, ts + 30.days.toMillis, moreThanStandardFee * 3)
            val buyer     = randomFrom(accounts).get
            val buyOrder  = OrderV2.buy(buyer, matcher, pair, 100000000, 1, ts, ts + 1.day.toMillis, moreThanStandardFee * 3)
            logOption(ExchangeTransactionV2.create(matcher, buyOrder, sellOrder, 100000000, 1, 300000, 300000, moreThanStandardFee * 3, ts))
          case LeaseTransactionV2 =>
            val sender   = randomFrom(accounts).get
            val useAlias = random.nextBoolean()
            val recipientOpt =
              if (useAlias && aliases.nonEmpty) randomFrom(aliases.filter(_.sender != sender)).map(_.alias)
              else randomFrom(accounts.filter(_ != sender).map(_.toAddress))
            recipientOpt.flatMap(recipient => logOption(LeaseTransactionV2.selfSigned(sender, 1, moreThanStandardFee * 3, ts, recipient)))
          case LeaseCancelTransactionV2 =>
            randomFrom(activeLeaseTransactions).flatMap(lease => {
              val sender = accounts.find(_.addressString == lease.sender.addressString).get
              logOption(LeaseCancelTransactionV2.selfSigned(AddressScheme.current.chainId, sender, lease.id(), moreThanStandardFee * 3, ts))
            })
          case CreateAliasTransactionV2 =>
            val sender      = randomFrom(accounts).get
            val aliasString = NarrowTransactionGenerator.generateAlias()
            logOption(CreateAliasTransactionV2.selfSigned(sender, Alias.create(aliasString).explicitGet(), 500000, ts))
          case MassTransferTransaction =>
            val transferCount = random.nextInt(MassTransferTransaction.MaxTransferCount)
            val transfers = for (i <- 0 to transferCount) yield {
              val useAlias  = random.nextBoolean()
              val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
              val amount    = random.nextLong(500000)
              ParsedTransfer(recipient, amount)
            }
            val sendAsset = random.nextBoolean()
            val senderAndAssetOpt = if (sendAsset) {
              val asset = randomFrom(validIssueTxs)
              asset.map(issue => {
                val pk = accounts.find(_.publicKey == issue.sender).get
                (pk, Some(issue.id()))
              })
            } else Some((randomFrom(accounts).get, None))
            senderAndAssetOpt.flatMap {
              case (sender, asset) =>
                logOption(
                  MassTransferTransaction
                    .selfSigned(Asset.fromCompatId(asset),
                                sender,
                                transfers.toList,
                                ts,
                                200000 + 50000 * transferCount,
                                Array.fill(random.nextInt(100))(random.nextInt().toByte)))
            }
          case DataTransaction =>
            val sender = randomFrom(accounts).get
            val count  = random.nextInt(10)

            val data = for {
              _ <- 0 until count
              etype = random.nextInt(Type.maxId)
            } yield
              etype match {
                case t if t == Type.Integer.id => IntegerDataEntry(Random.nextString(10), random.nextLong)
                case t if t == Type.Boolean.id => BooleanDataEntry(Random.nextString(10), random.nextBoolean)
                case t if t == Type.String.id  => StringDataEntry(Random.nextString(10), random.nextLong.toString)
                case t if t == Type.Binary.id =>
                  val size = random.nextInt(MaxValueSize + 1)
                  val b    = new Array[Byte](size)
                  random.nextBytes(b)
                  BinaryDataEntry(Random.nextString(10), ByteStr(b))
              }
            val size = 128 + data.map(_.toBytes.length).sum
            val fee  = 100000 * (size / 1024 + 1)
            logOption(DataTransaction.selfSigned(sender, data.toList, fee, ts))
          case SponsorFeeTransaction =>
            randomFrom(validIssueTxs).flatMap(assetTx => {
              val sender = accounts.find(_.addressString == assetTx.sender.addressString).get
              logOption(SponsorFeeTransaction.selfSigned(sender, IssuedAsset(assetTx.id()), Some(Random.nextInt(1000)), 100400000L, ts))
            })

          case InvokeScriptTransaction =>
            val script   = randomFrom(settings.scripts).get
            val function = randomFrom(script.functions).get
            val sender   = randomFrom(accounts).get
            val data = for {
              ScriptSettings.Function.Arg(argType, value) <- function.args
            } yield
              argType.toLowerCase match {
                case "integer" => Terms.CONST_LONG(value.toLong)
                case "string"  => Terms.CONST_STRING(value.toString).explicitGet()
                case "boolean" => Terms.CONST_BOOLEAN(value.toBoolean)
                case "binary"  => Terms.CONST_BYTESTR(Base58.decode(value)).explicitGet()
              }

            val maybeFunctionCall =
              if (function.name.isEmpty) None
              else Some(Terms.FUNCTION_CALL(FunctionHeader.User(function.name), data.toList))

            val asset = randomFrom(Universe.IssuedAssets.filter(a => script.paymentAssets.contains(new String(a.name))))
              .fold(Waves: Asset)(tx => IssuedAsset(tx.id()))

            logOption(
              InvokeScriptTransaction.selfSigned(
                sender,
                GeneratorSettings.toKeyPair(script.dappAccount).toAddress,
                maybeFunctionCall,
                Seq(InvokeScriptTransaction.Payment(random.nextInt(5000), asset)),
                5300000L,
                Waves,
                ts
              ))
        }

        (tx.map(tx => allTxsWithValid :+ tx).getOrElse(allTxsWithValid), tx match {
          case Some(tx: IssueTransactionV2) => validIssueTxs :+ tx
          case _                            => validIssueTxs
        }, tx match {
          case Some(tx: IssueTransactionV2) if tx.reissuable  => reissuableIssueTxs :+ tx
          case Some(tx: ReissueTransaction) if !tx.reissuable => reissuableIssueTxs.filter(_.id() != tx.id())
          case _                                              => reissuableIssueTxs
        }, tx match {
          case Some(tx: LeaseTransactionV2)     => activeLeaseTransactions :+ tx
          case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id() != tx.leaseId)
          case _                                => activeLeaseTransactions
        }, tx match {
          case Some(tx: CreateAliasTransaction) => aliases :+ tx
          case _                                => aliases
        })
    }

    generated._1
  }

  private[this] def random = ThreadLocalRandom.current

  private[this] def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(random.nextInt(c.size))) else None

  private[this] def logOption[T <: Transaction](txE: Either[ValidationError, T])(implicit m: Manifest[T]): Option[T] = {
    txE match {
      case Left(e) =>
        log.warn(s"${m.runtimeClass.getName}: ${e.toString}")
        None
      case Right(tx) => Some(tx)
    }
  }
}

object NarrowTransactionGenerator {
  final case class ScriptSettings(dappAccount: String, paymentAssets: Set[String], functions: Seq[ScriptSettings.Function])
  object ScriptSettings {
    final case class Function(name: String, args: Seq[Function.Arg])
    object Function {
      final case class Arg(`type`: String, value: String)
    }
  }

  final case class Settings(transactions: Int, probabilities: Map[TransactionParser, Double], scripts: Seq[ScriptSettings])

  private val minAliasLength = 4
  private val maxAliasLength = 30
  private val aliasAlphabet  = "-.0123456789@_abcdefghijklmnopqrstuvwxyz".toVector

  def generateAlias(): String = {
    val len = Random.nextInt(maxAliasLength - minAliasLength) + minAliasLength
    Random.shuffle(aliasAlphabet).take(len).mkString
  }

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

}
