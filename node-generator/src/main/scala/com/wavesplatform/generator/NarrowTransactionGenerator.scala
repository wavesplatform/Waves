package com.wavesplatform.generator

import java.nio.file.{Files, Paths}
import java.util.UUID
import java.util.concurrent.ThreadLocalRandom
import cats.Show
import com.wavesplatform.account.{KeyPair, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.generator.utils.{Gen, Universe}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.DataEntry.{MaxValueSize, Type}
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TransactionType.TransactionType
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.assets.*
import com.wavesplatform.transaction.assets.exchange.*
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer.*
import com.wavesplatform.transaction.utils.Signed
import com.wavesplatform.utils.{LoggerFacade, NTP}
import org.slf4j.LoggerFactory
import org.web3j.crypto.Bip32ECKeyPair

import scala.concurrent.duration.*
import scala.util.Random
import scala.util.Random.*

//noinspection ScalaStyle, TypeAnnotation
class NarrowTransactionGenerator(
    settings: NarrowTransactionGenerator.Settings,
    preconditions: NarrowTransactionGenerator.Preconditions,
    accounts: Seq[SeedKeyPair],
    estimator: ScriptEstimator,
    override val initial: Seq[Transaction],
    override val tailInitial: Seq[Transaction]
) extends TransactionGenerator {
  import NarrowTransactionGenerator.*

  private[this] val log     = LoggerFacade(LoggerFactory.getLogger(getClass))
  private[this] val typeGen = DistributedRandomGenerator(settings.probabilities)

  private[this] def correctVersion(v: TxVersion): TxVersion =
    if (settings.protobuf) (v + 1).toByte
    else v

  override def next(): Iterator[Transaction] = generate(settings.transactions).iterator

  private[this] def generate(n: Int): Seq[Transaction] = {
    val now = System.currentTimeMillis()

    val generated = (0 until (n * 1.2).toInt).foldLeft(
      (
        Seq.empty[Transaction],
        preconditions.tradeAsset.fold(Seq.empty[IssueTransaction])(Seq(_)),
        preconditions.tradeAsset.fold(Seq.empty[IssueTransaction])(Seq(_)),
        Universe.Leases,
        Seq.empty[CreateAliasTransaction]
      )
    ) { case ((allTxsWithValid, validIssueTxs, reissuableIssueTxs, activeLeaseTransactions, aliases), i) =>
      val timestamp = now + i

      val tx: Option[Transaction] = typeGen.getRandom match {
        case TransactionType.Issue =>
          val sender      = randomFrom(accounts).get
          val name        = random.nextString(5)
          val description = random.nextString(5)
          val reissuable  = random.nextBoolean()
          val amount      = 100000000L + Random.nextInt(Int.MaxValue)
          logOption(
            IssueTransaction
              .selfSigned(
                correctVersion(TxVersion.V2),
                sender,
                name,
                description,
                amount,
                Random.nextInt(9).toByte,
                reissuable,
                None,
                100400000L,
                timestamp
              )
          )

        case TransactionType.Transfer =>
          (
            for {
              (sender, asset) <- randomSenderAndAsset(validIssueTxs)
              useAlias = random.nextBoolean()
              recipient <- if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias) else randomFrom(accounts).map(_.toAddress)
              tx <- logOption(
                TransferTransaction
                  .selfSigned(
                    correctVersion(TxVersion.V2),
                    sender,
                    recipient,
                    Asset.fromCompatId(asset),
                    500,
                    Waves,
                    500000L,
                    createAttachment(),
                    timestamp
                  )
              )
            } yield tx
          ).logNone("There is no issued assets, may be you need to increase issue transaction's probability or pre-configure them")

        case TransactionType.Reissue =>
          (
            for {
              assetTx <- randomFrom(reissuableIssueTxs) orElse randomFrom(Universe.IssuedAssets.filter(_.reissuable))
              sender  <- accountByAddress(assetTx.sender.toAddress.toString)
              tx <- logOption(
                ReissueTransaction
                  .selfSigned(
                    correctVersion(TxVersion.V2),
                    sender,
                    IssuedAsset(assetTx.id()),
                    Random.nextInt(Int.MaxValue),
                    true,
                    100400000L,
                    timestamp
                  )
              )
            } yield tx
          ).logNone("There is no reissuable assets, may be you need to increase issue transaction's probability or pre-configure them")

        case TransactionType.Burn =>
          (
            for {
              assetTx <- randomFrom(validIssueTxs).orElse(randomFrom(Universe.IssuedAssets))
              sender  <- accountByAddress(assetTx.sender.toAddress.toString)
              tx <- logOption(
                BurnTransaction.selfSigned(
                  correctVersion(TxVersion.V2),
                  sender,
                  IssuedAsset(assetTx.id()),
                  Random.nextInt(1000),
                  500000L,
                  timestamp
                )
              )
            } yield tx
          ).logNone("There is no issued assets, may be you need to increase issue transaction's probability or pre-configure them")

        case TransactionType.Exchange =>
          (
            for {
              matcher <- randomFrom(Universe.Accounts).map(_.keyPair)
              seller  <- randomFrom(Universe.Accounts).map(_.keyPair)
              buyer   <- randomFrom(Universe.Accounts).map(_.keyPair)
              pair    <- preconditions.tradeAsset.map(a => AssetPair(Waves, IssuedAsset(a.id())))
              delta = random.nextLong(10000)
              sellOrder = Order
                .sell(
                  Order.V2,
                  seller,
                  matcher.publicKey,
                  pair,
                  10000000 + delta,
                  10,
                  timestamp,
                  timestamp + 30.days.toMillis,
                  300000L
                )
                .explicitGet()
              buyOrder = Order
                .buy(
                  Order.V2,
                  buyer,
                  matcher.publicKey,
                  pair,
                  10000000 + delta,
                  10 + random.nextLong(10),
                  timestamp,
                  timestamp + 1.day.toMillis,
                  300000L
                )
                .explicitGet()
              tx <- logOption(
                ExchangeTransaction.signed(
                  correctVersion(TxVersion.V2),
                  matcher.privateKey,
                  buyOrder,
                  sellOrder,
                  10000000L + delta,
                  10,
                  300000L,
                  300000L,
                  700000L,
                  timestamp
                )
              )
            } yield tx
          ).logNone("Can't define seller/matcher/buyer of transaction, check your configuration")

        case TransactionType.Lease =>
          (
            for {
              sender <- randomFrom(accounts)
              useAlias = random.nextBoolean()
              recipient <- (if (useAlias && aliases.nonEmpty) randomFrom(aliases.filter(_.sender != sender)).map(_.alias)
                            else randomFrom(accounts.filter(_ != sender).map(_.toAddress))) orElse Some(preconditions.leaseRecipient.toAddress)
              tx <- logOption(
                LeaseTransaction.selfSigned(correctVersion(TxVersion.V2), sender, recipient, random.nextLong(1, 100), 500000L, timestamp)
              )
            } yield tx
          ).logNone("Can't define recipient of transaction, check your configuration")

        case TransactionType.LeaseCancel =>
          (
            for {
              lease  <- activeLeaseTransactions.headOption
              sender <- accountByAddress(lease.sender.toAddress.toString)
              tx     <- logOption(LeaseCancelTransaction.selfSigned(2.toByte, sender, lease.id(), 500000L, timestamp))
            } yield tx
          ).logNone("There is no active lease transactions, may be you need to increase lease transaction's probability")

        case TransactionType.CreateAlias =>
          val sender      = randomFrom(accounts).get
          val aliasString = NarrowTransactionGenerator.generateAlias()
          logOption(
            CreateAliasTransaction.selfSigned(correctVersion(TxVersion.V2), sender, aliasString, 500000L, timestamp)
          )

        case TransactionType.MassTransfer =>
          (
            for {
              (sender, asset) <- randomSenderAndAsset(validIssueTxs)
              transferCount = random.nextInt(MassTransferTransaction.MaxTransferCount)
              transfers = for (_ <- 0 until transferCount) yield {
                val useAlias  = random.nextBoolean()
                val recipient = if (useAlias && aliases.nonEmpty) randomFrom(aliases).map(_.alias).get else randomFrom(accounts).get.toAddress
                val amount    = 1000 / (transferCount + 1)
                ParsedTransfer(recipient, TxNonNegativeAmount.unsafeFrom(amount))
              }
              tx <- logOption(
                MassTransferTransaction
                  .selfSigned(
                    correctVersion(TxVersion.V1),
                    sender,
                    Asset.fromCompatId(asset),
                    transfers.toList,
                    100000L + 50000L * transferCount + 400000L,
                    timestamp,
                    createAttachment()
                  )
              )
            } yield tx
          ).logNone("There is no issued assets, may be you need to increase issue transaction's probability or pre-configure them")

        case TransactionType.Data =>
          val sender = randomFrom(accounts).get
          val count  = random.nextInt(10)

          val data = for {
            _ <- 0 until count
            etype = random.nextInt(Type.maxId)
          } yield etype match {
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
          val fee  = 500000L * (size / 1024 + 1)
          logOption(DataTransaction.selfSigned(correctVersion(TxVersion.V1), sender, data.toList, fee, timestamp))

        case TransactionType.SponsorFee =>
          (
            for {
              assetTx <- randomFrom(validIssueTxs).orElse(randomFrom(Universe.IssuedAssets))
              sender  <- accountByAddress(assetTx.sender.toAddress.toString)
              tx <- logOption(
                SponsorFeeTransaction.selfSigned(
                  correctVersion(TxVersion.V1),
                  sender,
                  IssuedAsset(assetTx.id()),
                  Some(Random.nextInt(1000)),
                  100400000L,
                  timestamp
                )
              )
            } yield tx
          ).logNone("There is no issued assets, may be you need to increase issue transaction's probability or pre-configure them")

        case TransactionType.InvokeScript =>
          val script   = randomFrom(settings.scripts).get
          val function = randomFrom(script.functions).get
          val sender   = randomFrom(accounts).get
          val data = for {
            ScriptSettings.Function.Arg(argType, value) <- function.args
          } yield argType.toLowerCase match {
            case "integer" => Terms.CONST_LONG(value.toLong)
            case "string"  => Terms.CONST_STRING(value).explicitGet()
            case "boolean" => Terms.CONST_BOOLEAN(value.toBoolean)
            case "binary"  => Terms.CONST_BYTESTR(ByteStr.decodeBase58(value).get).explicitGet()
          }

          val maybeFunctionCall =
            if (function.name.isEmpty) None
            else Some(Terms.FUNCTION_CALL(FunctionHeader.User(function.name), data.toList))

          val asset = randomFrom(Universe.IssuedAssets.filter(a => script.paymentAssets.contains(a.name.toStringUtf8)))
            .fold(Waves: Asset)(tx => IssuedAsset(tx.id()))

          logOption(
            Right(
              Signed.invokeScript(
                correctVersion(TxVersion.V1),
                sender,
                GeneratorSettings.toKeyPair(script.dappAccount).toAddress,
                maybeFunctionCall,
                Seq(InvokeScriptTransaction.Payment(random.nextInt(5000), asset)),
                5300000L,
                Waves,
                timestamp
              )
            )
          )

        case TransactionType.Ethereum =>
          import EthTxGenerator.Arg

          val script   = randomFrom(settings.scripts).get
          val function = randomFrom(script.functions).get
          val sender   = randomFrom(accounts).get
          val ethArgs = for {
            ScriptSettings.Function.Arg(argType, value) <- function.args
          } yield argType.toLowerCase match {
            case "integer" | "int" | "long" | "int64" | "uint64" => Arg.Integer(value.toLong)
            case "bigint" | "int256" | "uint256"                 => Arg.BigInteger(BigInt(Base58.decode(value)))
            case "string"                                        => Arg.Str(value)
            case "boolean" | "bool"                              => Arg.Bool(value.toBoolean)
            case "binary"                                        => Arg.Bytes(ByteStr(Base58.decode(value)))
          }

          val asset = randomFrom(Universe.IssuedAssets.filter(a => script.paymentAssets.contains(a.name.toStringUtf8)))
            .fold(Waves: Asset)(tx => IssuedAsset(tx.id()))

          logOption(
            Right(
              EthTxGenerator.generateEthInvoke(
                Bip32ECKeyPair.generateKeyPair(sender.seed),
                GeneratorSettings.toKeyPair(script.dappAccount).toAddress,
                function.name,
                ethArgs,
                Seq(InvokeScriptTransaction.Payment(random.nextInt(5000), asset))
              )
            )
          )

        case TransactionType.SetScript =>
          for {
            sender <- randomFrom(preconditions.setScriptAccounts)
            script = Gen.script(complexity = false, estimator)
            tx <- logOption(
              SetScriptTransaction.selfSigned(
                correctVersion(TxVersion.V1),
                sender,
                Some(script),
                1400000L + random.nextLong(100),
                timestamp
              )
            )
          } yield tx

        case TransactionType.SetAssetScript =>
          (
            for {
              assetTx <- randomFrom(preconditions.setScriptAssets)
              sender  <- preconditions.setScriptAccounts.find(_.publicKey == assetTx.sender)
              script = Gen.script(complexity = false, estimator)
              tx <- logOption(
                SetAssetScriptTransaction.selfSigned(
                  correctVersion(TxVersion.V1),
                  sender,
                  IssuedAsset(assetTx.id()),
                  Some(script),
                  100400000L,
                  timestamp
                )
              )
            } yield tx
          ).logNone("There is no issued smart assets, may be you need to increase issue transaction's probability or pre-configure them")

        case _ => ???
      }

      (
        tx.fold(allTxsWithValid)(tx => allTxsWithValid :+ tx),
        tx match {
          case Some(tx: IssueTransaction) => validIssueTxs :+ tx
          case _                          => validIssueTxs
        },
        tx match {
          case Some(tx: IssueTransaction) if tx.reissuable    => reissuableIssueTxs :+ tx
          case Some(tx: ReissueTransaction) if !tx.reissuable => reissuableIssueTxs.filter(_.id() != tx.id())
          case _                                              => reissuableIssueTxs
        },
        tx match {
          case Some(tx: LeaseTransaction)       => activeLeaseTransactions :+ tx
          case Some(tx: LeaseCancelTransaction) => activeLeaseTransactions.filter(_.id() != tx.leaseId)
          case _                                => activeLeaseTransactions
        },
        tx match {
          case Some(tx: CreateAliasTransaction) => aliases :+ tx
          case _                                => aliases
        }
      )
    }

    Universe.Leases = generated._4

    log.trace(s"Distribution:\n${generated._1.groupBy(_.getClass).view.mapValues(_.size).mkString("\t", "\n\t", "")}")

    generated._1
  }

  private def createAttachment(): ByteStr = {
    if (random.nextBoolean()) ByteStr.empty
    else ByteStr(Array.fill(random.nextInt(100))(random.nextInt().toByte))
  }

  private[this] def logOption[T <: Transaction](txE: Either[ValidationError, T])(implicit m: Manifest[T]): Option[T] = {
    txE match {
      case Left(e) =>
        log.warn(s"${m.runtimeClass.getName}: ${e.toString}")
        None
      case Right(tx) => Some(tx)
    }
  }

  private[this] def accountByAddress(address: String): Option[KeyPair] =
    accounts
      .find(_.toAddress.toString == address)
      .orElse(Universe.Accounts.map(_.keyPair).find(_.toAddress.toString == address))

  private[this] def randomSenderAndAsset(issueTxs: Seq[IssueTransaction]): Option[(KeyPair, Option[ByteStr])] =
    if (random.nextBoolean()) {
      (randomFrom(issueTxs) orElse randomFrom(Universe.IssuedAssets)).map { issue =>
        val pk = (accounts ++ Universe.Accounts.map(_.keyPair)).find(_.publicKey == issue.sender).get
        (pk, Some(issue.id()))
      }
    } else randomFrom(accounts).map((_, None))

  private implicit class OptionExt[A](opt: Option[A]) {
    def logNone(msg: => String): Option[A] =
      opt match {
        case None =>
          log.warn(msg)
          None
        case Some(_) => opt
      }
  }
}

object NarrowTransactionGenerator {

  final case class ScriptSettings(
      dappAccount: String,
      paymentAssets: Set[String],
      functions: Seq[ScriptSettings.Function],
      scriptFile: Option[String]
  ) {
    def dappAccountKP = GeneratorSettings.toKeyPair(dappAccount)
    def dappAddress   = dappAccountKP.toAddress
  }
  object ScriptSettings {
    final case class Function(name: String, args: Seq[Function.Arg])
    object Function {
      final case class Arg(`type`: String, value: String)
    }
  }

  final case class SetScriptSettings(
      richAccount: String,
      accounts: SetScriptSettings.Accounts,
      assets: SetScriptSettings.Assets
  )

  object SetScriptSettings {
    final case class Accounts(balance: Long, scriptFile: String, repeat: Int)
    final case class Assets(description: String, amount: Long, decimals: Int, reissuable: Boolean, scriptFile: String, repeat: Int)
  }

  final case class Settings(
      transactions: Int,
      probabilities: Map[TransactionType, Double],
      scripts: Seq[ScriptSettings],
      setScript: Option[SetScriptSettings],
      protobuf: Boolean
  )

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._
      s"""transactions per iteration: $transactions
         |probabilities:
         |  ${probabilities.mkString("\n  ")}""".stripMargin
    }
  }

  final case class Preconditions(
      leaseRecipient: KeyPair,
      tradeAsset: Option[IssueTransaction],
      setScriptAccounts: Seq[KeyPair],
      setScriptAssets: Seq[IssueTransaction]
  )

  private val minAliasLength = 4
  private val maxAliasLength = 30
  private val aliasAlphabet  = "-.0123456789@_abcdefghijklmnopqrstuvwxyz".toVector

  def generateAlias(): String = {
    val len = Random.nextInt(maxAliasLength - minAliasLength) + minAliasLength
    Random.shuffle(aliasAlphabet).take(len).mkString
  }

  private def random = ThreadLocalRandom.current

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(random.nextInt(c.size))) else None

  def apply(settings: Settings, accounts: Seq[SeedKeyPair], time: NTP, estimator: ScriptEstimator): NarrowTransactionGenerator = {

    val (setScriptInitTxs, setScriptTailInitTxs, setScriptAccounts, setScriptAssets) =
      if (
        settings.probabilities
          .get(TransactionType.SetScript)
          .exists(_ > 0) || settings.probabilities.get(TransactionType.SetAssetScript).exists(_ > 0)
      ) {
        require(settings.setScript.isDefined, "SetScript and SetAssetScript generations require additional settings [set-script]")

        val accountsSettings = settings.setScript.get.accounts
        val assetsSettings   = settings.setScript.get.assets

        val richAccount = GeneratorSettings.toKeyPair(settings.setScript.get.richAccount)

        require(accountsSettings.repeat > 0, "[accounts.repeat] should be positive")
        require(assetsSettings.repeat > 0, "[assets.repeat] should be positive")

        val fee = 1500000L

        val (accountInitTxs, accountTailInitTxs, accounts) =
          ((1 to accountsSettings.repeat) foldLeft ((Seq.empty[Transaction], Seq.empty[Transaction], Seq.empty[KeyPair]))) {
            case ((initTxs, tailInitTxs, accounts), _) =>
              import accountsSettings._

              val account = GeneratorSettings.toKeyPair(s"${UUID.randomUUID().toString}")

              val transferTx = TransferTransaction
                .selfSigned(2.toByte, richAccount, account.toAddress, Waves, balance, Waves, fee, ByteStr.empty, time.correctedTime())
                .explicitGet()

              val script   = ScriptCompiler.compile(new String(Files.readAllBytes(Paths.get(scriptFile))), estimator).explicitGet()._1
              val scriptTx = SetScriptTransaction.selfSigned(TxVersion.V1, account, Some(script), fee, time.correctedTime()).explicitGet()

              (initTxs :+ transferTx, tailInitTxs :+ scriptTx, accounts :+ account)
          }

        val assetTailInitTxs =
          if (settings.probabilities.keySet.contains(TransactionType.SetAssetScript))
            ((1 to assetsSettings.repeat) foldLeft Seq.empty[IssueTransaction]) { case (txs, i) =>
              import assetsSettings._

              val issuer = randomFrom(accounts).get
              val script = ScriptCompiler.compile(new String(Files.readAllBytes(Paths.get(scriptFile))), estimator).explicitGet()._1

              val tx = IssueTransaction
                .selfSigned(
                  TxVersion.V2,
                  issuer,
                  UUID.randomUUID().toString.take(16),
                  s"$description #$i",
                  amount,
                  decimals.toByte,
                  reissuable,
                  Some(script),
                  100000000 + fee,
                  time.correctedTime()
                )
                .explicitGet()
              txs :+ tx
            }
          else Seq()

        (accountInitTxs, accountTailInitTxs ++ assetTailInitTxs, accounts, assetTailInitTxs)
      } else (Seq(), Seq(), Seq(), Seq())

    val (tradeAsset, tradeTailInitTxs) = if (settings.probabilities.keySet.contains(TransactionType.Exchange)) {

      val trader = randomFrom(accounts).get

      val tradeAsset = IssueTransaction
        .selfSigned(
          TxVersion.V2,
          trader,
          "TRADE",
          "Waves DEX is the best exchange ever",
          100000000,
          2,
          reissuable = true,
          fee = 100400000L,
          timestamp = System.currentTimeMillis(),
          script = None
        )
        .explicitGet()

      val tradeAssetDistribution: Seq[Transaction] = {
        (Universe.Accounts.map(_.keyPair).toSet - trader).toSeq.map(acc => {
          TransferTransaction
            .selfSigned(
              TxVersion.V2,
              trader,
              acc.toAddress,
              IssuedAsset(tradeAsset.id()),
              tradeAsset.quantity.value / Universe.Accounts.size,
              Waves,
              900000,
              ByteStr(Array.fill(random.nextInt(100))(random.nextInt().toByte)),
              System.currentTimeMillis()
            )
            .explicitGet()
        })
      }

      (Some(tradeAsset), tradeAssetDistribution)
    } else (None, Seq())

    val leaseRecipient = GeneratorSettings.toKeyPair("lease recipient")

    val fundEthereumAddresses = accounts.map { kp =>
      import com.wavesplatform.transaction.utils.EthConverters.*
      val ethAccount = kp.toEthWavesAddress
      TransferTransaction
        .selfSigned(TxVersion.V1, accounts.head, ethAccount, Waves, 100_0000_0000L, Waves, 500000L, ByteStr.empty, System.currentTimeMillis())
        .explicitGet()
    }

    val setPredefScripts = settings.scripts.collect {
      case s if s.scriptFile.nonEmpty =>
        val transferTx = TransferTransaction
          .selfSigned(2.toByte, accounts.head, s.dappAddress, Waves, 1_0000_000L, Waves, 500000L, ByteStr.empty, time.correctedTime())
          .explicitGet()

        val script   = ScriptCompiler.compile(new String(Files.readAllBytes(Paths.get(s.scriptFile.get))), estimator).explicitGet()._1
        val scriptTx = SetScriptTransaction.selfSigned(TxVersion.V1, s.dappAccountKP, Some(script), 500000L, time.correctedTime()).explicitGet()

        Seq(transferTx, scriptTx)
    }.flatten

    val initialTxs     = fundEthereumAddresses ++ setPredefScripts ++ tradeAsset.fold(Seq.empty[Transaction])(Seq(_)) ++ setScriptInitTxs
    val tailInitialTxs = tradeTailInitTxs ++ setScriptTailInitTxs
    val preconditions  = Preconditions(leaseRecipient, tradeAsset, setScriptAccounts, setScriptAssets)

    new NarrowTransactionGenerator(settings, preconditions, accounts, estimator, initialTxs, tailInitialTxs)
  }
}
