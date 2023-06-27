package com.wavesplatform.generator

import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Transaction, TxVersion}
import com.wavesplatform.utils.Time
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.collection.Factory

object Preconditions {
  private[this] val Fee = 1500000L

  sealed abstract class PAction(val priority: Int) extends Product with Serializable

  final case class LeaseP(from: KeyPair, to: Address, amount: Long, repeat: Option[Int]) extends PAction(3)
  final case class IssueP(name: String, issuer: KeyPair, desc: String, amount: Long, decimals: Int, reissuable: Boolean, scriptFile: String)
      extends PAction(2)
  final case class CreateAccountP(seed: String, balance: Long, scriptFile: Option[String]) extends PAction(1)

  final case class PGenSettings(faucet: KeyPair, actions: List[PAction])

  final case class CreatedAccount(keyPair: KeyPair, balance: Long, script: Option[Script])

  final case class UniverseHolder(
      accounts: List[CreatedAccount] = Nil,
      issuedAssets: List[IssueTransaction] = Nil,
      leases: List[LeaseTransaction] = Nil
  )

  def mk(settings: PGenSettings, time: Time, estimator: ScriptEstimator): (UniverseHolder, List[Transaction], List[Transaction]) = {
    val (holder, headTransactions) = settings.actions
      .sortBy(_.priority)(Ordering[Int].reverse)
      .foldLeft((UniverseHolder(), List.empty[Transaction])) { case ((uni, txs), action) =>
        action match {
          case LeaseP(from, to, amount, repeat) =>
            val newTxs = (1 to repeat.getOrElse(1)).map { _ =>
              LeaseTransaction
                .selfSigned(2.toByte, from, to, amount, Fee, time.correctedTime())
                .explicitGet()
            }.toList
            (uni.copy(leases = newTxs ::: uni.leases), newTxs ::: txs)

          case IssueP(assetName, issuer, assetDescription, amount, decimals, reissuable, scriptFile) =>
            val script = Option(scriptFile)
              .filter(_.nonEmpty)
              .map(file => ScriptCompiler.compile(new String(Files.readAllBytes(Paths.get(file))), estimator))
              .flatMap(_.toOption)
              .map(_._1)

            val tx = IssueTransaction
              .selfSigned(
                TxVersion.V2,
                issuer,
                assetName,
                assetDescription,
                amount,
                decimals.toByte,
                reissuable,
                script,
                100000000 + Fee,
                time.correctedTime()
              )
              .explicitGet()
            (uni.copy(issuedAssets = tx :: uni.issuedAssets), tx :: txs)

          case CreateAccountP(seed, balance, scriptOption) =>
            val acc = GeneratorSettings.toKeyPair(seed)
            val transferTx = TransferTransaction
              .selfSigned(2.toByte, settings.faucet, acc.toAddress, Waves, balance, Waves, Fee, ByteStr.empty, time.correctedTime())
              .explicitGet()
            val scriptAndTx = scriptOption.map { file =>
              val scriptText = new String(Files.readAllBytes(Paths.get(file)))
              val script     = ScriptCompiler.compile(scriptText, estimator).explicitGet()._1
              val tx         = SetScriptTransaction.selfSigned(1.toByte, acc, Some(script), Fee, time.correctedTime()).explicitGet()
              (script, tx)
            }

            val addTxs = List(transferTx) ++ scriptAndTx.map(_._2)
            (uni.copy(accounts = CreatedAccount(acc, balance, scriptAndTx.map(_._1)) :: uni.accounts), addTxs ::: txs)
        }
      }

    val tailTransactions = holder.issuedAssets.flatMap { issuedAsset =>
      val balance = issuedAsset.quantity.value / holder.accounts.size
      holder.accounts.map { acc =>
        TransferTransaction
          .selfSigned(
            2.toByte,
            settings.faucet,
            acc.keyPair.toAddress,
            IssuedAsset(issuedAsset.assetId),
            balance,
            Waves,
            Fee,
            ByteStr.empty,
            time.correctedTime()
          )
          .explicitGet()
      }
    }

    (holder, headTransactions, tailTransactions)
  }

  private val accountSectionReader = new ValueReader[CreateAccountP] {
    override def read(config: Config, path: String): CreateAccountP = {
      val conf = config.getConfig(path)

      val seed       = conf.as[String]("seed")
      val balance    = conf.as[Long]("balance")
      val scriptFile = conf.as[Option[String]]("script-file").filter(_.nonEmpty)

      CreateAccountP(seed, balance, scriptFile)
    }
  }

  private val leasingSectionReader = new ValueReader[LeaseP] {
    override def read(config: Config, path: String): LeaseP = {
      val conf = config.getConfig(path)

      val from   = conf.as[String]("from")
      val to     = conf.as[String]("to")
      val amount = conf.as[Long]("amount")
      val repeat = conf.as[Option[Int]]("repeat")

      LeaseP(
        GeneratorSettings.toKeyPair(from),
        GeneratorSettings.toKeyPair(to).toAddress,
        amount,
        repeat
      )
    }
  }

  private val assetSectionReader = new ValueReader[IssueP] {
    override def read(config: Config, path: String): IssueP = {
      val conf   = config.getConfig(path)
      val issuer = GeneratorSettings.toKeyPair(conf.getString("issuer"))

      val name        = conf.as[Option[String]]("name").getOrElse("")
      val description = conf.as[Option[String]]("description").getOrElse("")
      val amount      = conf.as[Long]("amount")
      val decimals    = conf.as[Option[Int]]("decimals").getOrElse(0)
      val reissuable  = conf.as[Option[Boolean]]("reissuable").getOrElse(false)
      val scriptFile  = conf.as[Option[String]]("script-file").getOrElse("")

      IssueP(name, issuer, description, amount, decimals, reissuable, scriptFile)
    }
  }

  implicit val preconditionsReader: ValueReader[PGenSettings] = (config: Config, path: String) => {
    val faucet = GeneratorSettings.toKeyPair(config.as[String](s"$path.faucet"))

    val accounts =
      config.as[List[CreateAccountP]](s"$path.accounts")(
        traversableReader(
          accountSectionReader,
          implicitly[Factory[CreateAccountP, List[CreateAccountP]]]
        )
      )
    val assets = config.as[List[IssueP]](s"$path.assets")(
      traversableReader(
        assetSectionReader,
        implicitly[Factory[IssueP, List[IssueP]]]
      )
    )
    val leases = config.as[List[LeaseP]](s"$path.leases")(
      traversableReader(
        leasingSectionReader,
        implicitly[Factory[LeaseP, List[LeaseP]]]
      )
    )

    val actions = accounts ++ assets ++ leases
    PGenSettings(faucet, actions)
  }
}
