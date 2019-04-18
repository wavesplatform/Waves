package com.wavesplatform.generator

import java.nio.file.{Files, Paths}

import com.typesafe.config.Config
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV2}
import com.wavesplatform.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.utils.Time
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.collection.generic.CanBuildFrom

object Preconditions {
  private[this] val Fee = 1000000

  sealed abstract class PAction(val priority: Int)
  final case class DeployScriptP(account: KeyPair, file: String)                                                          extends PAction(priority = 4)
  final case class LeaseP(from: KeyPair, to: Address, amount: Long)                                                       extends PAction(3)
  final case class IssueP(name: String, issuer: KeyPair, desc: String, amount: Long, decimals: Int, reissueable: Boolean, scriptFile: String) extends PAction(2)
  final case class CreateAccountP(seed: String, balance: Long)                                                            extends PAction(1)

  final case class PGenSettings(faucet: KeyPair, actions: List[PAction])

  final case class UniverseHolder(accountsWithBalances: List[(KeyPair, Long)] = Nil,
                                  issuedAssets: List[IssueTransaction] = Nil,
                                  leases: List[LeaseTransaction] = Nil,
                                  scripts: List[(Address, Script)] = Nil)

  def mk(settings: PGenSettings, time: Time): (UniverseHolder, List[Transaction]) = {
    settings.actions
      .sortBy(_.priority)(Ordering[Int].reverse)
      .foldLeft((UniverseHolder(), List.empty[Transaction])) {
        case ((uni, txs), action) =>
          action match {
            case DeployScriptP(account, file) =>
              val scriptText         = new String(Files.readAllBytes(Paths.get(file)))
              val Right((script, _)) = ScriptCompiler.compile(scriptText)
              val Right(tx)          = SetScriptTransaction.selfSigned(account, Some(script), Fee, time.correctedTime())
              (uni.copy(scripts = uni.scripts :+ (account.toAddress -> script)), tx :: txs)

            case LeaseP(from, to, amount) =>
              val tx = LeaseTransactionV1
                .selfSigned(from, amount, Fee, time.correctedTime(), to)
                .explicitGet()
              (uni.copy(leases = tx :: uni.leases), tx :: txs)

            case IssueP(assetName, issuer, assetDescription, amount, decimals, reissueable, scriptFile) =>
              val script = Option(scriptFile)
                .filter(_.nonEmpty)
                .map(file => ScriptCompiler.compile(new String(Files.readAllBytes(Paths.get(file)))))
                .flatMap(_.toOption)
                .map(_._1)

              val tx = IssueTransactionV2
                .selfSigned(AddressScheme.current.chainId, issuer, assetName.getBytes(), assetDescription.getBytes, amount, decimals.toByte, reissueable, script, Fee, time.correctedTime())
                .explicitGet()
              (uni.copy(issuedAssets = tx :: uni.issuedAssets), tx :: txs)

            case CreateAccountP(seed, balance) =>
              val acc = KeyPair
                .fromSeed(seed)
                .explicitGet()
              val tx = TransferTransactionV1
                .selfSigned(Waves, settings.faucet, acc, balance, time.correctedTime(), Waves, Fee, "Generator".getBytes())
                .explicitGet()
              (uni.copy(accountsWithBalances = (acc, balance) :: uni.accountsWithBalances), tx :: txs)
          }
      }
  }

  private val accountSectionReader = new ValueReader[CreateAccountP] {
    override def read(config: Config, path: String): CreateAccountP = {
      val conf = config.getConfig(path)

      val seed    = conf.as[String]("seed")
      val balance = conf.as[Long]("balance")

      CreateAccountP(seed, balance)
    }
  }

  private val leasingSectionReader = new ValueReader[LeaseP] {
    override def read(config: Config, path: String): LeaseP = {
      val conf = config.getConfig(path)

      val from   = conf.as[String]("from")
      val to     = conf.as[String]("to")
      val amount = conf.as[Long]("amount")

      LeaseP(
        KeyPair.fromSeed(from).explicitGet(),
        KeyPair.fromSeed(to).explicitGet(),
        amount
      )
    }
  }

  private val assetSectionReader = new ValueReader[IssueP] {
    override def read(config: Config, path: String): IssueP = {
      val conf = config.getConfig(path)
      val Right(issuer) = KeyPair.fromSeed(conf.getString("issuer"))

      val name        = conf.as[Option[String]]("name").getOrElse("")
      val description = conf.as[Option[String]]("description").getOrElse("")
      val amount      = conf.as[Long]("amount")
      val decimals    = conf.as[Option[Int]]("decimals").getOrElse(0)
      val reissuable  = conf.as[Option[Boolean]]("reissuable").getOrElse(false)

      IssueP(name, issuer, description, amount, decimals, reissuable)
    }
  }

  private val deployScriptSectionReader = new ValueReader[DeployScriptP] {
    override def read(config: Config, path: String): DeployScriptP = {
      val conf = config.getConfig(path)

      val from = KeyPair.fromSeed(conf.as[String]("account")).explicitGet()
      val file = conf.as[String]("file")

      DeployScriptP(from, file)
    }
  }

  implicit val preconditionsReader = new ValueReader[PGenSettings] {
    override def read(config: Config, path: String): PGenSettings = {
      val faucet = KeyPair
        .fromSeed(config.as[String](s"$path.faucet"))
        .explicitGet()

      val accounts =
        config.as[List[CreateAccountP]](s"$path.accounts")(
          traversableReader(
            accountSectionReader,
            implicitly[CanBuildFrom[Nothing, CreateAccountP, List[CreateAccountP]]]
          )
        )
      val assets = config.as[List[IssueP]](s"$path.assets")(
        traversableReader(
          assetSectionReader,
          implicitly[CanBuildFrom[Nothing, IssueP, List[IssueP]]]
        )
      )
      val leases = config.as[List[LeaseP]](s"$path.leases")(
        traversableReader(
          leasingSectionReader,
          implicitly[CanBuildFrom[Nothing, LeaseP, List[LeaseP]]]
        )
      )

      val scripts = config.as[List[DeployScriptP]](s"$path.scripts")(
        traversableReader(
          deployScriptSectionReader,
          implicitly[CanBuildFrom[Nothing, DeployScriptP, List[DeployScriptP]]]
        )
      )

      val actions = accounts ++ assets ++ leases ++ scripts
      PGenSettings(faucet, actions)
    }
  }
}
