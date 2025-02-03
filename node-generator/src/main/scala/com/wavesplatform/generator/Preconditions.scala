package com.wavesplatform.generator

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.common.utils.EitherExt2.explicitGet
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
import pureconfig.ConfigReader

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Try

object Preconditions {
  private val Fee = 1500000L

  sealed trait PAction

  final case class LeaseP(from: KeyPair, to: Address, amount: Long, repeat: Option[Int]) extends PAction derives ConfigReader
  final case class IssueP(name: String, issuer: KeyPair, desc: String, amount: Long, decimals: Int, reissuable: Boolean, scriptFile: String)
    extends PAction derives ConfigReader

  final case class CreateAccountP(seed: String, balance: Long, scriptFile: Option[String]) extends PAction derives ConfigReader

  given ConfigReader[KeyPair] =
    ConfigReader[String].map(s => KeyPair(com.wavesplatform.crypto.secureHash(Bytes.concat(Ints.toByteArray(0), s.getBytes(StandardCharsets.UTF_8)))))

  given ConfigReader[Address] = ConfigReader.fromStringTry(str => Try(Address.fromString(str).explicitGet()))

  final case class PGenSettings(faucet: KeyPair, accounts: List[CreateAccountP], leases: List[LeaseP], assets: List[IssueP])derives ConfigReader {
    val actions: List[PAction] = accounts ++ assets ++ leases
  }

  final case class CreatedAccount(keyPair: KeyPair, balance: Long, script: Option[Script])

  final case class UniverseHolder(
      accounts: List[CreatedAccount] = Nil,
      issuedAssets: List[IssueTransaction] = Nil,
      leases: List[LeaseTransaction] = Nil
  )

  def mk(settings: PGenSettings, time: Time, estimator: ScriptEstimator): (UniverseHolder, List[Transaction], List[Transaction]) = {
    val (holder, headTransactions) = settings.actions
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
}
