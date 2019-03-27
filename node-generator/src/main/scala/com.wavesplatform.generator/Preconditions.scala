package com.wavesplatform.generator

import com.typesafe.config.Config
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.assets.IssueTransactionV1
import com.wavesplatform.transaction.lease.LeaseTransactionV1
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import com.wavesplatform.utils.Time
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ValueReader

import scala.collection.generic.CanBuildFrom

object Preconditions {

  private val FEE = 1000000

  sealed abstract class PAction(val priority: Int)
  final case class LeaseP(from: PrivateKeyAccount, to: Address, amount: Long)                                                       extends PAction(3)
  final case class IssueP(name: String, issuer: PrivateKeyAccount, desc: String, amount: Long, decimals: Int, reissueable: Boolean) extends PAction(2)
  final case class CreateAccountP(seed: String, balance: Long)                                                                      extends PAction(1)

  final case class PGenSettings(faucet: PrivateKeyAccount, actions: List[PAction])

  final case class UniverseHolder(accountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil,
                                  issuedAssets: List[ByteStr] = Nil,
                                  leases: List[ByteStr] = Nil)

  def mk(settings: PGenSettings, time: Time): (UniverseHolder, List[Transaction]) = {
    settings.actions
      .sortBy(-_.priority)
      .foldLeft((UniverseHolder(), List.empty[Transaction])) {
        case ((uni, txs), action) =>
          action match {
            case LeaseP(from, to, amount) =>
              val tx = LeaseTransactionV1
                .selfSigned(from, amount, FEE, time.correctedTime(), to)
                .explicitGet()
              (uni.copy(leases = tx.id() :: uni.leases), tx :: txs)
            case IssueP(assetName, issuer, assetDescription, amount, decimals, reissueable) =>
              val tx = IssueTransactionV1
                .selfSigned(issuer, assetName.getBytes(), assetDescription.getBytes, amount, decimals.toByte, reissueable, FEE, time.correctedTime())
                .explicitGet()
              (uni.copy(issuedAssets = tx.id() :: uni.issuedAssets), tx :: txs)
            case CreateAccountP(seed, balance) =>
              val acc = PrivateKeyAccount
                .fromSeed(seed)
                .explicitGet()
              val tx = TransferTransactionV1
                .selfSigned(Waves, settings.faucet, acc, balance, time.correctedTime(), Waves, FEE, "Generator".getBytes())
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
        PrivateKeyAccount.fromSeed(from).explicitGet(),
        PrivateKeyAccount.fromSeed(to).explicitGet(),
        amount
      )
    }
  }

  private val assetSectionReader = new ValueReader[IssueP] {
    override def read(config: Config, path: String): IssueP = {
      val conf = config.getConfig(path)

      val issuer = PrivateKeyAccount
        .fromSeed(conf.getString("issuer"))
        .explicitGet()
      val name        = conf.as[Option[String]]("name").getOrElse("")
      val description = conf.as[Option[String]]("description").getOrElse("")
      val amount      = conf.as[Long]("amount")
      val decimals    = conf.as[Option[Int]]("decimals").getOrElse(0)
      val reissuable  = conf.as[Option[Boolean]]("reissuable").getOrElse(false)

      IssueP(name, issuer, description, amount, decimals, reissuable)
    }
  }

  implicit val preconditionsReader = new ValueReader[PGenSettings] {
    override def read(config: Config, path: String): PGenSettings = {
      val faucet = PrivateKeyAccount
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

      val actions = accounts ++ assets ++ leases

      PGenSettings(faucet, actions)
    }
  }
}
