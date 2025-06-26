package com.wavesplatform.generator

import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{Address, KeyPair, SeedKeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Transaction, TxVersion}
import com.wavesplatform.utils.Time
import pureconfig.ConfigReader

import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.util.{Random, Try}

object Preconditions {
  private val Fee = 1500000L

  given ConfigReader[KeyPair] =
    ConfigReader[String].map(s => KeyPair(com.wavesplatform.crypto.secureHash(Bytes.concat(Ints.toByteArray(0), s.getBytes(StandardCharsets.UTF_8)))))

  given ConfigReader[Address] = ConfigReader.fromStringTry(str => Try(Address.fromString(str).explicitGet()))

  final case class PGenSettings(faucet: KeyPair, balance: Long, leasesCount: Int, assetsCount: Int) derives ConfigReader

  final case class UniverseHolder(
      issuedAssets: List[IssueTransaction] = Nil,
      leases: List[LeaseTransaction] = Nil
  )

  def mk(
      settings: PGenSettings,
      accounts: Seq[SeedKeyPair],
      time: Time
  ): (UniverseHolder, List[Transaction], List[Transaction]) = {
    val transfers = accounts.map { account =>
      // val acc = GeneratorSettings.toKeyPair(accountSeed)
      TransferTransaction
        .selfSigned(2.toByte, settings.faucet, account.toAddress, Waves, settings.balance, Waves, Fee, ByteStr.empty, time.correctedTime())
        .explicitGet()
    }.toList

    val issuedAssets = (1 to settings.assetsCount)
      .map(_ =>
        IssueTransaction
          .selfSigned(
            TxVersion.V3,
            accounts(Random.nextInt(accounts.size)),
            UUID.randomUUID().toString.take(8),
            Random.nextString(100),
            10_000_000_000L,
            Random.nextLong(9).toByte,
            true,
            None,
            100000000,
            time.correctedTime()
          )
          .explicitGet()
      )
      .toList

    val leaseTxs = (1 to settings.leasesCount).map { _ =>
      val rndAccount = Random.nextInt(accounts.size - 1)

      LeaseTransaction
        .selfSigned(
          TxVersion.V3,
          accounts(rndAccount),
          GeneratorSettings.toKeyPair(Random.nextString(10)).toAddress,
          1 + Random.nextInt(1000),
          Fee,
          time.correctedTime()
        )
        .explicitGet()
    }.toList

    val transferAssets = issuedAssets.flatMap(issuedAsset =>
      val issuer  = accounts.find(_.publicKey == issuedAsset.sender).get
      val balance = issuedAsset.quantity.value / accounts.size
      accounts.map { acc =>
        TransferTransaction
          .selfSigned(
            TxVersion.V3,
            issuer,
            acc.toAddress,
            IssuedAsset(issuedAsset.assetId),
            balance,
            Waves,
            Fee,
            ByteStr.empty,
            time.correctedTime()
          )
          .explicitGet()
      }
    )
    val holder = UniverseHolder(issuedAssets, leaseTxs)
    (holder, transfers ++ issuedAssets, transferAssets ++ leaseTxs)
  }

}
