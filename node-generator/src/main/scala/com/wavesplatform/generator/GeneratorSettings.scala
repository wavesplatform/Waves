package com.wavesplatform.generator

import cats.Show
import cats.implicits.showInterpolator
import com.google.common.primitives.{Bytes, Ints}
import com.wavesplatform.account.{KeyPair, SeedKeyPair}
import com.wavesplatform.generator.GeneratorSettings.NodeAddress
import com.wavesplatform.generator.config.ConfigReaders
import pureconfig.ConfigReader
import pureconfig.generic.derivation.*

import java.net.{InetSocketAddress, URI, URL}
import java.nio.charset.StandardCharsets
import scala.util.Try

case class GeneratorSettings(
    chainId: String,
    accounts: Seq[String],
    sendTo: Seq[NodeAddress],
    worker: Worker.Settings,
    mode: Mode,
    narrow: NarrowTransactionGenerator.Settings,
    wide: WideTransactionGenerator.Settings,
    dynWide: DynamicWideTransactionGenerator.Settings,
    multisig: MultisigTransactionGenerator.Settings,
    oracle: OracleTransactionGenerator.Settings,
    swarm: SmartGenerator.Settings
) derives ConfigReader {
  val addressScheme: Char                  = chainId.head
  val privateKeyAccounts: Seq[SeedKeyPair] = accounts.map(s => GeneratorSettings.toKeyPair(s))
}

object GeneratorSettings extends ConfigReaders {
  given ConfigReader[InetSocketAddress] = ConfigReader.fromStringTry(str =>
    Try {
      val url = new URI(s"my://$str")
      new InetSocketAddress(url.getHost, url.getPort)
    }
  )

  given ConfigReader[URL] = ConfigReader[String].map(str => new URL(str))

  case class NodeAddress(networkAddress: InetSocketAddress, apiAddress: URL) derives ConfigReader

  implicit val toPrintable: Show[GeneratorSettings] = { x =>
    import x.*

    val modeSettings: String = (mode: @unchecked) match {
      case Mode.NARROW   => show"$narrow"
      case Mode.WIDE     => show"$wide"
      case Mode.DYN_WIDE => show"$dynWide"
      case Mode.MULTISIG => show"$multisig"
      case Mode.ORACLE   => show"$oracle"
      case Mode.SWARM    => show"$swarm"
    }

    s"""network byte: $chainId
       |rich accounts:
       |  ${accounts.mkString("\n  ")}
       |recipient nodes:
       |  ${sendTo.mkString("\n  ")}
       |worker:
       |  ${show"$worker".split('\n').mkString("\n  ")}
       |mode: $mode
       |$mode settings:
       |  ${modeSettings.split('\n').mkString("\n  ")}""".stripMargin
  }

  def toKeyPair(seedText: String): SeedKeyPair = {
    KeyPair(com.wavesplatform.crypto.secureHash(Bytes.concat(Ints.toByteArray(0), seedText.getBytes(StandardCharsets.UTF_8))))
  }
}
