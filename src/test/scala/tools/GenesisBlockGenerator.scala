package tools

import com.wavesplatform.TransactionGen
import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.state2._
import scorex.account.{Address, AddressScheme, PrivateKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash.DigestSize
import scorex.transaction.GenesisTransaction
import scorex.transaction.TransactionParser.SignatureLength
import scorex.wallet.Wallet

import scala.concurrent.duration._

object GenesisBlockGenerator extends App with TransactionGen {

  val genesisSigner = PrivateKeyAccount(Array.empty)
  val reference = ByteStr(Array.fill(SignatureLength)(-1: Byte))
  val distributions = Map(
    1 -> Seq(1000000000000000L),
    2 -> Seq(800000000000000L, 200000000000000L),
    3 -> Seq(650000000000000L, 200000000000000L, 150000000000000L),
    4 -> Seq(700000000000000L, 200000000000000L, 150000000000000L, 50000000000000L)
  )

  def generateFullAddressInfo() = {
    val seed = bytes32gen.sample.get
    val acc = Wallet.generateNewAccount(seed, 0)
    val privateKey = ByteStr(acc.privateKey)
    val publicKey = ByteStr(acc.publicKey)
    val address = acc.toAddress

    (ByteStr(seed), ByteStr(acc.seed), privateKey, publicKey, address)
  }

  def generate(networkByte: Char, accountsTotal: Int, baseTraget: Long, averageBlockDelay: FiniteDuration, featureCheckBlocksPeriod: Long) = {
    scorex.account.AddressScheme.current = new AddressScheme {
      override val chainId: Byte = networkByte.toByte
    }
    val timestamp = System.currentTimeMillis()
    val initialBalance = 1000000000000000L

    val accounts = Range(0, accountsTotal).map(n => n -> generateFullAddressInfo())
    val genesisTxs = accounts.map { case (n, (_, _, _, _, address)) => GenesisTransaction(address, distributions(accountsTotal)(n), timestamp, ByteStr.empty) }
    val genesisBlock = Block.buildAndSign(1, timestamp, reference, NxtLikeConsensusBlockData(baseTraget, Array.fill(DigestSize)(0: Byte)), genesisTxs, genesisSigner).explicitGet()
    val signature = genesisBlock.signerData.signature

    (accounts, GenesisSettings(timestamp, timestamp, initialBalance, Some(signature),
      genesisTxs.map(tx => GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)), baseTraget, averageBlockDelay, featureCheckBlocksPeriod))
  }

  def print(accs: Seq[(Int, (ByteStr, ByteStr, ByteStr, ByteStr, Address))], settings: GenesisSettings): Unit = {

    println("Addresses:")
    accs.foreach { case (n, (seed, accSeed, priv, pub, addess)) =>
      println(
        s"""($n):
           | seed: $seed
           | accSeed: $accSeed
           | priv: $priv
           | pub : $pub
           | addr: ${addess.address}
           |
       """.stripMargin)
    }

    println(
      s"""GenesisSettings:
         | timestamp: ${settings.timestamp}
         | blockTimestamp: ${settings.blockTimestamp}
         | averageBlockDelay: ${settings.averageBlockDelay}
         | initialBalance: ${settings.initialBalance}
         | initialBaseTarget: ${settings.initialBaseTarget}
         | signature: ${settings.signature}
         | transactions: ${settings.transactions.mkString("\n   ", "\n   ", "")}
     """.stripMargin)
  }

  val (a, s) = generate('D', 3, 153722867, 60.seconds, 10000)
  print(a, s)
}
