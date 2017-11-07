package tools

import com.wavesplatform.state2._
import scorex.account.{AddressScheme, PublicKeyAccount}
import scorex.block.{Block, SignerData}
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock

object JsBlockParser extends App {

//  def parseTx(str: String): TransferTransaction = {
//    val js = Json.parse(str)
//    val sig = ByteStr.decodeBase58((js \ "signature").get.as[String]).get
//    val senderPk = ByteStr.decodeBase58((js \ "senderPublicKey").get.as[String]).get
//    val recipient = (js \ "recipient").get.as[String]
//    val fee = (js \ "fee").get.as[Long]
//    val timestamp = (js \ "timestamp").get.as[Long]
//    val amount = (js \ "amount").get.as[Long]
//    TransferTransaction(None, PublicKeyAccount(senderPk.arr), Address.fromString(recipient).explicitGet(), amount, timestamp, None, fee, Array.emptyByteArray, sig)
//  }

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 84//'I'
  }

//  private val txs: List[TransferTransaction] = Source.fromFile("C:\\Users\\ilyas\\Desktop\\is2.json").getLines
//    .toList
//    .map(_.trim)
//    .map(parseTx)

  /*

  2017-08-04 12:57:41 TRACE [miner-pool-21] c.w.network.package$ - Broadcasting BlockForged(Block(1501851461300,3,
  4GkW491kuPtg3LDkCrXwBRASdQ7qUV2BNEFpM9XzD56JufSpmLqV9672wRQcSqJdxyEgq8nSQs2ogHit8Sr1ZtGT,
  SignerData(3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k,2S5A8DnwrqpxFv3cjYKJB8D86h4PS1tAy2Qaii1ntZhpiuj3bmTC8McES5XoBFj8z2yQn9yAxCPTxjsexjfjddXo),
  NxtLikeConsensusBlockData(197518,96C1rM2CxoKEua8Ha1xdVEYmnsL3VB44Z2uVKmbXPV3D),List(..)))

  account-seed = Co9cYj4YC15QWHZ3Ux8acv3Q2mVe3PzHucxuNWSRhpzj   // recreated
  private-key = G6skvWMu6QLHVmM7Lo85Wq1GHdTPHzhyrPbsjkSWAYC6
  public-key = FkV3y43B4SAXkSL31SXFZU5xm5bonRtRVNU1sQzwpVhm   // actual public key, not address
  address = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k

   */
//
//  val block = Block(timestamp = 1501851461300L, version = 3.toByte,
//    reference = ByteStr.decodeBase58("4GkW491kuPtg3LDkCrXwBRASdQ7qUV2BNEFpM9XzD56JufSpmLqV9672wRQcSqJdxyEgq8nSQs2ogHit8Sr1ZtGT").get,
//    signerData = SignerData(PublicKeyAccount(ByteStr.decodeBase58("FkV3y43B4SAXkSL31SXFZU5xm5bonRtRVNU1sQzwpVhm").get.arr),
//      ByteStr.decodeBase58("2S5A8DnwrqpxFv3cjYKJB8D86h4PS1tAy2Qaii1ntZhpiuj3bmTC8McES5XoBFj8z2yQn9yAxCPTxjsexjfjddXo").get),
//    consensusData = NxtLikeConsensusBlockData(197518, ByteStr.decodeBase58("96C1rM2CxoKEua8Ha1xdVEYmnsL3VB44Z2uVKmbXPV3D").get),
//    transactionData = txs,
//    Set.empty)


  val goodBlock = Block.buildAndSign(
    timestamp = 0L,
    version = 3.toByte,
    reference = ByteStr.decodeBase58("2RxD9WRHsLcLMSf58VPj38TaA7XyYrREaz3vsKWVFS7JE93wGzd57UTrHdzb1v2bdgS3fhZK96HAjsr2gb5gq43w").get,
    signer = TestBlock.defaultSigner,
    consensusData = NxtLikeConsensusBlockData(1,ByteStr.decodeBase58("D866WPMvdahU2BdLowfYY9m4GrqXfXSmFxFd4e6rKYTd").get),
    transactionData = Seq.empty,
    featureVotes = Set(2)).explicitGet()

  println(goodBlock.signaturesValid())

  val badBlock = Block(
    timestamp=0,
    version=3.toByte,
    reference=ByteStr.decodeBase58("2RxD9WRHsLcLMSf58VPj38TaA7XyYrREaz3vsKWVFS7JE93wGzd57UTrHdzb1v2bdgS3fhZK96HAjsr2gb5gq43w").get,
    signerData=SignerData(PublicKeyAccount(ByteStr.decodeBase58("EENPV1mRhUD9gSKbcWt84cqnfSGQP5LkCu5gMBfAanYH").get.arr),
      ByteStr.decodeBase58("3DxYZjbzgkgad17JC25CUiYNCJPhBSgF6zunKy3RUDZzNoqWhyKoNihRjqkxPUx8ocTp3Qw6Nux7WH1w1CHwSwq4").get),
      consensusData=NxtLikeConsensusBlockData(1,ByteStr.decodeBase58("D866WPMvdahU2BdLowfYY9m4GrqXfXSmFxFd4e6rKYTd").get),
        transactionData = Seq.empty, featureVotes=Set(2))

  println(badBlock.signaturesValid())

  println(Block.parseBytes(goodBlock.bytes()).get.signaturesValid())

//  val recreated = Block.buildAndSign(3, 1501851461300L, ByteStr.decodeBase58("4GkW491kuPtg3LDkCrXwBRASdQ7qUV2BNEFpM9XzD56JufSpmLqV9672wRQcSqJdxyEgq8nSQs2ogHit8Sr1ZtGT").get,
//    NxtLikeConsensusBlockData(197518, ByteStr.decodeBase58("96C1rM2CxoKEua8Ha1xdVEYmnsL3VB44Z2uVKmbXPV3D").get),txs.take(128),
//    PrivateKeyAccount(ByteStr.decodeBase58("Co9cYj4YC15QWHZ3Ux8acv3Q2mVe3PzHucxuNWSRhpzj").get.arr)
//  ).explicitGet()
//
//  println(Block.parseBytes(recreated.bytes()).get.signatureValid)

}
