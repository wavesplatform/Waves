package com.wavesplatform.state.snapshot

import com.google.common.primitives.Ints
import com.google.protobuf.ByteString.copyFrom as bs
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.crypto.fastHash
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.protobuf.snapshot.{TransactionStatus, TransactionStateSnapshot as TSS}
import com.wavesplatform.protobuf.transaction.DataEntry
import com.wavesplatform.protobuf.{Amount, PBSnapshots}
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import org.bouncycastle.util.encoders.Hex

class TxStateSnapshotHashSpec extends PropSpec {
  private def hashInt(i: Int) = bs(fastHash(Ints.toByteArray(i)))

  val stateHash         = new StateHashBuilder
  private val signer101 = TxHelpers.signer(101)
  private val signer102 = TxHelpers.signer(102)

  private val address1 = signer101.toAddress
  private val address2 = signer102.toAddress

  private val assetId1 = hashInt(0xaa22aa44)
  private val assetId2 = hashInt(0xbb22aa44)

  private val leaseId  = hashInt(0x11aaef22)
  private val orderId1 = hashInt(0xee23ef22)
  private val orderId2 = hashInt(0xbb77ef29)

  private val testScript = bs(TestCompiler(V6).compileExpression("true").bytes().arr)

  private val wavesBalances = TSS(balances =
    Seq(
      TSS.Balance(bs(address1.bytes), Some(Amount(amount = 10.waves))),
      TSS.Balance(bs(address2.bytes), Some(Amount(amount = 20.waves)))
    )
  )

  private val assetBalances = TSS(balances =
    Seq(
      TSS.Balance(bs(address1.bytes), Some(Amount(assetId1, 10_000))),
      TSS.Balance(bs(address2.bytes), Some(Amount(assetId2, 20_000)))
    )
  )

  private val dataEntries = TSS(accountData =
    Seq(
      TSS.AccountData(
        bs(address1.bytes),
        Seq(
          DataEntry("foo", DataEntry.Value.Empty),
          DataEntry("baz", DataEntry.Value.StringValue("StringValue")),
          DataEntry("baz", DataEntry.Value.BinaryValue(bs(address1.bytes)))
        )
      ),
      TSS.AccountData(
        bs(address2.bytes),
        Seq(
          DataEntry("foo", DataEntry.Value.IntValue(1200)),
          DataEntry("bar", DataEntry.Value.BoolValue(true))
        )
      )
    )
  )

  private val accountScript = TSS(accountScripts =
    Some(
      TSS.AccountScript(
        bs(signer101.publicKey.arr),
        testScript,
        250
      )
    )
  )

  private val assetScript = TSS(assetScripts = Some(TSS.AssetScript(assetId2, testScript)))

  private val newLease = TSS(
    leaseBalances = Seq(
      TSS.LeaseBalance(bs(address1.bytes), out = 45.waves),
      TSS.LeaseBalance(bs(address2.bytes), in = 55.waves)
    ),
    newLeases = Seq(
      TSS.NewLease(leaseId,  bs(signer101.publicKey.arr), bs(address2.bytes), 25.waves)
    )
  )

  private val cancelledLease = TSS(
    leaseBalances = Seq(TSS.LeaseBalance(bs(address1.bytes), out = 20.waves), TSS.LeaseBalance(bs(address2.bytes), in = 0.waves)),
    cancelledLeases = Seq(
      TSS.CancelledLease(leaseId)
    )
  )

  private val sponsorship = TSS(
    sponsorships = Seq(TSS.Sponsorship(assetId2, 5500))
  )

  private val alias = TSS(
    aliases = Some(TSS.Alias(bs(address2.bytes), "wavesevo"))
  )

  private val volumeAndFee = TSS(
    orderFills = Seq(
      TSS.OrderFill(orderId1, 10.waves, 2000),
      TSS.OrderFill(orderId2, 10.waves, 2000)
    )
  )

  private val newAsset = TSS(
    assetStatics = Seq(
      TSS.NewAsset(assetId1, hashInt(0x88aadd55), nft = true),
      TSS.NewAsset(assetId2, hashInt(0x88aadd55), decimals = 8)
    ),
    assetVolumes = Seq(
      TSS.AssetVolume(assetId2, true, bs((BigInt(Long.MaxValue) * 10).toByteArray)),
      TSS.AssetVolume(assetId1, false, bs(BigInt(1).toByteArray))
    ),
    assetNamesAndDescriptions = Seq()
  )

  private val reissuedAsset = TSS(
    assetVolumes = Seq(
      TSS.AssetVolume(assetId2, false, bs((BigInt(10_00000000L)).toByteArray)),
    )
  )
  private val renamedAsset = TSS(
    assetNamesAndDescriptions = Seq(
      TSS.AssetNameAndDescription()
    )
  )
  private val failedTransaction = TSS(
    balances = Seq(),
    transactionStatus = TransactionStatus.FAILED
  )
  private val elidedTransaction = TSS(
    transactionStatus = TransactionStatus.ELIDED
  )

  private val all = TSS(
    assetBalances.balances ++ wavesBalances.balances,
    newLease.leaseBalances ++ cancelledLease.leaseBalances,
    newLease.newLeases,
    cancelledLease.cancelledLeases,
    newAsset.assetStatics,
    newAsset.assetVolumes ++ reissuedAsset.assetVolumes,
    newAsset.assetNamesAndDescriptions ++ renamedAsset.assetNamesAndDescriptions,
    newAsset.assetScripts,
    alias.aliases,
    volumeAndFee.orderFills,
    accountScript.accountScripts,
    dataEntries.accountData,
    sponsorship.sponsorships,
    failedTransaction.transactionStatus
  )

  private val testData = Table(
    ("state snapshot", "base64 bytes", "tx id", "previous state hash", "expected result"),
    (
      wavesBalances,
      "CiQKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgYQgJTr3AMKJAoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSBhCAqNa5Bw==",
      ByteStr.empty,
      "",
      "954bf440a83542e528fe1e650471033e42d97c5896cc571aec39fccc912d7db0"
    ),
    (
      assetBalances,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQ==",
      ByteStr.empty,
      "954bf440a83542e528fe1e650471033e42d97c5896cc571aec39fccc912d7db0",
      "534e27c3a787536e18faf844ff217a8f14e5323dfcd3cc5b9ab3a8e261f60cf7"
    ),
    (
      dataEntries,
      "WloKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgUKA2ZvbxISCgNiYXpqC1N0cmluZ1ZhbHVlEiEKA2JhemIaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7VaLwoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCAoDZm9vULAJEgcKA2JhclgB",
      ByteStr.empty,
      "534e27c3a787536e18faf844ff217a8f14e5323dfcd3cc5b9ab3a8e261f60cf7",
      "e24952f7e5ee51450ffedbddb03cbbf116964f207c9621d9c67353a75030ba01"
    ),
    (
      accountScript,
      "Ui4KIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoEgcGAQaw0U/PGPoB",
      ByteStr.empty,
      "e24952f7e5ee51450ffedbddb03cbbf116964f207c9621d9c67353a75030ba01",
      "976df303cdd90d3e89b1a9fd11b0b93cfec47107ee51a9c00746b8dc4d4ccf4d"
    ),
    (
      assetScript,
      "MisKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEgcGAQaw0U/P",
      ByteStr.empty,
      "976df303cdd90d3e89b1a9fd11b0b93cfec47107ee51a9c00746b8dc4d4ccf4d",
      "08c68eb3bf3494a0725de020f353b530a4ac5a9db059e23b54f0b98b2a2a4d24"
    ),
    (
      newLease,
      "EiIKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1GICa4uEQEiIKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEICuzb4USmkKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqNqgFECIDyi6gJEiBQx1mvQnelVPDNtCyxR82wU73hEX9bBDsNfUlEuNBgaBoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCo=",
      ByteStr.empty,
      "08c68eb3bf3494a0725de020f353b530a4ac5a9db059e23b54f0b98b2a2a4d24",
      "77bcff83d90592e950527d35e70886eceaab04648e8b0e30dd74c6ec2f722e53"
    ),
    (
      cancelledLease,
      "EiIKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1GICo1rkHEhwKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqSiUKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqNsgEA",
      ByteStr.empty,
      "77bcff83d90592e950527d35e70886eceaab04648e8b0e30dd74c6ec2f722e53",
      "3c115c2cf9e73ea9b9b97c2c3ac672227814c313a28ee1f3c226501f50ede2db"
    ),
    (
      sponsorship,
      "YiUKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEPwq",
      ByteStr.empty,
      "3c115c2cf9e73ea9b9b97c2c3ac672227814c313a28ee1f3c226501f50ede2db",
      "192e86fa9ebb24b5780a053a9b9864518705795647be47523dcb6cba9a67345d"
    ),
    (
      alias,
      "OiYKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgh3YXZlc2V2bw==",
      ByteStr.empty,
      "192e86fa9ebb24b5780a053a9b9864518705795647be47523dcb6cba9a67345d",
      "8a64e138c63c6f23b657316b9cecbbbb163a30c77caf1111ddecf70428b585b9"
    ),
    (
      volumeAndFee,
      "QisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPQisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAP",
      ByteStr.empty,
      "8a64e138c63c6f23b657316b9cecbbbb163a30c77caf1111ddecf70428b585b9",
      "2cd2b9115eb62e30904c0bee93d0051c11ab7df99f6ce403bb30dbc8314d8f68"
    ),
    (
      newAsset,
      "GkYKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBniABGkYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBnhgK",
      ByteStr.empty,
      "2cd2b9115eb62e30904c0bee93d0051c11ab7df99f6ce403bb30dbc8314d8f68",
      "3724dfd7fc0a9d756b64a956829a2d2377bb90c852b388a9f85e00fe999c050d"
    ),
    (
      reissuedAsset,
      "",
      ByteStr.empty,
      "3724dfd7fc0a9d756b64a956829a2d2377bb90c852b388a9f85e00fe999c050d",
      "dd2971ec2c572c5ffea5a6870eea0a5baec8ded08a6ab1e3bb132271646c5924"
    ),
    (
      renamedAsset,
      "",
      ByteStr.empty,
      "dd2971ec2c572c5ffea5a6870eea0a5baec8ded08a6ab1e3bb132271646c5924",
      "048fb3f170306f8f34c2e0c2841604333529540160d235c11852f5169d1b9a08"
    ),
    (
      failedTransaction,
      "",
      ByteStr.empty,
      "048fb3f170306f8f34c2e0c2841604333529540160d235c11852f5169d1b9a08",
      "684262eae7aa1ec68e6adbf0abe528b1d77cb9389b334193236fb2927c4287f1"
    ),
    (
      elidedTransaction,
      "",
      ByteStr.empty,
      "684262eae7aa1ec68e6adbf0abe528b1d77cb9389b334193236fb2927c4287f1",
      "6fbd5cca3707185cd6004e31521c6f94fab3c75474bda7926862aa3385205200"
    ),
    (
      all,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQokChoBVGD9UO8g3kVxIH37nIi+fBwviiLHCtiPtRIGEICU69wDCiQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgYQgKjWuQcSIgoaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7UYgJri4RASIgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoQgK7NvhQSIgoaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7UYgKjWuQcSHAoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoaRgogXmafggpn0Ihth0eM8EOirHhcx69V3DHOEHU5S9NQolsSINxgYWpj0yi0dOkOmySjJw38y3rZtl8/EgLh+lTRlMGeIAEaRgogeJ3AESPVNg9wgq/UtGq4v+i1Fgu/tSbAQ+X8eDpPiU4SINxgYWpj0yi0dOkOmySjJw38y3rZtl8/EgLh+lTRlMGeGAo6JgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCHdhdmVzZXZvQisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPQisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAPSmkKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqNqgFECIDyi6gJEiBQx1mvQnelVPDNtCyxR82wU73hEX9bBDsNfUlEuNBgaBoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCpKJQoguIIzLIWCBbxl3Ysa38C0yvtZan6R9ZvOU33eldmrOo2yAQBSLgogUMdZr0J3pVTwzbQssUfNsFO94RF/WwQ7DX1JRLjQYGgSBwYBBrDRT88Y+gFaWgoaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7USBQoDZm9vEhIKA2JhemoLU3RyaW5nVmFsdWUSIQoDYmF6YhoBVGD9UO8g3kVxIH37nIi+fBwviiLHCtiPtVovChoBVELFyWNz9Q/YE1BgTxwU8qbJJWra/RmwKhIICgNmb29QsAkSBwoDYmFyWAFiJQogeJ3AESPVNg9wgq/UtGq4v+i1Fgu/tSbAQ+X8eDpPiU4Q/Co=",
      ByteStr.empty,
      "6fbd5cca3707185cd6004e31521c6f94fab3c75474bda7926862aa3385205200",
      "f6b2f644c24466b27b05aa187b4a339e732d37b488635403c03f437686f07e46"
    )
  )

  property("correctly create transaction state snapshot hash from snapshot") {
    forAll(testData) { case (pbSnapshot, b64str, txId, prev, expectedResult) =>
      val parsedSnapshot = TSS.parseFrom(Base64.decode(b64str))
      parsedSnapshot shouldEqual pbSnapshot
      val (snapshot, meta) = PBSnapshots.fromProtobuf(pbSnapshot, txId, 10)
      val raw = Hex.toHexString(
        TxStateSnapshotHashBuilder
          .createHashFromSnapshot(snapshot, Some(TxStateSnapshotHashBuilder.TxStatusInfo(txId, meta)))
          .createHash(ByteStr.decodeBase64(prev).get)
          .arr
      )

      println(s"\n\t${Base64.encode(pbSnapshot.toByteArray)}\n\t$raw")

      raw shouldEqual expectedResult
    }
  }
}
