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
  private val signer103 = TxHelpers.signer(103)

  private val address1 = signer101.toAddress
  private val address2 = signer102.toAddress
  private val address3 = signer103.toAddress

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
          DataEntry("bar", DataEntry.Value.StringValue("StringValue")),
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
      TSS.NewLease(leaseId, bs(signer101.publicKey.arr), bs(address2.bytes), 25.waves)
    )
  )

  private val cancelledLease = TSS(
    leaseBalances = Seq(TSS.LeaseBalance(bs(address3.bytes), out = 20.waves), TSS.LeaseBalance(bs(TxHelpers.address(104).bytes), in = 0.waves)),
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
      TSS.AssetVolume(hashInt(0x23aadd55), false, bs((BigInt(10000000_00L)).toByteArray))
    )
  )
  private val renamedAsset = TSS(
    assetNamesAndDescriptions = Seq(
      TSS.AssetNameAndDescription(
        assetId2,
        "newname",
        "some fancy description"
      )
    )
  )
  private val failedTransaction = TSS(
    balances = Seq(
      TSS.Balance(bs(address2.bytes), Some(Amount(amount = 25.995.waves)))
    ),
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
    ("clue", "state snapshot", "base64 bytes", "tx id", "previous state hash", "expected result"),
    (
      "waves balances",
      wavesBalances,
      "CiQKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgYQgJTr3AMKJAoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSBhCAqNa5Bw==",
      ByteStr.empty,
      Hex.toHexString(TxStateSnapshotHashBuilder.InitStateHash.arr),
      "f0a8b6745534c2d20412f40cdb097b7050898e44531a661ef64fc5be0744ac72"
    ),
    (
      "asset balances",
      assetBalances,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQ==",
      ByteStr.empty,
      "f0a8b6745534c2d20412f40cdb097b7050898e44531a661ef64fc5be0744ac72",
      "16c4803d12ee8e9d6c705ca6334fd84f57c0e78c4ed8a9a3dc6c28dcd9b29a34"
    ),
    (
      "data entries",
      dataEntries,
      "YloKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgUKA2ZvbxISCgNiYXJqC1N0cmluZ1ZhbHVlEiEKA2JhemIaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7ViLwoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCAoDZm9vULAJEgcKA2JhclgB",
      ByteStr.empty,
      "16c4803d12ee8e9d6c705ca6334fd84f57c0e78c4ed8a9a3dc6c28dcd9b29a34",
      "d33269372999bfd8f7afdf97e23bc343bcf3812f437e8971681a37d56868ec8a"
    ),
    (
      "account script",
      accountScript,
      "Wi4KIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoEgcGAQaw0U/PGPoB",
      ByteStr.empty,
      "d33269372999bfd8f7afdf97e23bc343bcf3812f437e8971681a37d56868ec8a",
      "dcdf7df91b11fdbeb2d99c4fd64abb4657adfda15eed63b1d4730aa2b6275ee2"
    ),
    (
      "asset script",
      assetScript,
      "QisKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEgcGAQaw0U/P",
      ByteStr.empty,
      "dcdf7df91b11fdbeb2d99c4fd64abb4657adfda15eed63b1d4730aa2b6275ee2",
      "d3c7f2aeb1d978ecebc2fe1f0555e4378cef5171db460d8bbfebef0e59c3a44c"
    ),
    (
      "new lease",
      newLease,
      "EiIKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1GICa4uEQEiIKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEICuzb4UGmYKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqNEiBQx1mvQnelVPDNtCyxR82wU73hEX9bBDsNfUlEuNBgaBoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoggPKLqAk=",
      ByteStr.empty,
      "d3c7f2aeb1d978ecebc2fe1f0555e4378cef5171db460d8bbfebef0e59c3a44c",
      "2665ce187b867f2dae95699882d9fd7c31039c505b8af93ed22cada90524ff37"
    ),
    (
      "cancelled lease",
      cancelledLease,
      "EiIKGgFUMCPLqLW81X2Atgaj2KwF9QkaJq47Cev9GICo1rkHEhwKGgFUYSJd8vzI9rq7GdIuDy65JMc8zi497E98IiIKILiCMyyFggW8Zd2LGt/AtMr7WWp+kfWbzlN93pXZqzqN",
      ByteStr.empty,
      "2665ce187b867f2dae95699882d9fd7c31039c505b8af93ed22cada90524ff37",
      "dafc56fb4f5e13ddd3e82547874e154c5c61ac556e76e9e9766b5d7ccbc1e1be"
    ),
    (
      "sponsorship",
      sponsorship,
      "aiUKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEPwq",
      ByteStr.empty,
      "dafc56fb4f5e13ddd3e82547874e154c5c61ac556e76e9e9766b5d7ccbc1e1be",
      "d9eab5091d57c18c38e0a8702e7cbe6f133e109281f2ef0f2bc88686b458f31f"
    ),
    (
      "alias",
      alias,
      "SiYKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgh3YXZlc2V2bw==",
      ByteStr.empty,
      "d9eab5091d57c18c38e0a8702e7cbe6f133e109281f2ef0f2bc88686b458f31f",
      "eaa251c161cfe875932275ce6ff8873cd169099e021f09245f4069ccd58d6669"
    ),
    (
      "order fill",
      volumeAndFee,
      "UisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPUisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAP",
      ByteStr.empty,
      "eaa251c161cfe875932275ce6ff8873cd169099e021f09245f4069ccd58d6669",
      "de22575b5c2ef7de6388c0ea96e6d0f172802f4c8e33684473c91af65866b1d4"
    ),
    (
      "new asset",
      newAsset,
      "KkYKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBniABKkYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEiDcYGFqY9MotHTpDpskoycN/Mt62bZfPxIC4fpU0ZTBnhgIMi8KIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEAEaCQT/////////9jIlCiBeZp+CCmfQiG2HR4zwQ6KseFzHr1XcMc4QdTlL01CiWxoBAQ==",
      ByteStr.empty,
      "de22575b5c2ef7de6388c0ea96e6d0f172802f4c8e33684473c91af65866b1d4",
      "5f09358e944a386ad12b4f6e22c79a5c614967f6da40465e30d878e9b58e75e2"
    ),
    (
      "reissued asset",
      reissuedAsset,
      "MigKIDhvjT3TTlJ+v4Ni205vcYc1m9WWgnQPFovjmJI1H62yGgQ7msoA",
      ByteStr.empty,
      "5f09358e944a386ad12b4f6e22c79a5c614967f6da40465e30d878e9b58e75e2",
      "6d5e0f4e2a4b650541b66711bbc687f51fea7bc3aa35b43642e21ab3dd064743"
    ),
    (
      "renamed asset",
      renamedAsset,
      "OkMKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEgduZXduYW1lGhZzb21lIGZhbmN5IGRlc2NyaXB0aW9u",
      ByteStr.empty,
      "6d5e0f4e2a4b650541b66711bbc687f51fea7bc3aa35b43642e21ab3dd064743",
      "885ac4b03397e63cdc1a2e3fe60d2aae0d4701e5cfb8c19ca80feb912a028a48"
    ),
    (
      "failed transaction",
      failedTransaction,
      "CiQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgYQ4PHE1wlwAQ==",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef20))),
      "885ac4b03397e63cdc1a2e3fe60d2aae0d4701e5cfb8c19ca80feb912a028a48",
      "4185fb099c6dd4f483d4488045cc0912f02b9c292128b90142367af680ce2a32"
    ),
    (
      "elided transaction",
      elidedTransaction,
      "cAI=",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef40))),
      "4185fb099c6dd4f483d4488045cc0912f02b9c292128b90142367af680ce2a32",
      "7a15507d73ff9f98c3c777e687e23a4c8b33d02212203be73f0518403e91d431"
    ),
    (
      "all together",
      all,
      "CkMKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EiUKIF5mn4IKZ9CIbYdHjPBDoqx4XMevVdwxzhB1OUvTUKJbEJBOCkQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEiYKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEKCcAQokChoBVGD9UO8g3kVxIH37nIi+fBwviiLHCtiPtRIGEICU69wDCiQKGgFUQsXJY3P1D9gTUGBPHBTypsklatr9GbAqEgYQgKjWuQcSIgoaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7UYgJri4RASIgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoQgK7NvhQSIgoaAVQwI8uotbzVfYC2BqPYrAX1CRomrjsJ6/0YgKjWuQcSHAoaAVRhIl3y/Mj2ursZ0i4PLrkkxzzOLj3sT3waZgoguIIzLIWCBbxl3Ysa38C0yvtZan6R9ZvOU33eldmrOo0SIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoGhoBVELFyWNz9Q/YE1BgTxwU8qbJJWra/RmwKiCA8ouoCSIiCiC4gjMshYIFvGXdixrfwLTK+1lqfpH1m85Tfd6V2as6jSpGCiBeZp+CCmfQiG2HR4zwQ6KseFzHr1XcMc4QdTlL01CiWxIg3GBhamPTKLR06Q6bJKMnDfzLetm2Xz8SAuH6VNGUwZ4gASpGCiB4ncARI9U2D3CCr9S0ari/6LUWC7+1JsBD5fx4Ok+JThIg3GBhamPTKLR06Q6bJKMnDfzLetm2Xz8SAuH6VNGUwZ4YCDIvCiB4ncARI9U2D3CCr9S0ari/6LUWC7+1JsBD5fx4Ok+JThABGgkE//////////YyJQogXmafggpn0Ihth0eM8EOirHhcx69V3DHOEHU5S9NQolsaAQEyKAogOG+NPdNOUn6/g2LbTm9xhzWb1ZaCdA8Wi+OYkjUfrbIaBDuaygA6QwogeJ3AESPVNg9wgq/UtGq4v+i1Fgu/tSbAQ+X8eDpPiU4SB25ld25hbWUaFnNvbWUgZmFuY3kgZGVzY3JpcHRpb25KJgoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCHdhdmVzZXZvUisKIMkknO8yHpMUT/XKkkdlrbYCG0Dt+qvVgphfgtRbyRDMEICU69wDGNAPUisKIJZ9YwvJObbWItHAD2zhbaFOTFx2zQ4p0Xbo81GXHKeEEICU69wDGNAPWi4KIFDHWa9Cd6VU8M20LLFHzbBTveERf1sEOw19SUS40GBoEgcGAQaw0U/PGPoBYloKGgFUYP1Q7yDeRXEgffuciL58HC+KIscK2I+1EgUKA2ZvbxISCgNiYXJqC1N0cmluZ1ZhbHVlEiEKA2JhemIaAVRg/VDvIN5FcSB9+5yIvnwcL4oixwrYj7ViLwoaAVRCxcljc/UP2BNQYE8cFPKmySVq2v0ZsCoSCAoDZm9vULAJEgcKA2JhclgBaiUKIHidwBEj1TYPcIKv1LRquL/otRYLv7UmwEPl/Hg6T4lOEPwqcAE=",
      ByteStr(fastHash(Ints.toByteArray(0xaabbef50))),
      "7a15507d73ff9f98c3c777e687e23a4c8b33d02212203be73f0518403e91d431",
      "6502773294f32cc1702d374ffc1e67ee278cd63c5f00432f80f64a689fcb17f9"
    )
  )

  property("correctly create transaction state snapshot hash from snapshot") {
    forAll(testData) { case (clue, pbSnapshot, b64str, txId, prev, expectedResult) =>
      withClue(clue) {
        TSS.parseFrom(Base64.decode(b64str)) shouldEqual pbSnapshot

        val (snapshot, meta) = PBSnapshots.fromProtobuf(pbSnapshot, txId, 10)
        val raw = Hex.toHexString(
          TxStateSnapshotHashBuilder
            .createHashFromSnapshot(snapshot, Some(TxStateSnapshotHashBuilder.TxStatusInfo(txId, meta)))
            .createHash(ByteStr(Hex.decodeStrict(prev)))
            .arr
        )
        PBSnapshots.toProtobuf(snapshot, meta) shouldEqual pbSnapshot
        raw shouldEqual expectedResult
      }
    }
  }
}
