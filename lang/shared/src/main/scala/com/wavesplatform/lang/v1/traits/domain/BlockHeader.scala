package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr

final case class BlockHeader(timestamp: Long,
                             version: Long,
                             reference: ByteStr,
                             generator: ByteStr,
                             signature: ByteStr,
                             baseTarget: Long,
                             generationSignature: ByteStr,
                             transactionCount: Long,
                             transactionTreeHash: ByteStr,
                             minerWavesBalancesTreeHash: ByteStr,
                             minerEffectiveBalancesTreeHash: ByteStr,
                             featureVotes: Seq[Long])
