package com.wavesplatform.lang.v1.traits.domain

import scodec.bits.ByteVector

case class Ord(id: ByteVector,
               sender: Recipient.Address,
               senderPublicKey: ByteVector,
               matcherPublicKey: ByteVector,
               assetPair: APair,
               orderType: OrdType,
               price: Long,
               amount: Long,
               timestamp: Long,
               expiration: Long,
               matcherFee: Long,
               signature: ByteVector)
