//package com.wavesplatform.storage
//
//import com.wavesplatform.block.SignedBlockHeader
//import com.wavesplatform.blockchain.{RemoteData, TaggedData}
//import com.wavesplatform.grpc.BlockchainGrpcApi
//import com.wavesplatform.storage.persistent.PersistentCache
//
//import scala.collection.mutable
//
//class BlockHeadersStorage[TagT](blockchainApi: BlockchainGrpcApi, override val persistentCache: PersistentCache[Int, SignedBlockHeader])
//    extends Storage[Int, SignedBlockHeader, TagT] {
//  override protected val memoryCache = mutable.HashMap.empty[Int, TaggedData[RemoteData[SignedBlockHeader], TagT]]
//
//  override def getFromBlockchain(key: Int): Option[SignedBlockHeader] = blockchainApi.getBlockHeader(key)
//}
