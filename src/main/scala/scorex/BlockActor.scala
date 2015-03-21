package scorex

import java.util.logging.Logger

import akka.actor.Actor
import scorex.block.Block
import scorex.database.{PrunableBlockchainStorage, UnconfirmedTransactionsDatabaseImpl}
import scorex.network.message._
import scorex.network.{ConnectedPeer, Network}
import settings.Settings

import scala.annotation.tailrec
import scala.util.{Random, Success}


case class Synchronize(peer: ConnectedPeer)

case class NewBlock(block: Block, senderPeerOpt: Option[ConnectedPeer])

case class BlocksDownload(signatures: List[Array[Byte]], peer: ConnectedPeer)

class BlockActor extends Actor {

  def receive = {

    case Synchronize(peer) =>
      Logger.getGlobal.info("Synchronizing: " + peer.address.getHostAddress + " - " + peer.getPing)
      val common = findLastCommonBlock(peer)

      if (common.signature.sameElements(PrunableBlockchainStorage.lastBlock.signature)) {
        //GET NEXT 500 SIGNATURES
        val signatures = getBlockSignatures(common, Settings.MaxBlocksChunks, peer)

        self ! BlocksDownload(signatures, peer)
      } else {
        //GET SIGNATURES FROM COMMON HEIGHT UNTIL CURRENT HEIGHT
        val signatures = getBlockSignatures(common, PrunableBlockchainStorage.lastBlock.height().get - common.height().get, peer)

        //GET THE BLOCKS FROM SIGNATURES
        val blocks = getBlocks(signatures, peer)

        //SYNCHRONIZE BLOCKS
        synchronize(peer, Some(common), blocks)

        UnconfirmedTransactionsDatabaseImpl.getAll().foreach {
          tx => peer.sendMessage(new TransactionMessage(tx))
        }
      }


    case BlocksDownload(signatures, peer) =>
      val headSignatureOpt = signatures.headOption
      headSignatureOpt.map { signature =>
        val message = GetBlockMessage(signature, mbId = Some(Random.nextInt(1000000) + 1))

        peer.getResponse(message).map { case BlockMessage(_, block, _, _) =>
          if (block.isSignatureValid) {
            PrunableBlockchainStorage.appendBlock(block)
            self ! BlocksDownload(signatures.tail, peer)
          } else {
            Logger.getGlobal.warning(s"Dishonest peer $peer")
          }
        }
      }

    case NewBlock(block, senderPeerOpt) =>
      if (Block.isNewBlockValid(block)) {
        val filterList = senderPeerOpt match {
          case Some(senderPeer) =>
            Logger.getGlobal.info("received new valid block")
            List(senderPeer)
          case None =>
            Logger.getGlobal.info(s"Block generated $block (height: ${PrunableBlockchainStorage.height()}) " +
              s"with ${block.transactions.size} transactions inside, going to broadcast it")
            List[ConnectedPeer]()
        }

        block.process()
        PrunableBlockchainStorage.appendBlock(block)
        val height = PrunableBlockchainStorage.height()
        Network.broadcast(BlockMessage(height, block, senderPeerOpt), filterList)
      }

  }

  def synchronize(peer: ConnectedPeer, lastCommonBlockOpt: Option[Block], newBlocks: Seq[Block]) {
    lastCommonBlockOpt.map { lastCommonBlock =>
      var lastBlock = PrunableBlockchainStorage.lastBlock

      //ORPHAN LAST BLOCK UNTIL WE HAVE REACHED COMMON BLOCK
      while (!lastBlock.signature.sameElements(lastCommonBlock.signature)) {
        PrunableBlockchainStorage.discardBlock()
        lastBlock = PrunableBlockchainStorage.lastBlock
      }
    }

    //VALIDATE THE NEW BLOCKS
    newBlocks foreach { block =>
      if (block.isValid()) block.process()
      else {
        Network.onError(peer)
        throw new Exception("Dishonest peer")
      }
    }

    //NEW BLOCKS ARE ALL VALID SO WE CAN ORPHAN THEM FOR REAL NOW
    lastCommonBlockOpt.map { lastCommonBlock =>
      var lastBlock = PrunableBlockchainStorage.lastBlock

      //ORPHAN LAST BLOCK UNTIL WE HAVE REACHED COMMON BLOCK
      while (!lastBlock.signature.sameElements(lastCommonBlock.signature)) {
        //ADD ORPHANED TRANSACTIONS
        PrunableBlockchainStorage.discardBlock()
        lastBlock = PrunableBlockchainStorage.lastBlock
      }
    }

    //PROCESS THE NEW BLOCKS
    newBlocks.foreach { block =>
      block.process()
      PrunableBlockchainStorage.appendBlock(block)
    }
  }

  private def getBlockSignatures(start: Block, amount: Int, peer: ConnectedPeer): List[Array[Byte]] = {
    @tailrec
    def addNextHeaders(headers: List[Array[Byte]]): List[Array[Byte]] = {
      val nextHeaders = getBlockSignatures(headers.last, peer)
      val newHeaders = headers ++ nextHeaders
      headers.size < amount && nextHeaders.nonEmpty match {
        case true => addNextHeaders(newHeaders)
        case false => newHeaders
      }
    }

    //ASK NEXT 500 HEADERS SINCE START
    val headers = this.getBlockSignatures(start.signature, peer).toList
    if (headers.nonEmpty && headers.size < amount) addNextHeaders(headers) else headers
  }

  private def getBlocks(signatures: Seq[Array[Byte]], peer: ConnectedPeer) = signatures.map(getBlock(peer))

  private def getBlock(peer: ConnectedPeer)(signature: Array[Byte]) = {
    //CREATE MESSAGE
    val message = GetBlockMessage(signature, mbId = Some(Random.nextInt(1000000) + 1))

    //SEND MESSAGE TO PEER
    peer.getResponse(message) match {
      case Success(BlockMessage(_, block, _, _)) if block.isSignatureValid => block
      case _ => throw new Exception("Can't get new block")
    }
  }

  private def findLastCommonBlock(peer: ConnectedPeer): Block = {
    var block = PrunableBlockchainStorage.lastBlock

    //GET HEADERS UNTIL COMMON BLOCK IS FOUND OR ALL BLOCKS HAVE BEEN CHECKED
    var headers = getBlockSignatures(block.signature, peer)
    while (headers.isEmpty && block.height().get > 1) {
      //GO 500 BLOCKS BACK
      block = (0 to Settings.MaxBlocksChunks).foldLeft(block) { case (bl, _) =>
        bl.parent().getOrElse(bl)
      }

      headers = getBlockSignatures(block.signature, peer)
    }

    //CHECK IF NO HEADERS FOUND EVEN AFTER CHECKING WITH THE GENESISBLOCK
    if (headers.isEmpty) throw new Exception("Dishonest peer")


    //FIND LAST COMMON BLOCK IN HEADERS
    headers.reverseIterator.find { header =>
      PrunableBlockchainStorage.blockByHeader(header).isDefined
    } match {
      case Some(commonBlockHeader) => PrunableBlockchainStorage.blockByHeader(commonBlockHeader).get
      case None => block
    }
  }

  private def getBlockSignatures(header: Array[Byte], peer: ConnectedPeer): Seq[Array[Byte]] = {
    ///CREATE MESSAGE
    val message = GetSignaturesMessage(header, mbId = Some(Random.nextInt(1000000) + 1))

    peer.getResponse(message) match {
      case Success(SignaturesMessage(signatures, _, _)) => signatures
      case _ => Logger.getGlobal.info("Wrong message or failure instead of signatures"); Seq()
    }
  }
}
