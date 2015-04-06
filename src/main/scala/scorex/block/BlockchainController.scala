package scorex.block

import java.net.InetSocketAddress
import java.util.logging.Logger
import akka.actor.{Props, ActorRef, Actor}
import scorex.database.PrunableBlockchainStorage
import scorex.network.NetworkController
import scorex.network.message.{GetSignaturesMessage, BlockMessage}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


case class Synchronize(peer: InetSocketAddress)

case class NewBlock(block: Block, sender:Option[InetSocketAddress])

case class BlocksDownload(signatures: List[Array[Byte]], peer: InetSocketAddress)

case class BlockchainController(networkController: ActorRef) extends Actor {

  import BlockchainController._

  private var status = Status.Offline

  private val blockGenerator = context.actorOf(Props[BlockGenerator])

  override def preStart() = {
    context.system.scheduler.schedule(1.second, 2.seconds)(self ! CheckState)
    context.system.scheduler.schedule(500.millis, 1.second)(networkController ! GetMaxChainScore)
  }

  override def receive = {
    case CheckState =>
      status match {
        case Status.Offline =>

        case Status.Syncing =>
          val msg = GetSignaturesMessage(PrunableBlockchainStorage.lastSignatures())
          networkController ! NetworkController.SendMessageToBestPeer(msg)

        case Status.Generating =>
          blockGenerator ! BlockGenerator.TryToGenerateBlock
      }

    case MaxChainScore(scoreOpt) => scoreOpt match {
      case Some(maxScore) =>
        if (maxScore > PrunableBlockchainStorage.height()) {
          status = Status.Syncing
        } else {
          status = Status.Generating
        }
      case None => status = Status.Offline
    }

    case NewBlock(block, remoteOpt) =>
      if (Block.isNewBlockValid(block)) {
        Logger.getGlobal.info(s"New block: $block")
        block.process()
        PrunableBlockchainStorage.appendBlock(block)
        val height = PrunableBlockchainStorage.height()
        val exceptOf = remoteOpt.map(r=> List(r)).getOrElse(List())
        networkController ! NetworkController.BroadcastMessage(BlockMessage(height, block), exceptOf)
      } else {
        Logger.getGlobal.warning(s"Non-valid block: $block from $remoteOpt")
      }

    case GetStatus => sender() ! Status.replicate(status)

    case a:Any => Logger.getGlobal.warning(s"BlockchainController: got something strange $a")
  }
}

object BlockchainController {

  object Status extends Enumeration {
    val Offline = Value(0)
    val Syncing = Value(1)
    val Generating = Value(2)

    def replicate(status: Status.Value) = Value(status.id)
  }

  case object CheckState

  case object GetMaxChainScore

  case class MaxChainScore(scoreOpt: Option[Int])

  case object GetStatus
}


/*
//todo: OLD CODE BELOW:
override def receive = {
case Synchronize(peer) =>
Logger.getGlobal.info("Synchronizing: " + peer.getHostName)
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

synchronize(peer, Some(common), blocks) //SYNCHRONIZE BLOCKS

UnconfirmedTransactionsDatabaseImpl.getAll().foreach {
tx => peer.sendMessage(new TransactionMessage(tx))
}
}

case BlocksDownload(signatures, peer) =>
val headSignatureOpt = signatures.headOption
headSignatureOpt.map { signature =>
val message = GetBlockMessage(signature, mbId = Some(Random.nextInt(1000000) + 1))

peer.getResponse(message).map { case BlockMessage(_, block, _) =>
if (block.isSignatureValid) {
PrunableBlockchainStorage.appendBlock(block)
self ! BlocksDownload(signatures.tail, peer)
} else {
Logger.getGlobal.warning(s"Dishonest peer $peer")
}
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


  */