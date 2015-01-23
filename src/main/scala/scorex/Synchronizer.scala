package scorex

import java.util.ArrayList
import java.util.logging.Logger

import network.{ConnectedPeer, Peer}
import network.message._
import scorex.block.Block
import scorex.transaction.Transaction
import database.DBSet

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.util.{Random, Success}

object Synchronizer {
	var isRunning = true
	
	def synchronize(db:DBSet, lastCommonBlock:Block, newBlocks:Seq[Block]) = {
		val orphanedTransactions = new ArrayList[Transaction]()
		
		//VERIFY ALL BLOCKS TO PREVENT ORPHANING INCORRECTLY
		val fork = db.fork()
		
		//ORPHAN BLOCK IN FORK TO VALIDATE THE NEW BLOCKS
		if(lastCommonBlock != null) {
			//GET LAST BLOCK
			var lastBlock = fork.getBlockMap. getLastBlock //todo: make immutable
			
			//ORPHAN LAST BLOCK UNTIL WE HAVE REACHED COMMON BLOCK
			while(!lastBlock.signature.sameElements(lastCommonBlock.signature))
			{
				lastBlock.orphan(fork)
				lastBlock = fork.getBlockMap.getLastBlock
			}
		}
		
		//VALIDATE THE NEW BLOCKS
		newBlocks foreach { block =>
			if(block.isValid(fork)) block.process(fork) else throw new Exception("Dishonest peer")
		}
		
		//NEW BLOCKS ARE ALL VALID SO WE CAN ORPHAN THEM FOR REAL NOW
		if(lastCommonBlock != null) {
			//GET LAST BLOCK
			var lastBlock = db.getBlockMap.getLastBlock    //todo: make immutable
			
			//ORPHAN LAST BLOCK UNTIL WE HAVE REACHED COMMON BLOCK
			while(!lastBlock.signature.sameElements(lastCommonBlock.signature))
			{
				//ADD ORPHANED TRANSACTIONS
				orphanedTransactions.addAll(lastBlock.transactions)
				
				lastBlock.orphan(db)
				lastBlock = db.getBlockMap.getLastBlock
			}
		}
		
		//PROCESS THE NEW BLOCKS
		newBlocks.foreach(block => process(block))

		orphanedTransactions
	}
	
	def synchronize(peer:ConnectedPeer) {
		Logger.getGlobal.info("Synchronizing: " + peer.address.getHostAddress + " - " + peer.getPing)
		
		//FIND LAST COMMON BLOCK
		val common = findLastCommonBlock(peer)
				
		//CHECK COMMON BLOCK EXISTS
		if(common.signature.sameElements(DBSet.getInstance().getBlockMap.getLastBlockSignature)){
			//GET NEXT 500 SIGNATURES
			val signatures = this.getBlockSignatures(common, BlockChain.MAX_SIGNATURES, peer)
			
			//CREATE BLOCK BUFFER
			val blockBuffer = new BlockBuffer(signatures, peer)
			
			//GET AND PROCESS BLOCK BY BLOCK
			signatures.foreach {signature =>
				//GET BLOCK
				val block = blockBuffer.getBlock(signature)
				
				//PROCESS BLOCK
				if(!process(block)) throw new Exception("Dishonest peer")
			}
			
			//STOP BLOCKBUFFER
			blockBuffer.stopThread()
		} else {
			//GET SIGNATURES FROM COMMON HEIGHT UNTIL CURRENT HEIGHT
			val signatures = getBlockSignatures(common, DBSet.getInstance().getBlockMap.getLastBlock.getHeight - common.getHeight, peer)
			
			//GET THE BLOCKS FROM SIGNATURES
			val blocks = getBlocks(signatures, peer)
							
			//SYNCHRONIZE BLOCKS
			val orphanedTransactions = synchronize(DBSet.getInstance(), common, blocks)
			
			//SEND ORPHANED TRANSACTIONS TO PEER
			orphanedTransactions.foreach(tx => peer.sendMessage(new TransactionMessage(tx)))
		}
	}
	
	private def getBlockSignatures(start:Block, amount:Int, peer:ConnectedPeer):List[Array[Byte]] = {
		@tailrec
		def addNextHeaders(headers:List[Array[Byte]]):List[Array[Byte]] = {
			val nextHeaders = getBlockSignatures(headers.last, peer)
			val newHeaders = headers ++ nextHeaders
			headers.size < amount && nextHeaders.nonEmpty match {
				case true => addNextHeaders(newHeaders)
				case false => newHeaders
			}
		}

		//ASK NEXT 500 HEADERS SINCE START
		val headers = this.getBlockSignatures(start.signature, peer).toList
		if(headers.nonEmpty && headers.size < amount) addNextHeaders(headers) else headers
	}
	
	private def getBlockSignatures(header:Array[Byte], peer:ConnectedPeer):Seq[Array[Byte]] = {
		///CREATE MESSAGE
		val message = GetSignaturesMessage(header, mbId = Some(Random.nextInt(1000000) + 1))

		peer.getResponse(message) match{
			case Success(SignaturesMessage(signatures, _, _)) => signatures
			case _ => Logger.getGlobal.info("Wrong message or failure instead of signatures"); Seq()
		}
	}
	
	private def findLastCommonBlock(peer:ConnectedPeer) = {
		var block = DBSet.getInstance().getBlockMap.getLastBlock
		
		//GET HEADERS UNTIL COMMON BLOCK IS FOUND OR ALL BLOCKS HAVE BEEN CHECKED
		var headers = getBlockSignatures(block.signature, peer)
		while(headers.isEmpty && block.getHeight > 1){
			//GO 500 BLOCKS BACK
			block = (0 to BlockChain.MAX_SIGNATURES).foldLeft(block){case (bl,_) =>
					if(bl.getHeight >1) bl.getParent else bl
			}

			headers = getBlockSignatures(block.signature, peer)
		}
		
		//CHECK IF NO HEADERS FOUND EVEN AFTER CHECKING WITH THE GENESISBLOCK
		if (headers.isEmpty) throw new Exception("Dishonest peer")

		
		//FIND LAST COMMON BLOCK IN HEADERS
		headers.reverseIterator.find{header =>
			DBSet.getInstance().getBlockMap.contains(header)
		} match {
			case Some(commonBlockHeader) => DBSet.getInstance().getBlockMap.get(commonBlockHeader)
			case None => block
		}
	}

	private def getBlocks(signatures:Seq[Array[Byte]], peer:ConnectedPeer) = signatures.map(getBlock(peer))
	
	private def getBlock(peer:ConnectedPeer)(signature:Array[Byte]) = {
		//CREATE MESSAGE
		val message = GetBlockMessage(signature, mbId = Some(Random.nextInt(1000000) + 1))
		
		//SEND MESSAGE TO PEER
		peer.getResponse(message) match{
			case Success(BlockMessage(_, block, _, _)) if block.isSignatureValid() => block
			case _ => throw new Exception("Can't get new block")
		}
	}
	
	
	//SYNCHRONIZED DO NOT PROCCESS A BLOCK AT THE SAME TIME
	def process(block:Block):Boolean = this.synchronized{
		//CHECK IF WE ARE STILL PROCESSING BLOCKS
		if(isRunning && block != null){
			//SYNCHRONIZED MIGHT HAVE BEEN PROCESSING PREVIOUS BLOCK
			if(block.isValid()) {
				//PROCESS
				DBSet.getInstance().getBlockMap.setProcessing(true)
				block.process()
				DBSet.getInstance().getBlockMap.setProcessing(false)
				true
			}else false
		}else false
	}

	def stop() {
		isRunning = false
		process(null)
	}
}