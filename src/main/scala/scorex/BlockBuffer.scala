package scorex

import java.util.HashMap
import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.TimeUnit

import scorex.block.Block
import settings.Settings
import network.{ConnectedPeer}
import network.message.{GetBlockMessage, BlockMessage}

import scala.util.{Random, Success}

class BlockBuffer(signatures:List[Array[Byte]], peer:ConnectedPeer) extends Thread {
	private val BUFFER_SIZE = 20

	private var counter = 0
	private var error = false
	private val blocks = new HashMap[Array[Byte], BlockingQueue[Block]]()
	
	private var isRunning = true

	this.start()

	
	override def run() {
		while(isRunning) {

			(0 to Math.min(signatures.size-1, counter + BUFFER_SIZE - 1)).foreach{i =>
				val signature = signatures(i)

				//CHECK IF WE HAVE ALREADY LOADED THIS BLOCK
				if(!blocks.containsKey(signature)) loadBlock(signature)
			}

			Thread.sleep(10)
		}
	}
	
	private def loadBlock(signature:Array[Byte]){
		//CREATE QUEUE
		val blockingQueue = new ArrayBlockingQueue[Block](1)
		blocks.put(signature, blockingQueue)
		
		//LOAD BLOCK IN THREAD
		new Thread(){
			override def run(){
				//CREATE MESSAGE
				val message = GetBlockMessage(signature, mbId = Some(Random.nextInt(1000000) + 1))
				
				//SEND MESSAGE TO PEER
				peer.getResponse(message) match{
					case Success(BlockMessage(_, block, _, _)) if block.isSignatureValid() => blockingQueue.add(block)
					case _ => error = true
				}
			}
		}.start()
	}
	
	def getBlock(signature:Array[Byte]) = {
		if(error) throw new Exception("Block buffer error")

		this.counter = this.signatures.indexOf(signature)

		if(!blocks.containsKey(signature)) loadBlock(signature)

		//GET BLOCK
		blocks.get(signature).poll(Settings.getConnectionTimeout, TimeUnit.MILLISECONDS)
	}
	
	def stopThread(){
			isRunning = false
			join()
	}
}
