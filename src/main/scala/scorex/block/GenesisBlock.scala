package scorex.block

import java.math.BigDecimal
import scorex.account.Account
import scorex.account.PublicKeyAccount
import scorex.crypto.Crypto
import scorex.transaction.GenesisTransaction
import scorex.transaction.Transaction

import com.google.common.primitives.{Ints, Bytes, Longs}


import database.DBSet

object GenesisBlockParams{
	val genesisVersion = 1
	val genesisReference = Array[Byte](1,1,1,1,1,1,1,1)
	val genesisTimestamp = System.currentTimeMillis - 1000*60*60
	val generatingBalance = 10000000
	val genesisGenerator = new PublicKeyAccount(Array[Byte](1,1,1,1,1,1,1,1))
	
	val ipoMembers = List(
		"2UyntBprhFgZPJ1tCtKBAwryiSnDSk9Xmh8",
		"Y2BXLjiAhPUMSo8iBbDEhv81VwKnytTXsH",
		"a8zcZPAj4HJjNLCmfRPVxurcB4REj8YNse",
		"hWUV4cjcGPgKjaNuRWAYyFMDZKadSPuwfP",
		"2etPX8BRivVTqr3vBvaCBeebhhGipbuzBNW",
		"dNKdbrqeykhxsnUpLjFTDHtTWHquiCcBGe",
		"5MkGmznxmA1Jm2F5KtxYVaf2Bfa6sy2XS1",
		"2Cqn5vN5iv7jDMehTiXTv3SGpxrCDAkAnBT",
		"2ihjht1NWTv2T8nKDMzx2RMmp7ZDEchXJus",
		"2kx3DyWJpYYfLErWpRMLHwkL1ZGyKHAPNKr"
	)

	val genesisTransactions = ipoMembers.map{addr =>
		new GenesisTransaction(new Account(addr), new BigDecimal("1000000000").setScale(8), genesisTimestamp)
	}

	lazy val generatorSignature = {
		val versionBytes = Ints.toByteArray(genesisVersion)
		val referenceBytes = Bytes.ensureCapacity(genesisReference, 64, 0)
		val generatingBalanceBytes = Longs.toByteArray(generatingBalance)
		val generatorBytes = Bytes.ensureCapacity(genesisGenerator.getPublicKey, 32, 0)

		val data = Bytes.concat(versionBytes, referenceBytes, generatingBalanceBytes, generatorBytes)
		//DIGEST
		val digest = Crypto.sha256(data)
		Bytes.concat(digest, digest)
	}
}

object GenesisBlock extends Block(version = GenesisBlockParams.genesisVersion, 
	reference = GenesisBlockParams.genesisReference,
  timestamp = GenesisBlockParams.genesisTimestamp, 
	generatingBalance = GenesisBlockParams.generatingBalance, 
	generator = GenesisBlockParams.genesisGenerator,
  generatorSignature = GenesisBlockParams.generatorSignature,
  transactions = GenesisBlockParams.genesisTransactions,
  transactionsSignature = GenesisBlockParams.generatorSignature){
	
	override def getParent() = null

	//SIGNATURE
	
	override def isSignatureValid() = {
		val versionBytes = Bytes.ensureCapacity(Longs.toByteArray(version), 4, 0)
		val referenceBytes = Bytes.ensureCapacity(reference, 64, 0)
		val generatingBalanceBytes = Bytes.ensureCapacity(Longs.toByteArray(generatingBalance), 8, 0)
		val generatorBytes = Bytes.ensureCapacity(generator.getPublicKey, 32, 0)

		val data = Bytes.concat(versionBytes, referenceBytes, generatingBalanceBytes, generatorBytes)
		val digest0 = Crypto.sha256(data)
		val digest = Bytes.concat(digest0, digest0)

		digest.sameElements(generatorSignature) && digest.sameElements(transactionsSignature)
	}
	
	override def isValid(db:DBSet) =
		(db.getBlockMap.getLastBlock == null) && transactions.forall(_.isValid(db) == Transaction.VALIDATE_OKE)
}
