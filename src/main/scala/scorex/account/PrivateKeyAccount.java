package scorex.account;

import scorex.crypto.Crypto;
import utils.Pair;

public class PrivateKeyAccount extends PublicKeyAccount {

	private byte[] seed;
	private Pair<byte[], byte[]> keyPair;
	
	public PrivateKeyAccount(byte[] seed)
	{
		this.seed = seed;
		scala.Tuple2 pair = Crypto.createKeyPair(seed);
		this.keyPair = new Pair(pair._1(), pair._2());
		this.publicKey = keyPair.getB();
		this.address = Crypto.getAddress(this.publicKey);
	}
	
	public byte[] getSeed()
	{
		return this.seed;
	}
	
	public byte[] getPrivateKey() 
	{
		return this.keyPair.getA();
	}
	
	public Pair<byte[], byte[]> getKeyPair()
	{
		return this.keyPair;
	}
	
}
