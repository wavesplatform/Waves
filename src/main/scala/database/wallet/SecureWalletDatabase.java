package database.wallet;

import java.io.File;

import org.mapdb.DB;
import org.mapdb.DBMaker;
import org.mapdb.Serializer;

import scorex.account.PrivateKeyAccount;
import settings.Settings;

public class SecureWalletDatabase 
{
	private static final File SECURE_WALLET_FILE = new File(Settings.walletDir(), "wallet.s.dat");
	
	private static final String SEED = "seed";
	private static final String NONCE = "nonce";
	
	private DB database;
	
	private AccountSeedMap accountSeedMap;
	
	public static boolean exists()
	{
		return SECURE_WALLET_FILE.exists();
	}
	
	public SecureWalletDatabase(String password)
	{
		//OPEN WALLET
		SECURE_WALLET_FILE.getParentFile().mkdirs();
				
		//DELETE TRANSACTIONS
		//File transactionFile = new File(Settings.getInstance().getWalletDir(), "wallet.s.dat.t");
		//transactionFile.delete();	
		
		this.database = DBMaker.newFileDB(SECURE_WALLET_FILE)
						.encryptionEnable(password)
			    		.closeOnJvmShutdown()
			    		.cacheSize(2048)
			    		.checksumEnable()
			    		.mmapFileEnableIfSupported()
			            .make();
			    
		this.accountSeedMap = new AccountSeedMap(this, this.database);
	}
	
	public AccountSeedMap getAccountSeedMap()
	{
		return this.accountSeedMap;
	}

	public void setSeed(byte[] seed) 
	{
		this.database.createAtomicVar(SEED, seed, Serializer.BYTE_ARRAY);
	}
	
	public byte[] getSeed()
	{
		return (byte[]) this.database.getAtomicVar(SEED).get();
	}

	public void setNonce(int nonce) 
	{
		 this.database.getAtomicInteger(NONCE).set(nonce);	
	}
	
	public int getNonce()
	{
		return this.database.getAtomicInteger(NONCE).intValue();
	}
	
	public int getAndIncrementNonce()
	{
		return this.database.getAtomicInteger(NONCE).getAndIncrement();
	}
	
	public void delete(PrivateKeyAccount account)
	{
		this.accountSeedMap.delete(account);
	}
	
	public void commit()
	{
		this.database.commit();
	}
	
	public void close() 
	{
		if(this.database != null)
		{
			if(!this.database.isClosed())
			{
				this.database.commit();
				this.database.close();
			}
		}
	}
}

