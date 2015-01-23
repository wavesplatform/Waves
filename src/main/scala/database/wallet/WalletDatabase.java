package database.wallet;

import java.io.File;

import org.mapdb.Atomic.Var;
import org.mapdb.DB;
import org.mapdb.DBMaker;

import database.IDB;
import scorex.account.Account;
import settings.Settings;

public class WalletDatabase implements IDB
{
	private static final File WALLET_FILE = new File(Settings.getWalletDir(), "wallet.dat");
	
	private static final String VERSION = "version";
	private static final String LAST_BLOCK = "lastBlock";
	
	private DB database;	
	private AccountMap accountMap;
	private TransactionMap transactionMap;
	private BlockMap blockMap;

	public static boolean exists()
	{
		return WALLET_FILE.exists();
	}
	
	public WalletDatabase()
	{
		//OPEN WALLET
		WALLET_FILE.getParentFile().mkdirs();
		
	    this.database = DBMaker.newFileDB(WALLET_FILE)
	    		.closeOnJvmShutdown()
	    		.cacheSize(2048)
	    		.checksumEnable()
	    		.mmapFileEnableIfSupported()
	            .make();
	    
	    this.accountMap = new AccountMap(this, this.database);
	    this.transactionMap = new TransactionMap(this, this.database);
	    this.blockMap = new BlockMap(this, this.database);
	}
	
	public void setVersion(int version)
	{
		this.database.getAtomicInteger(VERSION).set(version);
	}
	
	public int getVersion()
	{
		return this.database.getAtomicInteger(VERSION).intValue();
	}
	
	public void setLastBlockSignature(byte[] signature)
	{
		Var<byte[]> atomic = this.database.getAtomicVar(LAST_BLOCK);
		atomic.set(signature);
	}
	
	public byte[] getLastBlockSignature()
	{
		Var<byte[]> atomic = this.database.getAtomicVar(LAST_BLOCK);
		return atomic.get();
	}
	
	public AccountMap getAccountMap()
	{
		return this.accountMap;
	}
	
	public TransactionMap getTransactionMap()
	{
		return this.transactionMap;
	}
	
	public BlockMap getBlockMap()
	{
		return this.blockMap;
	}

	
	public void delete(Account account)
	{
		this.accountMap.delete(account);
		this.blockMap.delete(account);
		this.transactionMap.delete(account);
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

