package utils;

import java.io.Serializable;
import java.util.Comparator;

public class ReverseComparator<T> implements Comparator<T>, Serializable
{
	private static final long serialVersionUID = 1294003434356818129L;
	private Comparator<T> delegate;
	
	public ReverseComparator(Comparator<T> delegate)
	{
		this.delegate = delegate;
	}

	public int compare(T a, T b) 
	{
		return this.delegate.compare(b,a);
	}
}