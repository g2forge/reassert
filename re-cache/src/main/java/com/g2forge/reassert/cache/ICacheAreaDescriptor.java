package com.g2forge.reassert.cache;

import java.nio.file.Path;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.cache.store.ICacheStore;

public interface ICacheAreaDescriptor<K, V> {
	public IFunction2<? super K, ? super Path, ? extends V> getFunction();

	public IFunction1<? super K, ? extends Path> getHashFunction();

	public ICacheStore<K> getKeyConverter();

	public String getKeyName();

	public Path getName();

	public ICacheStore<V> getValueConverter();
	
	public ICacheStore<Exception> getExceptionConverter();

	public String getValueName();

	public String getExceptionName();
}
