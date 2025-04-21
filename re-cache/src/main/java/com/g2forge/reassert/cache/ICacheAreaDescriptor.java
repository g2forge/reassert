package com.g2forge.reassert.cache;

import java.nio.file.Path;

import com.g2forge.alexandria.java.adt.name.INamed;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.reassert.cache.store.ICacheStore;

/**
 * A descriptor for a cache area. The methods on this interface allow the cache to learn how to compute values, load &amp; store keys, exceptions, and values,
 * and where to create cache entries within this area. Caches should cache exceptions when possible, and should verify that the key for any cache entry matches
 * to expected key in case of collisions in {@link #getHashFunction()}.
 * 
 * @param <K>
 *            The type of keys for this cache area.
 * @param <V>
 *            The type of values for this cache area.
 */
public interface ICacheAreaDescriptor<K, V> extends INamed<Path> {
	/**
	 * Storage abstraction for exceptions from {@link #getFunction()}. Exceptions are cached in order to improve performance. See
	 * {@link com.g2forge.reassert.cache.store.ObjectSerializationCacheStore} which is strongly recommended as being one of the few ways to serialize and
	 * deserialize exceptions in Java.
	 * 
	 * @return Storage abstraction for the cache function exceptions.
	 */
	public ICacheStore<Exception> getExceptionConverter();

	/**
	 * Get the name of the on-disk exception within the cache entry.
	 * 
	 * @return The name of the on-disk exception within the cache entry.
	 */
	public String getExceptionName();

	/**
	 * The function to cache. Accepts the key, and the on-disk path where the value should be cached. The path should only be used if this function is
	 * downloading something, or otherwise making a copy of a large file structure. The path will not exist when this function is called, but it can be created
	 * by this function. If <code>path</code> is <code>null</code> then the function should download any files to a temp directory instead. Returns the actual
	 * value.
	 * 
	 * @return The function whose results we are caching. A function from key and local path to actual value.
	 */
	public IFunction2<? super K, ? super Path, ? extends V> getFunction();

	/**
	 * Key hash function used to determine the on-disk location of the cache entry within the cache area. This function should return a (hopefully, but not
	 * necessarily always) unique path for each key, which is valid on this operating system. <code>key -> Paths.get(key.toString())</code>, or something
	 * equally simple, is acceptable if there are never OS-disallowed characters in that string.
	 * 
	 * @return The key hash function.
	 */
	public IFunction1<? super K, ? extends Path> getHashFunction();

	/**
	 * Storage abstraction for the cache keys. See implementations of {@link ICacheStore}.
	 * 
	 * @return Storage abstraction for the cache keys
	 */
	public ICacheStore<K> getKeyConverter();

	/**
	 * Get the name of the on-disk key within the cache entry.
	 * 
	 * @return The name of the on-disk key within the cache entry.
	 */
	public String getKeyName();

	/**
	 * A name for this cache area. Should be unique to whatever is being cached and stable over time. <code>Paths.get(getClass().getSimpleName())</code> is
	 * often a reasonable value.
	 */
	public Path getName();

	/**
	 * Storage abstraction for the cache values. See implementations of {@link ICacheStore}.
	 * 
	 * @return Storage abstraction for the cache values.
	 */
	public ICacheStore<V> getValueConverter();

	/**
	 * Get the name of the on-disk value within the cache entry.
	 * 
	 * @return The name of the on-disk value within the cache entry.
	 */
	public String getValueName();
}
