package com.g2forge.reassert.cache;

import com.g2forge.alexandria.java.function.IFunction1;

/**
 * An on disk cache, capable of creating multiple cache areas. Each cache area can have it's own key and value types.
 */
public interface ICache {
	/**
	 * Create a cache area from it's descriptor.
	 * 
	 * @param <K>
	 *            The type of keys for this cache area.
	 * @param <V>
	 *            The type of values for this cache area.
	 * @param areaDescriptor
	 *            The cache area descriptor.
	 * @return A cache area. A function from keys to values.
	 */
	public <K, V> IFunction1<? super K, ? extends V> createArea(ICacheAreaDescriptor<K, V> areaDescriptor);
}
