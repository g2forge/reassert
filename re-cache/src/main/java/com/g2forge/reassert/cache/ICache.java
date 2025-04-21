package com.g2forge.reassert.cache;

import com.g2forge.alexandria.java.function.IFunction1;

/**
 * An on disk cache, capable of creating multiple cache areas. Each cache area can have it's own key and value types.
 */
public interface ICache {
	public <K, V> IFunction1<? super K, ? extends V> createArea(ICacheAreaDescriptor<K, V> areaDescriptor);
}
