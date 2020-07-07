package com.g2forge.reassert.cache;

import com.g2forge.alexandria.java.function.IFunction1;

public interface ICache {
	public <K, V> IFunction1<? super K, ? extends V> createArea(ICacheAreaDescriptor<K, V> areaDescriptor);
}
