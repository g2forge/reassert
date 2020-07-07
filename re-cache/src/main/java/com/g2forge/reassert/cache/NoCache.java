package com.g2forge.reassert.cache;

import com.g2forge.alexandria.java.function.IFunction1;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class NoCache implements ICache {
	@Getter
	@RequiredArgsConstructor
	protected class CacheArea<K, V> implements IFunction1<K, V> {
		protected final ICacheAreaDescriptor<K, V> descriptor;

		@Override
		public V apply(K key) {
			return descriptor.getFunction().apply(key, null);
		}
	}

	@Override
	public <K, V> IFunction1<? super K, ? extends V> createArea(ICacheAreaDescriptor<K, V> areaDescriptor) {
		return new CacheArea<>(areaDescriptor);
	}
}
