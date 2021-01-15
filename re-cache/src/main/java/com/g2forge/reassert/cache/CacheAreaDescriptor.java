package com.g2forge.reassert.cache;

import java.nio.file.Path;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.cache.store.ICacheStore;
import com.g2forge.reassert.cache.store.ObjectSerializationCacheStore;

import lombok.Builder;
import lombok.Getter;
import lombok.ToString;

@Getter
@ToString
@Builder(toBuilder = true)
public class CacheAreaDescriptor<K, V> implements ICacheAreaDescriptor<K, V> {
	protected final Path name;

	protected final IFunction2<? super K, ? super Path, ? extends V> function;

	protected final IFunction1<? super K, ? extends Path> hashFunction;

	protected final ICacheStore<K> keyConverter;

	protected final ICacheStore<V> valueConverter;

	@Builder.Default
	protected final String keyName = "key";

	@Builder.Default
	protected final String valueName = "value";

	@Builder.Default
	protected final String exceptionName = "exception";

	@Builder.Default
	protected final ICacheStore<Exception> exceptionConverter = new ObjectSerializationCacheStore<>(ITypeRef.of(Exception.class));
}
