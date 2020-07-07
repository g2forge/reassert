package com.g2forge.reassert.cache.store;

import java.nio.file.Path;

import com.g2forge.alexandria.java.type.ref.ITypeRef;

public interface ICacheStore<T> {
	public ITypeRef<T> getType();

	public T load(Path path);

	public T store(Path path, T value);
}
