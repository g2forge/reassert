package com.g2forge.reassert.cache.store;

import java.nio.file.Path;

public interface ICacheStore<T> {
	public T load(Path path);

	public T store(Path path, T value);
}
