package com.g2forge.reassert.cache.store;

import java.nio.file.Path;

/**
 * A type-specific storage abstraction for cached keys and values.
 * 
 * @param <T>
 *            The type being stored.
 */
public interface ICacheStore<T> {
	/**
	 * Load a value from the specified path.
	 * 
	 * @param path
	 *            The file or directory to load.
	 * @return The loaded value.
	 */
	public T load(Path path);

	/**
	 * Store a value to the specified path.
	 * 
	 * @param path
	 *            The file or directory to store a value to. This may or may not exist.
	 * @param value
	 *            The value to store.
	 * @return The value that was stored.
	 */
	public T store(Path path, T value);
}
