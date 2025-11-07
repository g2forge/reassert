package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * A cache store for objects, which uses a jackson object mapper to serialize and deserialize them.
 * 
 * @param <T> The type of the values being stored.
 */
@Getter
@RequiredArgsConstructor
public class JacksonCacheStore<T> implements ICacheStore<T> {
	/** The jackson mapper to use during serdes. */
	protected final ObjectMapper mapper;

	/** The type of the values being stored. */
	protected final ITypeRef<T> type;

	@Override
	public T load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException(String.format("\"%1$s\" is not absolute", path));
		try {
			return getMapper().readValue(path.toFile(), type.getErasedType());
		} catch (IOException e) {
			throw new RuntimeIOException("Failed to load " + getType() + " from " + path, e);
		}
	}

	@Override
	public T store(Path path, T value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException(String.format("\"%1$s\" is not absolute", path));
		try {
			Files.createDirectories(path.getParent());
			getMapper().writeValue(path.toFile(), value);
		} catch (IOException e) {
			throw new RuntimeIOException("Failed to store " + value + " to " + path, e);
		}
		return value;
	}
}
