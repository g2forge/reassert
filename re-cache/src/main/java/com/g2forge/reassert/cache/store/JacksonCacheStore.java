package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class JacksonCacheStore<T> implements ICacheStore<T> {
	protected final ObjectMapper mapper;

	protected final ITypeRef<T> type;

	@Override
	public T load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try {
			return getMapper().readValue(path.toFile(), type.getErasedType());
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	@Override
	public T store(Path path, T value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try {
			Files.createDirectories(path.resolve("..").toRealPath());
			getMapper().writeValue(path.toFile(), value);
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
		return value;
	}
}
