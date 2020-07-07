package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;

public class FileCacheStore implements ICacheStore<Path> {
	@Getter
	protected final ITypeRef<Path> type = ITypeRef.of(Path.class);

	@Override
	public Path load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		if (!Files.isRegularFile(path)) throw new IllegalArgumentException();
		return path;
	}

	@Override
	public Path store(Path path, Path value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try {
			Files.createDirectories(path.resolve(".."));
			Files.move(value, path, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException exception) {
			throw new RuntimeIOException(exception);
		}
		return path;
	}
}
