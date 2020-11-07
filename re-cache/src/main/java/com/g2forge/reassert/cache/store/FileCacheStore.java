package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import com.g2forge.alexandria.java.io.RuntimeIOException;

public class FileCacheStore implements ICacheStore<Path> {
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
			Files.createDirectories(path.getParent());
			Files.move(value, path, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException exception) {
			throw new RuntimeIOException(exception);
		}
		return path;
	}
}
