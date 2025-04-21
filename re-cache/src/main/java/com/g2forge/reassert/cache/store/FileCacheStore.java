package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import com.g2forge.alexandria.java.io.RuntimeIOException;

/**
 * A cache store for files abstracted as java paths. They are moved into the cache when stored.
 */
public class FileCacheStore implements ICacheStore<Path> {
	@Override
	public Path load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException(String.format("\"%1$s\" is not absolute", path));
		if (!Files.isRegularFile(path)) throw new IllegalArgumentException(String.format("\"%1$s\" is not a regular file", path));
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
