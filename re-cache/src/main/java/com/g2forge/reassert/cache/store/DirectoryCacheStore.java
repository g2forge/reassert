package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.file.HFile;

/**
 * A cache store for directories abstracted as java paths. They are moved into the cache when stored, or copied in if the move fails.
 */
public class DirectoryCacheStore implements ICacheStore<Path> {
	@Override
	public Path load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		if (!Files.isDirectory(path)) throw new IllegalArgumentException();
		return path;
	}

	@Override
	public Path store(Path path, Path value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try {
			Files.createDirectories(path.getParent());
			try {
				Files.move(value, path, StandardCopyOption.REPLACE_EXISTING);
			} catch (DirectoryNotEmptyException e0) {
				if (Files.isDirectory(path) && Files.list(path).findAny().isPresent()) throw e0;
				try {
					HFile.copy(value, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
					HFile.delete(value, true);
				} catch (RuntimeIOException e1) {
					e1.addSuppressed(e0);
					throw e1;
				}
			}
		} catch (IOException exception) {
			throw new RuntimeIOException(exception);
		}
		return path;
	}
}
