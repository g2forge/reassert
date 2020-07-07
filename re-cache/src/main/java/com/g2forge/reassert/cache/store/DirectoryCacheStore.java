package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.file.HFile;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;

public class DirectoryCacheStore implements ICacheStore<Path> {
	@Getter
	protected final ITypeRef<Path> type = ITypeRef.of(Path.class);

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
			Files.createDirectories(path.resolve(".."));
			try {
				Files.move(value, path, StandardCopyOption.REPLACE_EXISTING);
			} catch (DirectoryNotEmptyException e0) {
				if (Files.isDirectory(path) && Files.list(path).findAny().isPresent()) throw e0;
				try {
					HFile.copy(value, path, true, IFunction1.create(true));
					HFile.delete(value, true);
				} catch (IOException e1) {
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
