package com.g2forge.reassert.cache.store;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * A cache store for objects, which uses java object serialization. This is particularly suited to storing exceptions on disk, and is often used for
 * {@link com.g2forge.reassert.cache.ICacheAreaDescriptor#getExceptionConverter()}.
 * 
 * @param <T>
 *            The type of the values being stored.
 */
@Getter
@RequiredArgsConstructor
public class ObjectSerializationCacheStore<T> implements ICacheStore<T> {
	/** The type of the values being stored. */
	protected final ITypeRef<T> type;

	@Override
	public T load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException(String.format("\"%1$s\" is not absolute", path));
		try (ObjectInputStream objectInputStream = new ObjectInputStream(Files.newInputStream(path))) {
			return type.cast(objectInputStream.readObject());
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("Failed to load " + getType() + " from " + path, e);
		} catch (IOException e) {
			throw new RuntimeIOException("Failed to load " + getType() + " from " + path, e);
		}
	}

	@Override
	public T store(Path path, T value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException(String.format("\"%1$s\" is not absolute", path));
		try {
			Files.createDirectories(path.getParent());
			try (ObjectOutputStream objectOutputStream = new ObjectOutputStream(Files.newOutputStream(path))) {
				objectOutputStream.writeObject(value);
				objectOutputStream.flush();
			}
		} catch (IOException e) {
			throw new RuntimeIOException("Failed to store " + value + " to " + path, e);
		}
		return value;
	}
}
