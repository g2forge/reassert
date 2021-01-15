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

@Getter
@RequiredArgsConstructor
public class ObjectSerializationCacheStore<T> implements ICacheStore<T> {
	protected final ITypeRef<T> type;

	@Override
	public T load(Path path) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try (ObjectInputStream objectInputStream = new ObjectInputStream(Files.newInputStream(path))) {
			return type.cast(objectInputStream.readObject());
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
	}

	@Override
	public T store(Path path, T value) {
		if (!path.isAbsolute()) throw new IllegalArgumentException();
		try {
			Files.createDirectories(path.getParent());
			try (ObjectOutputStream objectOutputStream = new ObjectOutputStream(Files.newOutputStream(path))) {
				objectOutputStream.writeObject(value);
				objectOutputStream.flush();
			}
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}
		return value;
	}
}
