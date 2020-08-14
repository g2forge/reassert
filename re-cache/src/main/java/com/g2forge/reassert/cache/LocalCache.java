package com.g2forge.reassert.cache;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.file.HFile;
import com.g2forge.alexandria.java.project.HProject;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class LocalCache implements ICache {
	@Getter
	@RequiredArgsConstructor
	protected class CacheArea<K, V> implements IFunction1<K, V> {
		protected final ICacheAreaDescriptor<K, V> descriptor;

		@Override
		public V apply(K key) {
			if (key == null) throw new IllegalArgumentException("No support for null cache keys!");

			final ICacheAreaDescriptor<K, V> descriptor = getDescriptor();
			final Path hash = descriptor.getHashFunction().apply(key);

			if (hash.isAbsolute()) throw new IllegalArgumentException();
			final Path directory = getRoot().resolve(descriptor.getName()).resolve(hash);
			final Path keyPath = directory.resolve(descriptor.getKeyName());
			final Path valuePath = directory.resolve(descriptor.getValueName());

			synchronized (descriptor) {
				if (Files.isDirectory(directory)) {
					K loadedKey = null;
					try {
						loadedKey = descriptor.getKeyConverter().load(keyPath);
					} catch (Throwable throwable) { /* If anything failed here, just recompute anyway */ }
					if (key.equals(loadedKey)) return descriptor.getValueConverter().load(valuePath);

					try {
						HFile.delete(directory, false);
					} catch (IOException e) {
						throw new RuntimeIOException(e);
					}
				}

				boolean success = false;
				try {
					try {
						Files.createDirectories(directory);
					} catch (IOException e) {
						throw new RuntimeIOException(e);
					}

					final V computed = descriptor.getFunction().apply(key, valuePath);
					descriptor.getKeyConverter().store(keyPath, key);
					final V retVal = descriptor.getValueConverter().store(valuePath, computed);
					success = true;
					return retVal;
				} finally {
					if (!success) {
						try {
							HFile.delete(directory, true);
						} catch (IOException e) {
							throw new RuntimeIOException(e);
						}
					}
				}
			}
		}
	}

	protected final Path root;

	public LocalCache() {
		this(HProject.getLocation(LocalCache.class).getProject().getRoot().resolve("../.cache"));
	}

	@Override
	public <K, V> IFunction1<? super K, ? extends V> createArea(ICacheAreaDescriptor<K, V> areaDescriptor) {
		return new CacheArea<>(areaDescriptor);
	}
}
