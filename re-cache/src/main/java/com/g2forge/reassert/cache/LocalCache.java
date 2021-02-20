package com.g2forge.reassert.cache;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.g2forge.alexandria.java.core.error.HError;
import com.g2forge.alexandria.java.core.error.UnreachableCodeError;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
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
			final Path exceptionPath = directory.resolve(descriptor.getExceptionName());

			synchronized (descriptor) {
				if (Files.isDirectory(directory)) {
					K loadedKey = null;
					try {
						loadedKey = descriptor.getKeyConverter().load(keyPath);
					} catch (Throwable throwable) { /* If anything failed here, just recompute anyway */ }
					if (key.equals(loadedKey)) {
						if (Files.isRegularFile(exceptionPath)) {
							final Exception loaded = descriptor.getExceptionConverter().load(exceptionPath);
							HError.throwQuietly(loaded);
							throw new UnreachableCodeError(loaded);
						}
						return descriptor.getValueConverter().load(valuePath);
					}

					HFile.delete(directory, false);
				}

				boolean successfullyCreatedCacheEntry = false;
				try {
					try {
						Files.createDirectories(directory);
					} catch (IOException e) {
						throw new RuntimeIOException(e);
					}
					descriptor.getKeyConverter().store(keyPath, key);

					final IFunction2<? super K, ? super Path, ? extends V> function = descriptor.getFunction();
					final V computed;

					try {
						computed = function.apply(key, valuePath);
					} catch (Exception exception) {
						descriptor.getExceptionConverter().store(exceptionPath, exception);
						successfullyCreatedCacheEntry = true;
						throw exception;
					}

					final V retVal = descriptor.getValueConverter().store(valuePath, computed);
					successfullyCreatedCacheEntry = true;
					return retVal;
				} finally {
					if (!successfullyCreatedCacheEntry) {
						HFile.delete(directory, true);
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
