package com.g2forge.reassert.cache;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.file.HFile;
import com.g2forge.alexandria.java.io.file.TempDirectory;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.cache.store.ICacheStore;
import com.g2forge.reassert.cache.store.JacksonCacheStore;

public class TestLocalCache {
	public static class Function implements IFunction2<Value, Path, Value> {
		protected int index;

		@Override
		public Value apply(Value value, Path path) {
			return new Value(value.getMessage(), index++);
		}
	}

	@Test
	public void test() {
		try (final TempDirectory temp = new TempDirectory()) {
			final LocalCache cache = new LocalCache(temp.get());

			final ObjectMapper mapper = new ObjectMapper();
			mapper.registerModule(new ParanamerModule());
			final ICacheStore<Value> converter = new JacksonCacheStore<>(mapper, ITypeRef.of(Value.class));

			final IFunction1<? super Value, ? extends Value> area = cache.createArea(CacheAreaDescriptor.<Value, Value>builder().name(Paths.get("area")).function(new Function()).hashFunction(ws -> Paths.get(ws.getMessage())).keyConverter(converter).valueConverter(converter).build());
			HAssert.assertEquals(new Value("Hello", 0), area.apply(new Value("Hello", 0)));
			HAssert.assertEquals(new Value("Hello", 0), area.apply(new Value("Hello", 0)));
			try {
				HFile.delete(temp.get().resolve("area"), true);
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
			HAssert.assertEquals(new Value("Hello", 1), area.apply(new Value("Hello", 0)));
		}
	}
}
