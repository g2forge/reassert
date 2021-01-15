package com.g2forge.reassert.cache;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.close.ICloseable;
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
	public static class TestCacheArea implements IFunction1<Value, Value>, ICloseable {
		protected static final String AREA = "area";

		protected static final ICacheStore<Value> converter;

		static {
			final ObjectMapper mapper = new ObjectMapper();
			mapper.registerModule(new ParanamerModule());
			converter = new JacksonCacheStore<>(mapper, ITypeRef.of(Value.class));
		}

		protected final TempDirectory temp;

		protected final IFunction1<? super Value, ? extends Value> area;

		public TestCacheArea(IFunction2<Value, Path, Value> function) {
			this.temp = new TempDirectory();
			final LocalCache cache = new LocalCache(temp.get());
			this.area = cache.createArea(CacheAreaDescriptor.<Value, Value>builder().name(Paths.get(AREA)).function(function).hashFunction(ws -> Paths.get(ws.getMessage())).keyConverter(converter).valueConverter(converter).build());
		}

		@Override
		public Value apply(Value value) {
			return area.apply(value);
		}

		public void clear() {
			try {
				HFile.delete(temp.get().resolve(AREA), true);
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		@Override
		public void close() {
			temp.close();
		}

	}

	@Test
	public void exception() {
		try (final TestCacheArea area = new TestCacheArea(new IFunction2<Value, Path, Value>() {
			protected int index;

			@Override
			public Value apply(Value value, Path path) {
				throw new RuntimeException(Integer.toString(index++));
			}
		})) {
			final Value hello0 = new Value("Hello", 0);
			HAssert.assertException(RuntimeException.class, "0", () -> area.apply(hello0));
			HAssert.assertException(RuntimeException.class, "0", () -> area.apply(hello0));
			area.clear();
			HAssert.assertException(RuntimeException.class, "1", () -> area.apply(hello0));
		}
	}

	@Test
	public void value() {
		try (final TestCacheArea area = new TestCacheArea(new IFunction2<Value, Path, Value>() {
			protected int index;

			@Override
			public Value apply(Value value, Path path) {
				return new Value(value.getMessage(), index++);
			}
		})) {
			final Value hello0 = new Value("Hello", 0);
			HAssert.assertEquals(new Value("Hello", 0), area.apply(hello0));
			HAssert.assertEquals(new Value("Hello", 0), area.apply(hello0));
			area.clear();
			HAssert.assertEquals(new Value("Hello", 1), area.apply(hello0));
		}
	}
}
