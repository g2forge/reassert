package com.g2forge.reassert.core.api.module;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.alexandria.service.BasicServiceLoader;
import com.g2forge.alexandria.service.DefaultInstantiator;
import com.g2forge.reassert.cache.ICache;
import com.g2forge.reassert.cache.LocalCache;
import com.g2forge.reassert.core.algorithm.ReassertObjectDescriber;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.licenseparser.CompositeLicenseParser;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.api.module.config.IConfig;
import com.g2forge.reassert.core.api.scanner.CompositeScanner;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.system.ISystem;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Getter
@Setter(AccessLevel.PROTECTED)
@Slf4j
public class Context implements IContext {
	public static class ContextBuilder implements IBuilder<IContext> {
		protected final List<IModule> modules = new ArrayList<>();

		@Override
		public IContext build() {
			return new Context(modules);
		}

		public ContextBuilder module(IModule module) {
			modules.add(module);
			return this;
		}
	}

	@Getter(lazy = true)
	private static final IContext context = computeContext();

	public static ContextBuilder builder() {
		return new ContextBuilder();
	}

	protected static IContext computeContext() {
		final DefaultInstantiator<IModule> instantiator = new DefaultInstantiator<>(null, IModule.class);
		final BasicServiceLoader<IModule> loader = new BasicServiceLoader<>(null, IModule.class, null, instantiator);
		return new Context(loader.load());
	}

	protected ILicenseParser licenseParser;

	protected IScanner scanner;

	protected Collection<ISystem<?>> systems;

	protected Collection<IDescriber<?>> describers;

	protected final ICache cache = new LocalCache(Paths.get(System.getProperty("user.home")).resolve(".reassert"));

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ReassertObjectDescriber describer = new ReassertObjectDescriber(this);

	public Context(IModule... modules) {
		this(HCollection.asList(modules));
	}

	public Context(Iterable<? extends IModule> modules) {
		final IModule.Loaded.LoadedBuilder builder = IModule.Loaded.builder();
		for (IModule module : modules) {
			final IModule.Loaded loaded = module.load(this);
			if (loaded == null) continue;
			loaded.getLicenseParsers().forEach(builder::licenseParser);
			loaded.getScanners().forEach(builder::scanner);
			loaded.getSystems().forEach(builder::system);
			loaded.getSystems().stream().map(ISystem::getScanner).filter(Objects::nonNull).forEach(builder::scanner);
			loaded.getDescribers().forEach(builder::describer);
		}

		final IModule.Loaded loaded = builder.build();
		setScanner(new CompositeScanner(loaded.getScanners()));
		setLicenseParser(new CompositeLicenseParser(loaded.getLicenseParsers()));
		setSystems(loaded.getSystems());
		setDescribers(loaded.getDescribers());
	}

	@Override
	public IDescription describe(Object object) {
		return getDescriber().apply(object);
	}

	@Override
	public IConfig getConfig() {
		final ObjectMapper mapper = new ObjectMapper();
		mapper.registerModule(new ParanamerModule());
		
		final Path configRoot = Paths.get("").toAbsolutePath();
		log.warn(String.format("Loading reassert configurations from %1$s", configRoot));
		
		return new IConfig() {
			@Override
			public <T> IOptional<T> load(ITypeRef<T> type) {
				final Class<T> klass = type.getErasedType();
				final Path path = configRoot.resolve(klass.getSimpleName() + ".json");
				if (!Files.exists(path)) return NullableOptional.empty();
				try {
					return NullableOptional.of(mapper.readValue(path.toFile(), klass));
				} catch (IOException e) {
					throw new RuntimeIOException(e);
				}
			}
		};
	}
}