package com.g2forge.reassert.core.api.module;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.cache.ICache;
import com.g2forge.reassert.cache.LocalCache;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.licenseparser.CompositeLicenseParser;
import com.g2forge.reassert.core.api.licenseparser.ILicenseParser;
import com.g2forge.reassert.core.api.scanner.CompositeScanner;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.scanner.LicenseFileScanner;
import com.g2forge.reassert.core.api.system.ISystem;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter(AccessLevel.PROTECTED)
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

	public static ContextBuilder builder() {
		return new ContextBuilder();
	}

	protected ILicenseParser licenseParser;

	protected IScanner scanner;

	protected Collection<ISystem<?>> systems;

	protected Collection<IDescriber<?>> describers;

	protected final ICache cache = new LocalCache();

	public Context(IModule... modules) {
		this(HCollection.asList(modules));
	}

	public Context(Iterable<? extends IModule> modules) {
		final IModule.Loaded.LoadedBuilder builder = IModule.Loaded.builder().scanner(new LicenseFileScanner(this));
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
}